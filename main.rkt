#lang racket/base
(require (file "private/cross.rkt"))

(define current-browser (make-parameter #f))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here



(module* listener racket/base
  (require (only-in racket/tcp tcp-listen tcp-accept/enable-break))
  (provide createListener)
  (define (createListener port [hostname #f])
    (tcp-accept/enable-break (tcp-listen port 1 #f hostname))))

(module* connector racket/base
  (require (only-in racket/tcp tcp-connect/enable-break))
  (provide createConnector)
  (define (createConnector hostname port)
    (tcp-connect/enable-break hostname port)))

(module* extension #f
  (require (for-syntax racket/base)
           (only-in racket/file display-to-file)
           (only-in racket/port input-port-append)
           (only-in racket/generic define-generics))
  (provide (struct-out message) (struct-out file) (struct-out link) handleInput)

  (define-generics structure
    (structure->port structure)
    (structure-out structure))

  (struct message (name content hour minute second timezone)
    #:methods gen:structure [(define (structure-out message) (format "~a>:~a<~a:~a:~a,~a>"
                                                                     (message-name message)
                                                                     (message-content message)
                                                                     (message-hour message)
                                                                     (message-minute message)
                                                                     (message-second message)
                                                                     (message-timezone message)))
                             (define (structure->port message) (open-input-string (format "[:message:~a]" (structure-out message))))])
  (define (port->message port)
    (with-handlers ((exn:fail:contract? (lambda (exn) #f)))
      (apply message (cdr (regexp-match-peek #rx#"^[[]:message:(.*?)>:(.*?)<([0-9]*):([0-9]*):([0-9]*),(.*?)>[]]$" port)))))

  (struct file (name content port)
    #:guard (lambda (name content port type-name)
              (values name
                      content
                      (cond [(input-port? port) port]
                            [(path? content) (open-input-file content)]
                            [(or (string? content) (bytes? content)) #f]
                            [else (error (format "~a error : port field" type-name))])))
    #:methods gen:structure [(define (structure-out file)
                               (display "Download?[y/n]:")
                               (cond [(string-ci=? (read-line) "y")
                                      (with-handlers ((exn:fail:filesystem? (lambda (exn) (void))))
                                        (make-directory "file"))
                                      (display-to-file (file-content file) (build-path 'same "file" (bytes->string/utf-8 (file-name file))) #:exists 'truncate/replace)
                                      "Successful"]
                                     [else "Cancelled"]))
                             (define (structure->port file) (input-port-append
                                                             #t
                                                             (open-input-string (format "[:file:~a>:" (file-name file)))
                                                             (file-port file)
                                                             (open-input-string "]")))])
  (define (port->file port)
    (with-handlers ((exn:fail:contract? (lambda (exn) #f)))
      (apply file `(,@(cdr (regexp-match-peek #rx#"^[[]:file:(.*?)>:(.*?)[]]$" port)) #f))))

  (struct link message ()
    #:methods gen:structure
    [(define (structure-out link)
       (displayln (format "link:~a<~a:~a:~a,~a>"
                          (message-name link)
                          (message-hour link)
                          (message-minute link)
                          (message-second link)
                          (message-timezone link)))
       (display "Redirect[y/n]:")
       (cond ((string-ci=? "y" (read-line)) ((current-browser) (message-content link)) "ok")
             (else (format "~a:cancelled" (message-content link)))))
     (define (structure->port link)
       (open-input-string
        (format "[:link:~a>:~a<~a:~a:~a,~a>]"
                (message-name link)
                (message-content link)
                (message-hour link)
                (message-minute link)
                (message-second link)
                (message-timezone link))))])
  (define (port->link port)
    (with-handlers ((exn:fail:contract? (lambda (exn) #f)))
      (apply link (cdr (regexp-match-peek #rx#"^[[]:link:(.*?)>:(.*?)<([0-9]*):([0-9]*):([0-9]*),(.*?)>[]]$" port)))))

  ;;TODO

  (define-syntax (handleInput stx)
    (syntax-case stx ()
      ((_ object) #`(cond
                      ;;TCP INPUT
                      ((cond
                         ((port->file object))
                         ((port->link object))
                         ((port->message object))
                         (else #f))
                       => structure-out)
                      ;;CURRENT INPUT
                      ((cond
                         ((file? object))
                         ((link? object))
                         ((message? object))
                         (else #f))
                       (structure->port object))
                      (else object))))))

(module* parallel #f
  (require (only-in racket/place place* place-channel-get place-channel-put)
           (only-in file/gzip gzip-through-ports)
           (only-in file/gunzip gunzip-through-ports)
           (only-in racket/string string-prefix?)
           (only-in racket/path string->some-system-path)
           (only-in racket/file make-temporary-file)
           (only-in racket/date current-date)
           (only-in racket/port copy-port)
           (submod ".." extension))
  (provide runParallel)

  (define (getTime)
    (let ((date (current-date)))
      (list (date-hour date) (date-minute date) (date-second date) (date*-time-zone-name date))))
  (define (handleIO in-in out name)
    (let loop ()
      (define syn (sync in-in (read-line-evt)))
      (cond ((input-port? syn) (define port (open-output-bytes))
                               (gunzip-through-ports syn port)
                               (displayln (handleInput (open-input-bytes (get-output-bytes port))))
                               (flush-output (current-output-port)))
            ((string? syn) (gzip-through-ports
                            (handleInput
                             (cond
                               ((string-prefix? syn "file>")
                                (define path (string->some-system-path (substring syn 5) (system-type 'os)))
                                (with-handlers ((exn:fail:filesystem? (lambda (exn) (apply message name "error" (getTime)))))
                                  (file (path->string (let-values (((base name bool) (split-path path)))
                                                        name))
                                        path #f)))
                               ((string-prefix? syn "link>")
                                (apply link name (substring syn 5) (getTime)))
                               (else (apply message name syn (getTime)))))
                            out #f 0)
                           (flush-output out)))
      (loop)))
  (define (runParallel in out name)
    (let-values (((pl i in-in e) (place* #:in #f #:out #f #:err #f ch
                                         (define in (place-channel-get ch))
                                         (copy-port in (current-output-port)))))
      (place-channel-put pl in)
      (displayln "You can Chat now.")
      (handleIO in-in out name))))

(module* main #f

  (require (submod ".." listener)
           (submod ".." connector)
           (submod ".." parallel)
           (only-in browser open-url))
  (provide getMode getName getHostname getPort)


  (define (getName) (let ()
                      (display "name:")
                      (read-line)))
  (define (getMode) (let ()
                      (display "mode [Accept/Connect]:")
                      (read-line)))
  (define (getPort)
    (let ()
      (display "port-no:")
      (let work ([no (read-line)])
        (cond ((string->number no))
              (else (display "again")
                    (work (read)))))))
  (define (getHostname mode)
    (cond [(string-ci=? mode "Accept")
           (begin
             (display "hostname[optional,default to none]:")
             (define host (read-line))
             (cond [(equal? host "") #f]
                   [else host]))]
          [else (begin
                  (display "hostname:")
                  (let work ()
                    (define host (read-line))
                    (cond [(equal? host "") (work)]
                          [else host])))]))

  (begin
    (define name (getName))
    (define mode (getMode))
    (define port (getPort))
    (define hostname (getHostname mode))
    (parameterize ([current-browser open-url]
                   [current-custodian (make-custodian)])
      (break-enabled #t)
      (with-handlers ([exn:fail:network? (lambda (exn) (custodian-shutdown-all (current-custodian)))]
                      [exn:break? (lambda (exn) (custodian-shutdown-all (current-custodian)))])
        (define-values (in out)
          (cond [(string-ci=? mode "Accept") (createListener port hostname)]
                [else (createConnector hostname port)]))
        (displayln "Connect Successfully.")
        (runParallel in out name)))))