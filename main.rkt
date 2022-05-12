#lang racket/base
(require (file "private/cross.rkt"))

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
  (provide (struct-out message) (struct-out file) (struct-out link) (struct-out directory) handleInput)

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
      (apply message (cdr (regexp-try-match #rx#"^[[]:message:(.*?)>:(.*?)<([0-9]*):([0-9]*):([0-9]*),(.*?)>[]]$" port)))))

  (struct file (name content port)
    #:guard (lambda (name content port type-name)
              (values name
                      content
                      (cond [(input-port? port) port]
                            [(path? content) (open-input-file content)]
                            [(or (string? content) (bytes? content)) #f]
                            [else (error (format "~a error : port field" type-name))])))
    #:methods gen:structure [(define (structure-out file)
                               (displayln (format "file:~a" (file-name file)))
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
      (apply file `(,@(cdr (regexp-try-match #rx#"^[[]:file:(.*?)>:(.*?)[]]$" port)) #f))))

  (struct link message ()
    #:methods gen:structure
    [(define (structure-out link)
       (define url (message-content link))
       (displayln (format "link:~a<~a:~a:~a,~a>"
                          url
                          (message-hour link)
                          (message-minute link)
                          (message-second link)
                          (message-timezone link)))
       (display "Redirect[y/n]:")
       (cond ((string-ci=? "y" (read-line))
              ((dynamic-require 'browser/external 'send-url) (bytes->string/utf-8 url) #t)
              "ok")
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
      (apply link (cdr (regexp-try-match #rx#"^[[]:link:(.*?)>:(.*?)<([0-9]*):([0-9]*):([0-9]*),(.*?)>[]]$" port)))))

  (struct directory file ()
    #:methods gen:structure
    [(define (structure-out dir)
       (displayln (format "dir:~a" (file-name dir)))
       (display "Download?[y/n]:")
       (cond [(string-ci=? (read-line) "y")
              (with-handlers ((exn:fail:filesystem? (lambda (exn) (void))))
                (make-directory "file"))
              (display-to-file (file-content dir) (build-path 'same "file" (bytes->string/utf-8 (file-name dir))) #:exists 'truncate/replace)
              "Successful"]
             [else "Cancelled"]))
     (define (structure->port dir)
       (input-port-append
        #t
        (open-input-string (format "[:dir:~a>:" (file-name dir)))
        (file-port dir)
        (open-input-string "]")))])
  (define (port->directory port)
    (with-handlers ((exn:fail:contract? (lambda (exn) #f)))
      (apply directory `(,@(cdr (regexp-try-match #rx#"^[[]:dir:(.*?)>:(.*?)[]]$" port)) #f))))

  ;;TODO

  (define-syntax (handleInput stx)
    (syntax-case stx ()
      ((_ object) #`(cond
                      ;;TCP INPUT
                      ((cond
                         ((port->directory object))
                         ((port->file object))
                         ((port->link object))
                         ((port->message object))
                         (else #f))
                       => structure-out)
                      ;;CURRENT INPUT
                      ((cond
                         ((directory? object))
                         ((file? object))
                         ((link? object))
                         ((message? object))
                         (else #f))
                       (structure->port object))
                      (else object))))))

(module* crypto #f
  (require (only-in racket/generator sequence->repeated-generator)
           (only-in racket/math exact-floor)
           (only-in racket/list make-list list-update indexes-of))
  (provide vigenere-encrypt vigenere-decrypt makePrime)

  (define (vigenere-encrypt input-port output-port byte-string)
    (define generator (sequence->repeated-generator byte-string))
    (for ((byte (in-input-port-bytes input-port)) #:break (eof-object? byte))
      (write-byte (remainder (+ byte (generator)) 256) output-port)))
  (define (vigenere-decrypt input-port output-port byte-string)
    (define generator (sequence->repeated-generator byte-string))
    (for ((byte (in-input-port-bytes input-port)) #:break (eof-object? byte))
      (write-byte (remainder (+ (- byte (generator)) 256) 256) output-port)))
  (define (makePrime maximum)
    (define root (sub1 (exact-floor (sqrt maximum))))
    (define result
      (let loop
        ((base 2)
         (temp (make-list root 0)))
        (if (> (* base 2) root) temp
            (loop
             (add1 base)
             (let work
               ((num 2)
                (temp temp))
               (if (> (* base num) root) temp (work (add1 num) (list-update temp (- (* num base) 2) add1))))))))
    (map (lambda (n) (+ 2 n)) (indexes-of result 0 =))))

(module* parallel #f
  (require (only-in file/gzip gzip-through-ports)
           (only-in file/gunzip gunzip-through-ports)
           (only-in racket/string string-prefix?)
           (only-in racket/file make-temporary-file)
           (only-in racket/date current-date)
           (only-in racket/port copy-port)
           (only-in file/zip zip->output)
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
                               ((string-prefix? syn "dir>")
                                (define zip (make-temporary-file "rkt~a.zip"))
                                (define path (resolve-path (substring syn 4)))
                                (with-handlers ((exn:fail:filesystem? (lambda (exn) (apply message name "error" (getTime)))))
                                  (parameterize ((current-output-port (open-output-file zip #:exists 'truncate/replace))
                                                 (current-directory path))
                                    (zip->output
                                     (for/list
                                         ((fn (in-directory))
                                          #:when (not (directory-exists? fn)))
                                       (resolve-path fn)))
                                    (close-output-port (current-output-port)))
                                  (directory
                                   (path->string (let-values (((base name bool) (split-path path)))
                                                   name))
                                   zip
                                   #f)))
                               ((string-prefix? syn "file>")
                                (define path (resolve-path (substring syn 5)))
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
    (define-values (in-in in-out) (make-pipe))
    (thread (lambda () (copy-port in in-out)))
    (displayln "You can Chat now.")
    (handleIO in-in out name)))

(module* main #f

  (require (submod ".." listener)
           (submod ".." connector)
           (submod ".." parallel)
           (only-in racket/cmdline command-line))

  (define cname (make-parameter #f))
  (define cmode (make-parameter "accept"))
  (define cport (make-parameter #f))
  (define chost (make-parameter #f))

  (define (parseCmdln)
    (command-line
     #:program "qcr"
     #:once-any (("+n" "++name") n "Your name" (cname n))
     #:once-any (("+m" "++mode") m "Current mode[accept/connect,default to accept]" (cmode m))
     #:once-any (("+p" "++port") p "Port number" (cport (string->number p)))
     #:once-any (("+h" "++host") h "Hostname[default to none]" (chost h))))

  (begin
    (parseCmdln)
    (define name (cname))
    (define mode (cmode))
    (define port (cport))
    (define host (chost))
    (parameterize ([current-custodian (make-custodian)])
      (break-enabled #t)
      (with-handlers (;[exn:fail? (lambda (exn) (custodian-shutdown-all (current-custodian)))]
                      [exn:break? (lambda (exn) (custodian-shutdown-all (current-custodian)))])
        (define-values (in out)
          (cond [(string-ci=? mode "Accept") (createListener port host)]
                [else (createConnector host port)]))
        (displayln "Connect Successfully.")
        (runParallel in out name)))))