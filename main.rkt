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
           (only-in racket/block block)
           (only-in racket/file display-to-file))
  (provide (struct-out message) (struct-out file) handleInput)

  (struct message (name content hour minute second timezone))
  (define (stream->message stream)
    (with-handlers ((exn:fail:contract? (lambda (exn) #f)))
      (apply message (cdr (regexp-match #rx"(?-m:^[[]:message:(.*)>:(.*)<([0-9]*):([0-9]*):([0-9]*),(.*)>[]]$)" stream)))))
  (define (message-out message) (format "~a>:~a<~a:~a:~a,~a>"
                                        (message-name message)
                                        (message-content message)
                                        (message-hour message)
                                        (message-minute message)
                                        (message-second message)
                                        (message-timezone message)))
  (define (message->stream message) (format "[:message:~a]" (message-out message)))

  (struct file message ())
  (define (stream->file stream)
    (with-handlers ((exn:fail:contract? (lambda (exn) #f)))
      (apply file (append (cdr (regexp-match #rx"(?-m:^[[]:file:(.*)>:(.*)[]]$)" stream)) #f #f #f #f))))
  (define (file-out file)
    (display "Download?[y/n]:")
    (cond [(string-ci=? (read-line) "y")
           (with-handlers ((exn:fail:filesystem? (lambda (exn) (void))))
             (make-directory (string->path "file")))
           (display-to-file (message-content file) (path-join "file" (message-name file)) #:exists 'truncate/replace)
           "Successful"]
          [else "Cancelled"]))
  (define (file->stream file) (format "[:file:~a>:~a]" (message-name file) (message-content file)))

  ;;TODO

  (begin-for-syntax
    (define (generate object)
      #`(cond
          [(or (stream->file #,object) (file? #,object)) (values stream->file file->stream file-out file?)]
          [(or (stream->message #,object) (message? #,object)) (values stream->message message->stream message-out message?)]
          [else (values #f #f #f #f)])))
  (define-syntax (handleInput stx)
    (syntax-case stx ()
      ((_ object) #`(block
                     (define-values (stream->structure structure->stream structure-out structure?) #,(generate #'object))
                     (if (and stream->structure structure->stream structure-out structure?)
                         (cond ((stream->structure object)
                                ;;TCP INPUT
                                => structure-out)
                               ((structure? object)
                                ;;CURRENT INPUT
                                (structure->stream object)))
                         object))))))

(module* parallel #f
  (require (only-in racket/place place* place-channel-get place-channel-put)
           (only-in file/gzip gzip-through-ports)
           (only-in file/gunzip gunzip-through-ports)
           (only-in racket/string string-prefix?)
           (only-in racket/file file->bytes)
           (only-in racket/path string->some-system-path)
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
      (cond ((input-port? syn) (define string-port (open-output-string))
                               (gunzip-through-ports syn string-port)
                               (displayln (handleInput (get-output-string string-port)))
                               (flush-output (current-output-port)))
            ((string? syn) (gzip-through-ports (open-input-string
                                                (handleInput
                                                 (cond
                                                   ((string-prefix? syn "file>")
                                                    (define path (string->some-system-path (substring syn 5) (system-type 'os)))
                                                    (with-handlers ((exn:fail:filesystem? (lambda (exn) (apply message name "error" (getTime)))))
                                                      (file (path->string (let-values (((base name bool) (split-path path)))
                                                                            base))
                                                            (file->bytes path)
                                                            #f #f #f #f)))
                                                   (else (apply message name syn (getTime)))))) out #f 0)
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
           (submod ".." parallel))
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
    (parameterize ([current-custodian (make-custodian)])
      (break-enabled #t)
      (with-handlers ([exn:fail? (lambda (exn) (custodian-shutdown-all (current-custodian)))]
                      [exn:break? (lambda (exn) (custodian-shutdown-all (current-custodian)))])
        (define-values (in out)
          (cond [(string-ci=? mode "Accept") (createListener port hostname)]
                [else (createConnector hostname port)]))
        (displayln "Connect Successfully.")
        (runParallel in out name)))))