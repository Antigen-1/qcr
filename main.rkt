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

(module* parallel #f
  (require (only-in racket/place place* place-channel-get place-channel-put)
           (only-in file/gzip gzip-through-ports)
           (only-in file/gunzip gunzip-through-ports))
  (provide runParallel)

  (define (runParallel in out name)
    (let-values (((pl i o e) (place* #:in #f #:out (current-output-port) #:err #f ch
                                     (define in (place-channel-get ch))
                                     (define name (place-channel-get ch))
                                     (let loop ()
                                       (let ((bytes-port (open-output-bytes)))
                                         (gunzip-through-ports in bytes-port)
                                         (display name)
                                         (display #\>)
                                         (display (get-output-bytes bytes-port))
                                         (display #\newline))
                                       (loop)))))
      (place-channel-put pl in)
      (place-channel-put pl name)
      (displayln "You can Chat now.")
      (let loop ()
        (gzip-through-ports (open-input-bytes (read-bytes-line (current-input-port))) out #f 0)
        (flush-output out)
        (loop)))))

(module* main #f

  (require (submod ".." listener)
           (submod ".." connector)
           (submod ".." parallel))
  (provide getMode getName getHostname getPort)


  (define (getName) (let ()
                      (display "name:\n")
                      (read-line)))
  (define (getMode) (let ()
                      (display "mode [Accept/Connect]:\n")
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
      (with-handlers ([exn:fail? (lambda (exn) (custodian-shutdown-all (current-custodian)))])
        (define-values (in out)
          (cond [(string-ci=? mode "Accept") (createListener port hostname)]
                [else (createConnector hostname port)]))
        (displayln "Connect Successfully.")
        (runParallel in out)))))