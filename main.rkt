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

(define exe (make-will-executor))
(void (thread (lambda () (let loop () (will-execute exe) (loop)))))

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
           (only-in racket/port input-port-append port->bytes)
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
                             (define (structure->port message) (open-input-string
                                                                (format "message\n~v"
                                                                        (list (message-name message) (message-content message) (message-hour message)
                                                                              (message-minute message) (message-second message) (message-timezone message)))))])
  (define (port->message port)
    (with-handlers ((exn:fail:contract? (lambda (exn) #f)))
      (if (bytes=? #"message" (peek-bytes 7 0 port))
          (begin (read-bytes-line port) (let ((list (read port))) (apply message list)))
          #f)))

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
                                                             (open-input-string (format "file\n~a\n" (file-name file)))
                                                             (file-port file)))])
  (define (port->file port)
    (with-handlers ((exn:fail:contract? (lambda (exn) #f)))
      (if (bytes=? #"file" (peek-bytes 4 0 port)) (begin (read-bytes-line port) (file (read-bytes-line port) (port->bytes port) #f)) #f)))

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
        (format "link\n~v"
                (list
                 (message-name link)
                 (message-content link)
                 (message-hour link)
                 (message-minute link)
                 (message-second link)
                 (message-timezone link)))))])
  (define (port->link port)
    (with-handlers ((exn:fail:contract? (lambda (exn) #f)))
      (if (bytes=? #"link" (peek-bytes 4 0 port)) (begin (read-bytes-line port) (apply link (read port))) #f)))

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
        (open-input-string (format "dir\n~a\n" (file-name dir)))
        (file-port dir)
        ))])
  (define (port->directory port)
    (with-handlers ((exn:fail:contract? (lambda (exn) #f)))
      (if (bytes=? #"dir" (peek-bytes 3 0 port)) (begin (read-bytes-line port) (directory (read-bytes-line port) (port->bytes port) #f)) #f)))

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
  (require (only-in openssl/libcrypto libcrypto)
           ffi/unsafe
           (only-in racket/string string-split)
           (only-in ffi/unsafe/define define-ffi-definer))
  (provide (all-defined-out))

  (define (vigenere-encrypt input-port output-port generator)
    (for ((byte (in-input-port-bytes input-port)) #:break (eof-object? byte))
      (write-byte (remainder (+ byte (generator)) 256) output-port)))
  (define (vigenere-decrypt input-port output-port generator)
    (for ((byte (in-input-port-bytes input-port)) #:break (eof-object? byte))
      (write-byte (remainder (+ (- byte (generator)) 256) 256) output-port)))
  (define-ffi-definer define-libcrypto libcrypto)
  (define RSA_p (_cpointer/null 'RSA))
  (define FILE_p (_cpointer/null 'FILE))
  (define BIGNUM_p (_cpointer/null 'BIGNUM))
  (define BIO_p (_cpointer/null 'BIO))
  (define-libcrypto RSA_new (_fun -> (r : RSA_p) -> (if r r (error "RSA_new : fail."))))
  (define-libcrypto BN_new (_fun -> (r : BIGNUM_p) -> (if r r (error "BN_new : fail."))))
  (define-libcrypto BN_free (_fun BIGNUM_p -> _void))
  (define-libcrypto RSA_generate_key_ex (_fun RSA_p _int BIGNUM_p (_pointer = #f) -> _int))
  (define-libcrypto BN_set_word (_fun BIGNUM_p _ullong -> (r : _int) -> (if (zero? r) (error "BN_set_word : fail.") (void))))
  (define-libcrypto BIO_new_file (_fun _file _string -> (r : BIO_p) -> (if r r (error "BIO_new_file : fail."))))
  (define-libcrypto PEM_write_bio_RSAPrivateKey (_fun BIO_p RSA_p (_pointer = #f) (_pointer = #f) (_int = 0) (_pointer = #f) (_pointer = #f)
                                                      -> (r : _int) -> (if (zero? r) (error "PEM_write_bio_RSAPrivateKey : fail.") (void))))
  (define-libcrypto PEM_write_bio_RSAPublicKey (_fun BIO_p RSA_p -> (r : _int)
                                                     -> (if (zero? r) (error "PEM_write_bio_RSAPublicKey : fail.") (void))))
  (define-libcrypto PEM_read_bio_RSAPublicKey (_fun BIO_p (_pointer = #f) (_pointer = #f) (_pointer = #f) -> (r : RSA_p)
                                                    -> (if r r (error "PEM_read_bio_RSAPublicKey : fail."))))
  (define-libcrypto PEM_read_bio_RSAPrivateKey (_fun BIO_p (_pointer = #f) (_pointer = #f) (_pointer = #f) -> (r : RSA_p)
                                                     -> (if r r (error "PEM_read_bio_RSAPrivateKey : fail."))))
  (define-libcrypto BIO_free (_fun BIO_p -> (r : _int) -> (if (zero? r) (error "BIO_free : fail.") (void))))
  (define-libcrypto RSA_size (_fun RSA_p -> _int))
  (define-libcrypto RSA_public_encrypt
    (_fun _int _bytes (o : (_bytes o (RSA_size p))) (p : RSA_p) _int
          -> (r : _int)
          -> (if (= -1 r) (error "RSA_public_encrypt : fail.") o)))
  (define-libcrypto RSA_private_decrypt
    (_fun _int _bytes (o : (_bytes o (RSA_size p))) (p : RSA_p) _int
          -> (r : _int)
          -> (if (= -1 r) (error "RSA_private_decrypt : fail.") o)))
  (define-libcrypto RSA_free (_fun RSA_p -> _void))
  )

(module* parallel #f
  (require (only-in racket/string string-prefix?)
           (only-in racket/file make-temporary-file display-to-file)
           (only-in racket/date current-date)
           (only-in racket/port copy-port port->bytes make-limited-input-port)
           (only-in racket/generator sequence->repeated-generator)
           (only-in file/zip zip->output)
           (only-in racket/random crypto-random-bytes)
           (submod ".." crypto)
           (submod ".." extension))
  (provide runParallel)

  (define d-generator (make-parameter #f (lambda (sequence) (sequence->repeated-generator sequence))))
  (define e-generator (make-parameter #f (lambda (sequence) (sequence->repeated-generator sequence))))

  (define (getTime)
    (let ((date (current-date)))
      (list (date-hour date) (date-minute date) (date-second date) (date*-time-zone-name date))))
  (define (copy-into-port in out)
    (let ((bytes-string (port->bytes in)))
      (displayln (bytes-length bytes-string) out)
      (vigenere-encrypt (open-input-bytes bytes-string) out (e-generator))
      (flush-output out)))
  (define (copy-from-port in out)
    (let ((len (string->number (read-line in))))
      (vigenere-decrypt (make-limited-input-port in len #f) out (d-generator))
      (flush-output out)))
  (define (handleIO in-in out name)
    (let loop ()
      (define syn (sync in-in (read-line-evt)))
      (cond ((input-port? syn) (define port (open-output-bytes))
                               (copy-from-port syn port)
                               (displayln (handleInput (open-input-bytes (get-output-bytes port))))
                               (flush-output (current-output-port)))
            ((string? syn) (copy-into-port
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
                            out)
                           (flush-output out)))
      (loop)))
  (define (runParallel in out name)
    (define-values (in-in in-out) (make-pipe))
    (thread (lambda () (copy-port in in-out)))
    (define m-public (build-path "keys" "key.pub.pem"))
    (displayln (file-size m-public) out)
    (call-with-input-file m-public (lambda (input-port) (copy-port input-port out)))
    (flush-output out)
    (define bio-pub (BIO_new_file
                     (let ((temp (make-temporary-file))
                           (len (string->number (read-line in-in))))
                       (display-to-file (read-bytes len in-in) temp #:exists 'truncate/replace)
                       (path->complete-path temp))
                     "rb"))
    (define o-public (PEM_read_bio_RSAPublicKey bio-pub))
    (define bio-pri (BIO_new_file (path->complete-path (build-path "keys" "key.pem")) "rb"))
    (define m-private (PEM_read_bio_RSAPrivateKey bio-pri))
    (will-register exe bio-pri BIO_free)
    (will-register exe bio-pub BIO_free)
    (will-register exe o-public RSA_free)
    (will-register exe m-private RSA_free)
    (define crypto-bytes (crypto-random-bytes 128))
    (define m-key (RSA_public_encrypt (- (RSA_size o-public) 11) crypto-bytes o-public 1))
    (displayln (bytes-length m-key) out)
    (display m-key out)
    (flush-output out)
    (define o-key (let ((len (string->number (read-line in-in))))
                    (RSA_private_decrypt (RSA_size m-private) (read-bytes len in-in) m-private 1)))
    (e-generator crypto-bytes)
    (d-generator o-key)
    (displayln "You can Chat now.")
    (handleIO in-in out name)))

(module* main #f

  (require (submod ".." listener)
           (submod ".." connector)
           (submod ".." parallel)
           (submod ".." crypto)
           (only-in racket/file delete-directory/files)
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
     #:once-any (("+h" "++host") h "Hostname[default to none]" (chost h))
     #:once-any (("+k" "++keys") k "Update rsa keys"
                                 (with-handlers ((exn:fail:filesystem? (lambda (exn) (void))))
                                   (if (directory-exists? "keys") (delete-directory/files "keys") (void))
                                   (make-directory "keys"))
                                 (define rsa (RSA_new))
                                 (define e (BN_new))
                                 (define private (BIO_new_file (build-path "keys" "key.pem") "wb"))
                                 (define public (BIO_new_file (build-path "keys" "key.pub.pem") "wb"))
                                 (will-register exe rsa RSA_free)
                                 (will-register exe e BN_free)
                                 (will-register exe private BIO_free)
                                 (will-register exe public BIO_free)
                                 (BN_set_word e 65537)
                                 (RSA_generate_key_ex rsa (string->number k) e)
                                 (PEM_write_bio_RSAPrivateKey private rsa)
                                 (PEM_write_bio_RSAPublicKey public rsa)
                                 (exit))))

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