;;Copyright 2022 ZhangHao
;;本程序是自由软件：你可以再分发之和/或依照由自由软件基金会发布的 GNU 通用公共许可证修改之，无论是版本 3 许可证，还是（按你的决定）任何以后版都可以。
;;发布该程序是希望它能有用，但是并无保障;甚至连可销售和符合某个特定的目的都不保证。请参看 GNU 通用公共许可证，了解详情。
;;你应该随程序获得一份 GNU 通用公共许可证的复本。如果没有，请看 <https://www.gnu.org/licenses/>。
#lang racket/base

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
  (require (only-in racket/tcp tcp-listen tcp-accept/enable-break tcp-close))
  (provide createListener)
  (define (createListener port [hostname #f])
    (let ((listener (tcp-listen port 1 #f hostname)))
      (define-values (in out) (tcp-accept/enable-break listener))
      (tcp-close listener)
      (values in out))))

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
                                                                (format "message\n~s"
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
                               (cond [(string-ci=? (read-line (current-input-port) 'any) "y")
                                      (with-handlers ((exn:fail:filesystem? (lambda (exn) (void))))
                                        (make-directory "file"))
                                      (call-with-output-file
                                          #:exists 'truncate/replace
                                        (build-path 'same "file" (file-name file))
                                        (lambda (out) (write-bytes (file-content file) out)))
                                      "Successful"]
                                     [else "Cancelled"]))
                             (define (structure->port file) (input-port-append
                                                             #t
                                                             (open-input-string (format "file\n~a\n" (file-name file)))
                                                             (file-port file)))])
  (define (port->file port)
    (with-handlers ((exn:fail:contract? (lambda (exn) #f)))
      (if (bytes=? #"file" (peek-bytes 4 0 port))
          (begin (read-bytes-line port)
                 (let ((name (bytes->string/utf-8 (read-bytes-line port)))) (file name (port->bytes port) #f)))
          #f)))

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
       (cond ((string-ci=? "y" (read-line (current-input-port) 'any))
              ((dynamic-require 'browser/external 'send-url) url #t)
              "ok")
             (else (format "~a:cancelled" (message-content link)))))
     (define (structure->port link)
       (open-input-string
        (format "link\n~s"
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
       (cond [(string-ci=? (read-line (current-input-port) 'any) "y")
              (with-handlers ((exn:fail:filesystem? (lambda (exn) (void))))
                (make-directory "file"))
              (call-with-output-file
                  #:exists 'truncate/replace
                (build-path 'same "file" (file-name dir))
                (lambda (out) (write-bytes (file-content dir) out)))
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
      (if (bytes=? #"dir" (peek-bytes 3 0 port))
          (begin (read-bytes-line port)
                 (let ((name (bytes->string/utf-8 (read-bytes-line port)))) (directory name (port->bytes port) #f)))
          #f)))

  ;;TODO

  (define (handleInput object)
    (cond
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
      (else object))))

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
    (_fun (_int = (- (RSA_size p) 11)) _bytes (o : (_bytes o (RSA_size p))) (p : RSA_p) (_int = 1)
          -> (r : _int)
          -> (if (= -1 r) (error "RSA_public_encrypt : fail.") o)))
  (define-libcrypto RSA_private_decrypt
    (_fun (_int = (RSA_size p)) _bytes (o : (_bytes o (- (RSA_size p) 11))) (p : RSA_p) (_int = 1)
          -> (r : _int)
          -> (if (= -1 r) (error "RSA_private_decrypt : fail.") o)))
  (define-libcrypto RSA_free (_fun RSA_p -> _void))
  )

(module* protocol #f
  (require (only-in racket/string string-prefix?)
           (only-in racket/file make-temporary-file display-to-file file->bytes)
           (only-in racket/date current-date)
           (only-in racket/port copy-port port->bytes make-limited-input-port)
           (only-in racket/generator sequence->repeated-generator)
           (only-in file/zip zip->output)
           (only-in racket/random crypto-random-bytes)
           (only-in racket/format ~a)
           (only-in openssl/md5 md5-bytes)
           (only-in racket/port read-line-evt)
           (submod ".." crypto)
           (submod ".." extension))
  (provide mkProtocol handleIO copy-from-port copy-into-port)

  (define d-generator (make-parameter #f (lambda (sequence) (sequence->repeated-generator sequence))))
  (define e-generator (make-parameter #f (lambda (sequence) (sequence->repeated-generator sequence))))

  (define (getTime)
    (let ((date (current-date)))
      (list (date-hour date) (date-minute date) (date-second date) (date*-time-zone-name date))))
  (define (copy-into-port in out)
    (let ((bytes-string (port->bytes in)))
      (define data (bytes-append (md5-bytes (open-input-bytes bytes-string)) bytes-string))
      (define len (string->bytes/utf-8 (~a (bytes-length data))))
      (write-bytes len out)
      (write-byte 10 out)
      (vigenere-encrypt (open-input-bytes data) out (e-generator))
      (flush-output out)))
  (define (copy-from-port in out)
    (let ((len (string->number (bytes->string/utf-8 (read-bytes-line in)))))
      (define temp (open-output-bytes))
      (vigenere-decrypt (make-limited-input-port in len #f) temp (d-generator))
      (define bytes-port (open-input-bytes (get-output-bytes temp)))
      (define md5 (read-bytes 16 bytes-port))
      (define bytes (port->bytes bytes-port))
      (if (bytes=? md5 (md5-bytes (open-input-bytes bytes))) (write-bytes bytes out) (error "Fail to verify."))
      (flush-output out)))
  (define (handleIO in out name)
    (define sema (make-semaphore 1))
    (define thd
      (thread
       (lambda ()
         (let loop ()
           (define syn (thread-receive))
           (if (input-port? syn)
               (cond ((eof-object? (peek-byte syn)) (void))
                     (else
                      (define port (open-output-bytes))
                      (copy-from-port syn port)
                      (displayln (handleInput (open-input-bytes (get-output-bytes port))))
                      (flush-output (current-output-port))
                      (loop)))
               (if (eof-object? syn) (void)
                   (begin
                     (semaphore-wait sema)
                     (copy-into-port
                      (handleInput
                       (cond
                         ((string-prefix? syn "dir>")
                          (define zip (make-temporary-file "rkt~a.zip"))
                          (define path (resolve-path (substring syn 4)))
                          (with-handlers ((exn:fail:filesystem? (lambda (exn) (error "Directory constructor : fail."))))
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
                          (with-handlers ((exn:fail:filesystem? (lambda (exn) (error "File constructor : fail."))))
                            (file (path->string (let-values (((base name bool) (split-path path)))
                                                  name))
                                  path #f)))
                         ((string-prefix? syn "link>")
                          (apply link name (substring syn 5) (getTime)))
                         (else (apply message name syn (getTime)))))
                      out)
                     (flush-output out)
                     (semaphore-post sema)
                     (loop))))))))
    (void
     (thread
      (lambda ()
        (define sema-evt (semaphore-peek-evt sema))
        (let loop ()
          (define r (sync in (wrap-evt sema-evt (lambda (b) (sync (read-line-evt (current-input-port) 'any) always-evt)))))
          (if (evt? r) (void) (thread-send thd r))
          (loop)))))
    thd)
  (define (mkProtocol in out name)
    (define m-public (file->bytes (build-path "keys" "key.pub.pem")))
    (write-bytes (string->bytes/utf-8 (~a (bytes-length m-public))) out)
    (write-byte 10 out)
    (write-bytes m-public out)
    (flush-output out)
    (define bio-pub (BIO_new_file
                     (let ((temp (make-temporary-file))
                           (len (string->number (bytes->string/utf-8 (read-bytes-line in)))))
                       (call-with-output-file #:exists 'truncate/replace temp (lambda (out) (write-bytes (read-bytes len in) out)))
                       (path->complete-path temp))
                     "rb"))
    (define o-public (PEM_read_bio_RSAPublicKey bio-pub))
    (define bio-pri (BIO_new_file (path->complete-path (build-path "keys" "key.pem")) "rb"))
    (define m-private (PEM_read_bio_RSAPrivateKey bio-pri))
    (define crypto-bytes (crypto-random-bytes (- (RSA_size o-public) 11)))
    (define m-key (RSA_public_encrypt crypto-bytes o-public))
    (write-bytes m-key out)
    (flush-output out)
    (define o-key (RSA_private_decrypt (read-bytes (RSA_size m-private) in) m-private))
    (e-generator crypto-bytes)
    (d-generator o-key)
    (RSA_free o-public)
    (RSA_free m-private)
    (BIO_free bio-pri)
    (BIO_free bio-pub)
    (displayln name out)
    (flush-output out)
    (define o-name (read-line in))
    (displayln (format "You can Chat with ~a now." o-name))
    ))

(module* main #f

  (require (submod ".." listener)
           (submod ".." connector)
           (submod ".." protocol)
           (submod ".." crypto)
           (only-in racket/file delete-directory/files)
           (only-in racket/cmdline command-line))
  (provide mkRSATransport)

  (define cname (make-parameter #f (lambda (v) (if (string? v) v (error "name : not a string.")))))
  (define cmode (make-parameter "accept" (lambda (v) (if (string? v) v (error "mode : not a string.")))))
  (define cport (make-parameter #f (lambda (v) (if (exact-nonnegative-integer? v) v (error "port : not a exact-nonnegative-integer")))))
  (define chost (make-parameter #f (lambda (v) (if (string? v) v (error "host : not a string.")))))

  (define mkRSATransport
    (lambda (k)
      (define rsa (RSA_new))
      (define e (BN_new))
      (define private (BIO_new_file (path->complete-path "key.pem") "wb"))
      (define public (BIO_new_file (path->complete-path "key.pub.pem") "wb"))
      (BN_set_word e 65537)
      (RSA_generate_key_ex rsa k e)
      (PEM_write_bio_RSAPrivateKey private rsa)
      (PEM_write_bio_RSAPublicKey public rsa)
      (RSA_free rsa)
      (BN_free e)
      (BIO_free private)
      (BIO_free public)))

  (define (parseCmdln)
    (command-line
     #:program "qcr"
     #:once-any (("+n" "++name") n "Your name" (cname n))
     #:once-any (("+m" "++mode") m "Current mode[accept/connect,default to accept]" (cmode m))
     #:once-any (("+p" "++port") p "Port number" (cport (string->number p)))
     #:once-any (("+h" "++host") h "Hostname[default to none]" (chost h))
     #:once-any (("+k" "++keys") k "Update rsa keys"
                                 (if (directory-exists? "keys") (delete-directory "keys") (void))
                                 (make-directory "keys")
                                 (parameterize ((current-directory "keys"))
                                   (mkRSATransport (string->number k)))
                                 (exit))
     #:ps
     "1.首次运行请使用`./main +k [n]`生成指定大小的RSA公钥和私钥。"
     "2.客户端（`+m connect`）下必须通过`+h`参数指定hostname，服务端（`+m accept`或默认）下此参数不必需。"
     "3.支持以命令方式进行文件、目录、链接的分享。
        命令基本格式：[type]>[content]
        type：file|dir|link
        content：如分享链接则为url，如分享文件、目录则为path
        所有不符合命令格式的输入均被识别为普通消息予以发送"
     "4.ctrl-c退出"))

  (begin
    (parseCmdln)
    (define name (cname))
    (define mode (cmode))
    (define port (cport))
    (define host (chost))
    (parameterize ([current-custodian (make-custodian)])
      (break-enabled #t)
      (with-handlers ([exn:break? (lambda (exn) (custodian-shutdown-all (current-custodian)))])
        (define-values (in out)
          (cond [(string-ci=? mode "Accept") (createListener port host)]
                [else (createConnector host port)]))
        (displayln "Connect Successfully.")
        (mkProtocol in out name)
        (define thd (handleIO in out name))
        (when (sync (thread-dead-evt thd))
          (custodian-shutdown-all (current-custodian)))))))