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
  (require (only-in racket/date current-date)
           (only-in racket/tcp tcp-port?)
           (only-in racket/port input-port-append port->string copy-port)
           (only-in racket/file make-temporary-file)
           (only-in file/zip zip->output)
           racket/class)
  (provide processor% message-processor% link-processor% file-processor% directory-processor% handleInput)

  (define (getTime)
    (let ((date (current-date)))
      (list (date-hour date) (date-minute date) (date-second date) (date*-time-zone-name date))))

  (define processor% (class object%
                       (init port mode)
                       (super-new)
                       (define processor-mode mode)
                       (define processor-port port)
                       (define/pubment process
                         (lambda () (inner (void) process processor-port processor-mode)))))

  (define message-processor%
    (class processor%
      (init port mode)
      (super-new [port port] [mode mode])
      (define process
        (lambda (processor-port processor-mode)
          (case processor-mode
            ((make) (input-port-append #t (open-input-string ">>") processor-port (open-input-string (apply format "<~a:~a:~a,~a>" (getTime)))))
            ((parse) (port->string processor-port)))))
      (augment process)))

  (define link-processor%
    (class processor%
      (init port mode)
      (super-new [port port] [mode mode])
      (define process
        (lambda (processor-port processor-mode)
          (case processor-mode
            ((make) (read-string 5 processor-port)
                    (open-input-string (format "~s" (cons (port->string processor-port) (getTime)))))
            ((parse) (let ((data (read processor-port)))
                       (displayln (apply format "link:~a<~a:~a:~a,~a>" data))
                       (display "Redirect[y/n]:")
                       (cond ((string-ci=? "y" (read-line (current-input-port) 'any))
                              ((dynamic-require 'browser/external 'send-url) (car data) #t)
                              "ok")
                             (else (format "~a:cancelled" (car data)))))))))
      (augment process)))

  (define file-processor%
    (class processor%
      (init port mode)
      (super-new [port port] [mode mode])
      (define/public confirm (lambda (name processor-port)
                               (cond [(string-ci=? (read-line (current-input-port) 'any) "y")
                                      (if (directory-exists? "file") (void) (make-directory "file"))
                                      (call-with-output-file
                                          #:exists 'truncate/replace
                                        (build-path 'same "file" name)
                                        (lambda (out) (copy-port processor-port out)))
                                      "Successful"]
                                     [else "Cancelled"])))
      (define process
        (lambda (processor-port processor-mode)
          (case processor-mode
            ((make) (read-string 5 processor-port)
                    (define path-string (port->string processor-port))
                    (let-values (((base name) (split-path path-string)))
                      (input-port-append #t (open-input-string (format "~a\n" (path->string name))) (open-input-file path-string))))
            ((parse) (let ((name (read-line processor-port)))
                       (displayln (format "file:~a" name))
                       (display "Download?[y/n]:")
                       (confirm name processor-port))))))
      (augride process)))

  (define directory-processor%
    (class file-processor%
      (init port mode)
      (super-new [port port] [mode mode])
      (inherit confirm)
      (define process
        (lambda (processor-port processor-mode)
          (case processor-mode
            ((make) (define tmp (make-temporary-file))
                    (read-string 4 processor-port)
                    (define path-string (port->string processor-port))
                    (parameterize ((current-directory path-string))
                      (call-with-output-file tmp #:exists 'truncate/replace
                        (lambda (out)
                          (zip->output
                           (for/list ((file (in-directory)))
                             file)
                           out))))
                    (let-values (((base name) (split-path path-string)))
                      (input-port-append #t (open-input-string (format "~a\n" (path->string name))) (open-input-file tmp))))
            ((parse) (let ((name (read-line processor-port)))
                       (displayln (format "dir:~a" name))
                       (display "Download?[y/n]:")
                       (confirm name processor-port))))))
      (override process)))

  ;;TODO

  (define (handleInput object)
    (case (if (tcp-port? object) (string->symbol (read-line object)) object)
      ((message) message-processor%)
      ((link) link-processor%)
      ((file) file-processor%)
      ((dir) directory-processor%))))

(module* crypto #f
  (require (only-in openssl/libcrypto libcrypto)
           ffi/unsafe
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
  (require (only-in racket/file make-temporary-file file->bytes)
           (only-in racket/port copy-port port->bytes make-limited-input-port)
           (only-in racket/generator sequence->repeated-generator)
           (only-in racket/random crypto-random-bytes)
           (only-in racket/format ~a)
           (only-in openssl/md5 md5-bytes)
           (only-in racket/port read-line-evt)
           racket/class
           (submod ".." crypto)
           (submod ".." extension))
  (provide mkProtocol handleIO copy-from-port copy-into-port)

  (define d-generator (make-parameter #f (lambda (sequence) (sequence->repeated-generator sequence))))
  (define e-generator (make-parameter #f (lambda (sequence) (sequence->repeated-generator sequence))))

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
  (define <type>? (lambda (port type) (string=? (peek-string (add1 (string-length type)) 0 port) (string-append type ">"))))
  (define (handleIO in out name)
    (thread
     (lambda ()
       (let loop ()
         (sync
          (handle-evt
           in
           (lambda (syn)
             (if (eof-object? (peek-byte syn)) (void)
                 (let ((% (handleInput syn)))
                   (define output (open-output-bytes))
                   (copy-from-port syn output)
                   (displayln (send (new % [port (open-input-bytes (get-output-bytes output))] [mode 'parse]) process))
                   (loop)))))
          (handle-evt
           (read-line-evt (current-input-port) 'any)
           (lambda (syn)
             (if (eof-object? syn) (void)
                 (let ((input (open-input-string syn)))
                   (define type
                     (cond ((findf (lambda (type) (<type>? input type)) (list "link" "file" "dir"))
                            => (lambda (s) (string->symbol s)))
                           (else 'message)))
                   (displayln type out)
                   (let ((% (handleInput type)))
                     (define port (send (new % [port input] [mode 'make]) process))
                     (copy-into-port port out)
                     (flush-output out)
                     (close-input-port port))
                   (loop))))))))))
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
                                 (if (directory-exists? "keys") (delete-directory/files "keys") (void))
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