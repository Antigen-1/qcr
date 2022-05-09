#lang racket/base
(require ffi/unsafe ffi/unsafe/define)
(provide dir->zip)

(define libzip (ffi-lib (if (equal? (system-type 'os) 'windows) "zip" "libzip") #:get-lib-dirs (lambda () (list (string->path "libzip")))))
(define-ffi-definer define-zip libzip)
(define ZIP_CREATE 1)
(define zip_t_p (_cpointer/null 'zip_t))
(define-zip zip_open (_fun _path _int (r : (_ptr io _int)) -> (v : zip_t_p) -> (if (zero? r) v (error (format "errorp : ~a" r)))))
(define ZIP_FL_ENC_GUESS 0)
(define zip_int64_t _int64)
(define zip_flags_t _uint32)
(define zip_source_t_p (_cpointer 'zip_source_t))
(define-zip zip_source_file (_fun zip_t_p _path _uint64 _int64 -> zip_source_t_p))
(define-zip zip_file_add (_fun zip_t_p _string zip_source_t_p zip_flags_t -> (r : zip_int64_t)
                               -> (when (= r -1)
                                    (error "zip_file_add : fail"))))
(define-zip zip_close (_fun zip_t_p -> (r : _int)
                            -> (when (= r -1)
                                 (error "zip_close : fail"))))
(define dir->zip
  (lambda (path zip)
    (define v (zip_open zip ZIP_CREATE 0))
    (for ((fn (in-directory path)))
      (cond
        ((file-exists? fn)
         (zip_file_add
          v
          (let-values (((base name bool) (split-path fn)))
            (path->string name))
          (zip_source_file v fn 0 -1)
          ZIP_FL_ENC_GUESS))
        (else (void))))
    (zip_close v)))