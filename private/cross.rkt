#lang racket/base
(require (prefix-in o: racket/base)
         (prefix-in o: racket/port)
         (only-in racket/string string-join))
(provide (all-defined-out))

;;newline
(define read-line (lambda ([in (current-input-port)]) (o:read-line in 'any)))
(define read-bytes-line (lambda ([in (current-input-port)]) (o:read-bytes-line in 'any)))
(define read-line-evt (lambda ([in (current-input-port)]) (o:read-line-evt in 'any)))

;;path
(define path-join (lambda element (string->path (string-join
                                                 element
                                                 (cond ((equal? (system-type 'os) 'windows) "\\")
                                                       (else "/"))))))