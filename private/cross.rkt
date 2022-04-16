#lang racket/base
(require (prefix-in o: racket/base)
         (prefix-in o: racket/port))
(provide (all-defined-out))

;;newline
(define read-line (lambda ([in (current-input-port)]) (o:read-line in 'any)))
(define read-bytes-line (lambda ([in (current-input-port)]) (o:read-bytes-line in 'any)))
(define read-line-evt (lambda ([in (current-input-port)]) (o:read-line-evt in 'any)))