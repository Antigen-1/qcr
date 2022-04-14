#lang racket/base
(require (prefix-in o: racket/base))
(provide (all-defined-out))

;;newline
(define read-line (lambda ([in (current-input-port)]) (o:read-line in 'any)))
(define read-bytes-line (lambda ([in (current-input-port)]) (o:read-bytes-line in 'any)))