#lang racket/base
(provide (all-defined-out))

;;newline
(define read-line (lambda ([in (current-input-port)]) (read-line in 'any)))
(define read-bytes-line (lambda ([in (current-input-port)]) (read-bytes-line in 'any)))