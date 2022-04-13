#lang racket/base
(provide (all-defined-out))

;;newline
(define read-line (lambda (in) (read-line in 'any)))
(define read-bytes-line (lambda (in) (read-bytes-line in 'any)))