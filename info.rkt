#lang info
(define collection "qcr")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "drracket"))
(define scribblings '(("scribblings/qcr.scrbl" ())))
(define pkg-desc "A Small Chat Room.")
(define version "0.0")
(define pkg-authors '(haozhang))
(define license 'LGPL-3.0-only)