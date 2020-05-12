#lang info

(define version "0.1")

(define collection "bv")

(define deps '("base" "mischief" "math-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "quickcheck" "rosette"))

(define scribblings '(("scribblings/bv.scrbl" ())))
(define pkg-desc "Bitvector library following a Rosette-like API")
