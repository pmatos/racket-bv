#lang info

(define version "0.1")

(define collection "bv")

(define deps '("base" "mischief" "math-lib"))
(define build-deps
  '("sandbox-lib"
    "scribble-lib"
    "racket-doc"
    "rackunit-lib"
    "quickcheck"
    ("rosette" #:version "3.2")))

(define scribblings '(("scribblings/bv.scrbl" ())))
(define pkg-desc "Bitvector library following a Rosette-like API")
