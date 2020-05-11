#lang racket/base
;; ---------------------------------------------------------------------------------------------------
(require math/base
         racket/contract
         "bv.rkt")

(provide
 (contract-out
  [random-bv
   (exact-positive-integer? . -> . bv?)]
  [random-bv/limits
   (exact-positive-integer? exact-nonnegative-integer? exact-positive-integer? . -> . bv?)]))
;; ---------------------------------------------------------------------------------------------------

(define (random-bv byte-width)
  (bv (random-bits (* 8 byte-width))
      (* byte-width 8)))

(define (random-bv/limits byte-width min-v max-v)
  (bv (random-integer min-v (add1 max-v)) (* 8 byte-width)))

;; ---------------------------------------------------------------------------------------------------
(module+ test

  (require rackunit
           rackunit/text-ui)

  (run-tests
   (test-suite
    "Tests for bitvector utilities"

    ;; always pass provides a stub for tests.
    (test-case "always pass"
      (check-true #t)))))
