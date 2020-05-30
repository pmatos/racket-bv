#lang racket/base
;;
;; Copyright 2020 Paulo Matos
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
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
