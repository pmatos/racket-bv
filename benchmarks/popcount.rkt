#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require bv)

;; ---------------------------------------------------------------------------------------------------

;; Calculates all popcount values for 16 bit numbers
;; through shift or.
(for ([n (in-range (- (expt 2 16) 1))])
  (define v (bv n 16))

  ;; Calculating popcount of v
  ; Breaks when v is zero
  (let loop ([v v] [pc 0])
    (if (bvzero? v)
        (printf "~a," pc)
        (loop (bvlshr v (bv 1 16))
              (+ pc (bitvector->natural
                     (bvand v
                            (bv 1 16))))))))
