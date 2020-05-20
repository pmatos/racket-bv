#lang racket/base
;; ---------------------------------------------------------------------------------------------------

;; This bitvector implementation follows the Rosette interface and operations
;; and the implementation is similar to that of:
;; https://github.com/IagoAbal/haskell-bv/blob/master/src/Data/BitVector.hs

(require mischief/memoize
         racket/contract
         racket/format
         racket/match
         racket/math)

(provide
 (contract-out
  ;; Returns a predicate that recognizes bitvectors of a given size
  [rename make-bitvector bitvector (natural-number/c . -> . bitvector?)]
  ;; Alias for bv?
  [bv? (any/c . -> . boolean?)]
  ;; Zero predicate
  [bvzero? (bv? . -> . boolean?)]
  ;; Bitvector constructor
  [bv (exact-integer? (or/c exact-positive-integer? bitvector?) . -> . bv?)]
  ;; Bitvector equality
  [bveq (bv? bv? . -> . boolean?)]
  ;; Bitvector signed less than
  [bvslt (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out boolean?])]
  ;; Bitvector unsigned less than
  [bvult (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out boolean?])]
  ;; Bitvector signed less or equal
  [bvsle (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out boolean?])]
  ;; Bitvector unsigned less or equal
  [bvule (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out boolean?])]
  ;; Bitvector signed greater than
  [bvsgt (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out boolean?])]
  ;; Bitvector unsigned greater than
  [bvugt (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out boolean?])]
  ;; Bitvector signed greater or equal
  [bvsge (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out boolean?])]
  ;; Bitvector signed greater or equal
  [bvuge (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out boolean?])]
  ;; Bitvector not
  [bvnot (bv? . -> . bv?)]
  ;; Bitvector and
  [bvand (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out bv?])]

  ;; Bitvector or
  [bvor (->i ([bv1 bv?]
              [bv2 (bv1) (and/c bv?
                                (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
             [out bv?])]
  ;; Bitvector exclusive or
  [bvxor (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out bv?])]
  ;; Bitvector shift left
  [bvshl (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out bv?])]
  ;; Bitvector arithmetic shift right
  [bvashr (->i ([bv1 bv?]
                [bv2 (bv1) (and/c bv?
                                  (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
               [out bv?])]
  ;; Bitvector logical shift right
  [bvlshr (->i ([bv1 bv?]
                [bv2 (bv1) (and/c bv?
                                  (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
               [out bv?])]
  ;; Bitvector negation, i.e. (- x)
  [bvneg (bv? . -> . bv?)]
  ;; Bitvector addition
  [bvadd (bv? bv? . -> . bv?)]
  ;; [bvadd (->i ([bv1 bv?]
  ;;             [bv2 (bv1) (and/c bv?
  ;;                                (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
  ;;             [out bv?])]
  ;; Bitvector subtraction
  [bvsub (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out bv?])]
  ;; Bitvector multiplication
  [bvmul (->i ([bv1 bv?]
               [bv2 (bv1) (and/c bv?
                                 (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
              [out bv?])]
  ;; Bitvector signed division
  [bvsdiv (->i ([bv1 bv?]
                [bv2 (bv1) (and/c bv?
                                  (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
               [out bv?])]
  ;; Bitvector unsigned division
  [bvudiv (->i ([bv1 bv?]
                [bv2 (bv1) (and/c bv?
                                  (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
               [out bv?])]
  ;; Bitvector signed remainder
  [bvsrem (->i ([bv1 bv?]
                [bv2 (bv1) (and/c bv?
                                  (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
               [out bv?])]
  ;; Bitvector unsigned remainder
  [bvurem (->i ([bv1 bv?]
                [bv2 (bv1) (and/c bv?
                                  (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
               [out bv?])]
  ;; Bitvector signed modulo
  [bvsmod (->i ([bv1 bv?]
                [bv2 (bv1) (and/c bv?
                                  (lambda (bv2) (equal? (sbv-type bv2) (sbv-type bv1))))])
               [out bv?])]
  ;; Bitvector concatenation
  [concat (->i ([bv bv?]
                [bvs (listof bv?)])
               [result (bv bvs)
                       (and/c bv? (lambda (result)
                                    (= (bitvector-size (sbv-type result))
                                       (for/fold ([s 0])
                                                 ([b (in-list bvs)])
                                         (bitvector-size (sbv-type b))))))])]
  ;; Bitvector extraction
  [extract (->i ([i (bv) (and/c natural-number/c (</c (bitvector-size (sbv-type bv))))]
                 [j (i) (and/c natural-number/c (<=/c i))]
                 [bv bv?])
                [result (i j) (and/c bv? (lambda (r)
                                           (= (bitvector-size (sbv-type r))
                                              (add1 (- i j)))))])]

  ;; Bitvector sign extension
  [sign-extend (->i ([bv bv?]
                     [t bitvector?])
                    [result (t)
                            (and/c bv?
                                   (lambda (r)
                                     (equal? t (sbv-type r))))])]
  ;; Bitvector zero extension
  [zero-extend (->i ([bv bv?]
                     [t bitvector?])
                    [result (t)
                            (and/c bv?
                                   (lambda (r)
                                     (equal? t (sbv-type r))))])]
  ;; Bitvector to integer conversion
  [bitvector->integer (bv? . -> . integer?)]
  ;; Bitvector to natural number conversion
  [bitvector->natural (bv? . -> . natural-number/c)]
  ;; Integer to bitvector
  [integer->bitvector (integer? bitvector? . -> . bv?)]))

;; ---------------------------------------------------------------------------------------------------

(define (bv=? bv1 bv2 (recursive-equal? #false))
  (and (equal? (sbv-type bv1) (sbv-type bv2))
       (= (sbv-val bv1) (sbv-val bv2))))

(define (bv-hash-1 bv recursive-equal-hash)
  (+ (* 1000 (equal-hash-code (sbv-type bv)))
     (equal-hash-code (sbv-val bv))))

(define (bv-hash-2 bv recursive-equal-hash)
  (+ (* 1000 (equal-secondary-hash-code (sbv-type bv)))
     (equal-secondary-hash-code (sbv-val bv))))

;; Generates a bitmask for the lower n bits
;; Example:
;; (bitmask 5) => 0x1f
;; (bitmask 32) => 0xffffffff
(define/memoize (bitmask n)
  (sub1 (arithmetic-shift 1 n)))

(struct sbv
  (val    ; natural, value of bitvector as a natural number
   type)  ; bitvector, which contains size of bitvector in bits
  #:constructor-name -sbv
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match self
       [(sbv v (bitvector bw))
        (let*-values ([(q r) (quotient/remainder bw 4)]
                      [(p b mw) (if (zero? r) (values "x" 16 q) (values "b" 2 bw))])
          (fprintf port "{bv #~a~a ~a}"
                   p
                   (~r (ufinitize v bw) #:base b #:pad-string "0" #:min-width mw)
                   bw))]
       [_ (error "unknown sbv format")]))])

;; Memoized constructor
(define/memoize (make-bv val type)
  (-sbv val type))

;; Predicate rename
(define bv? sbv?)

(define (bvzero? bv)
  (= (sbv-val bv) 0))

;; Returns a predicate that recognizes bitvectors of width n
(struct bitvector (size)
  #:constructor-name -bitvector
  #:property prop:procedure
  (lambda (self other)
    (and (sbv? other)
         (eq? self (sbv-type other))))
  #:methods gen:custom-write
  [(define write-proc
     (lambda (b port mode)
         (write-string "[" port)
         (print (bitvector-size b) port)
         (write-string "] " port)))])

(define/memoize (make-bitvector size)
  (-bitvector size))

(define (bv val size)
  (match* (val size)
    [(val (and t (bitvector s)))
     (make-bv (bitwise-and val (bitmask s)) t)]
    [(val s)
     (make-bv (bitwise-and val (bitmask s)) (make-bitvector s))]))

(define bveq eq?)

(define/match (bvscmp bv1 bv2 op)
  [((sbv v1 (bitvector s)) (sbv v2 _) op)
   (op (two-complement v1 s) (two-complement v2 s))])

(define/match (bvucmp bv1 bv2 op)
  [((sbv v1 _) (sbv v2 _) op)
   (op v1 v2)])

(define (bvslt v1 v2) (bvscmp v1 v2 <))
(define (bvult v1 v2) (bvucmp v1 v2 <))
(define (bvsle v1 v2) (bvscmp v1 v2 <=))
(define (bvule v1 v2) (bvucmp v1 v2 <=))
(define (bvsgt v1 v2) (bvscmp v1 v2 >))
(define (bvugt v1 v2) (bvucmp v1 v2 >))
(define (bvsge v1 v2) (bvscmp v1 v2 >=))
(define (bvuge v1 v2) (bvucmp v1 v2 >=))

(define/match (bvnot bv)
  [((sbv v (and t (bitvector s))))
   (make-bv (bitwise-and (bitwise-not v) (bitmask s)) t)])

(define/match (bvand bv1 bv2)
  [((sbv v1 t) (sbv v2 _))
   (make-bv (bitwise-and v1 v2) t)])

(define/match (bvor bv1 bv2)
  [((sbv v1 t) (sbv v2 _))
   (make-bv (bitwise-ior v1 v2) t)])

(define/match (bvxor bv1 bv2)
  [((sbv v1 t) (sbv v2 _))
   (make-bv (bitwise-xor v1 v2) t)])

(define/match (bvshl bv n)
  [((sbv v (and t (bitvector s))) (sbv n _))
   (make-bv (bitwise-and (arithmetic-shift v n) (bitmask s)) t)])

(define/match (bvlshr bv n)
  [((sbv v t) (sbv n _))
   (make-bv (arithmetic-shift v (- n)) t)])

(define/match (bvashr bv n)
  [((sbv v (and t (bitvector s))) (sbv n _))
   (make-bv (bitwise-and (arithmetic-shift (two-complement v s) (- n)) (bitmask s)) t)])

(define/match (bvneg bv)
  [((sbv v (and t (bitvector s))))
   (make-bv (- (two-complement v s)) t)])

(define/match (bvadd bv1 bv2)
  [((sbv v1 (and t (bitvector s))) (sbv v2 t2))
   (unless (eq? t t2)
     (error 'bvadd "vectors of different types ~a ~a" bv1 bv2))
   (make-bv (bitwise-and (+ v1 v2) (bitmask s)) t)])

(define/match (bvsub bv1 bv2)
  [((sbv v1 (and t (bitvector s))) (sbv v2 _))
   (make-bv (bitwise-and (- v1 v2) (bitmask s)) t)])

(define/match (bvmul bv1 bv2)
  [((sbv v1 (and t (bitvector s))) (sbv v2 _))
   (make-bv (bitwise-and (* v1 v2) (bitmask s)) t)])

(define/match (bvsdiv bv1 bv2)
  [((sbv v1 (and t (bitvector s))) (sbv v2 _))
   (make-bv (bitwise-and (quotient (two-complement v1 s)
                                   (two-complement v2 s))
                         (bitmask s))
            t)])

(define/match (bvudiv bv1 bv2)
  [((sbv v1 t) (sbv v2 _))
   (make-bv (quotient v1 v2) t)])

(define/match (bvsrem bv1 bv2)
  [((sbv v1 (and t (bitvector s))) (sbv v2 _))
   (make-bv (bitwise-and (remainder (two-complement v1 s)
                                    (two-complement v2 s))
                         (bitmask s))
            t)])

(define/match (bvurem bv1 bv2)
  [((sbv v1 t) (sbv v2 _))
   (make-bv (remainder v1 v2) t)])

(define/match (bvsmod bv1 bv2)
  [((sbv v1 (and t (bitvector s))) (sbv v2 _))
   (make-bv (bitwise-and (modulo (two-complement v1 s)
                                 (two-complement v2 s))
                         (bitmask s))
            t)])

(define (concat bv . bvs)

  (define abvs (cons bv bvs))
  (define total-size
    (for/fold ([s 0])
              ([b (in-list abvs)])
      (+ s (bitvector-size (sbv-type b)))))

  (match-define-values (result _)
    (for/fold ([res (make-bv 0 (make-bitvector total-size))]
               [offset total-size])
              ([b (in-list abvs)])
      (values (bvor res (bvshl (zero-extend b (make-bitvector total-size))
                               (make-bv (- offset (bitvector-size (sbv-type b)))
                                    (make-bitvector total-size))))
              (- offset (bitvector-size (sbv-type b))))))
  result)

(define (extract i j x)
  (make-bv (bitwise-bit-field (sbv-val x) j (add1 i))
           (make-bitvector (+ 1 (- i j)))))

(define (sign-extend bvec t)
  (define diffsize
    (make-bv (- (bitvector-size t) (bitvector-size (sbv-type bvec)))
             (bitvector-size t)))
  (bvashr (bvshl (zero-extend bvec t) diffsize)
          diffsize))

(define (zero-extend bv t)
  (make-bv (sbv-val bv) t))

(define (bitvector->integer bv)
  ;; Copy of finitize from ops-racket
  (let* ([bit (bitvector-size (sbv-type bv))]
         [num (sbv-val bv)]
         [mask (arithmetic-shift -1 bit)]
         [masked (bitwise-and (bitwise-not mask) num)])
    (if (= (bitwise-and masked (arithmetic-shift 1 (sub1 bit))) 0)
        masked
        (- masked (arithmetic-shift 1 bit)))))

(define bitvector->natural sbv-val)

(define (integer->bitvector i t)
  (make-bv (bitwise-and i (bitmask (bitvector-size t))) t))

;; ---------------------------------------------------------------------------------------------------

;; Helper functions for the interface

;; Most significant bit of n using m bits
(define (msb n m)
  (arithmetic-shift n (- (sub1 m))))

;; Given the positive representation of a number
;; return the integer value in two's complement using m bits
(define (two-complement n m)
  (cond
    [(zero? (msb n m)) n]
    [else (- (add1 (bitwise-and (bitwise-not n) (bitmask m))))]))

;; FROM ROSETTE:
; Returns an unsigned representation of the given number, using the specified bitwidth.
; Assumes that val is a real, non-infinite, non-NaN number.
(define (ufinitize val bitwidth)
  (let* ([mask (arithmetic-shift -1 bitwidth)]
         [masked (bitwise-and (bitwise-not mask) (exact-truncate val))])
    masked))

(module+ test
  (require (prefix-in r: rosette)
           quickcheck
           racket/bool
           rackunit
           rackunit/quickcheck
           rackunit/text-ui)

  ;; TODO: Copied from ops-racket, should abstract
  (define (arbitrary-for-bits b)
    (choose-integer (- (expt 2 (- b 1))) (- (expt 2 (- b 1)) 1)))

  (define (arbitrary-natural-for-bits b)
    (choose-integer 0 (- (expt 2 b) 1)))

  (define (arbitrary-between low high)
    (choose-integer low high))

  (define arbitrary-bitvector-pair
    (arbitrary-record cons (list car cdr)
                      arbitrary-integer arbitrary-natural))

  (run-tests
   (test-suite
    "bv implementation using racket bignums"

    (test-case "two equal bvs are equal?"
      (check equal? (bv 2 32) (bv 2 32)))

    (test-case "two equal bvs are eq? (with memoization)"
      (check eq? (bv 2 32) (bv 2 32)))

    (test-case "zeros are equal?"
      (check equal? (bv 0 32) (bv 0 32)))

    (test-case "operation result returns equal to zero?"
      (check eq? (bvsub (bv 2 32) (bv 2 32)) (bvsub (bv 1 32) (bv 1 32))))

    (test-case "omit bitvector call should have no impact in eq?"
      (check eq? (bv 0 32) (bv 0 (make-bitvector 32))))

    (test-case "bitvectors are also equal"
      (check eq? (make-bitvector 32) (make-bitvector 32)))

    (test-case "bitvectors can be used as predicates"
      (check-true ((make-bitvector 32) (bv 2 32))))

    (test-case "bitvectors can be used as predicates 2"
      (check-false ((make-bitvector 4) (bv 0 32))))

    ;; Issue 42 test
    (test-case "srem needs to return positive values"
      (check = (bitvector->natural (bvsrem (bv #xb4abd572 32) (bv #xc3625a93 32))) #xf1497adf))

    ;; These are thorough tests to ensure rosette and this library behave the same
    (with-large-test-count

      (for ([bit (in-list (list 8 16 32 64 128))])
        (test-case (format "integer->bitvector and bitvector->integer are inverse operations (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)])
             (= (bitvector->integer (integer->bitvector n (make-bitvector bit))) n))))

        (test-case (format "integer->bitvector and bitvector->natural are inverse operations (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-natural-for-bits bit)])
             (= (bitvector->natural (integer->bitvector n (make-bitvector bit))) n))))

        (test-case (format "bitvector negation (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)])
             (define bvec (bv n bit))
             (define r:bvec (r:bv n bit))

             (= (bitvector->integer (bvneg bvec))
                (r:bitvector->integer (r:bvneg r:bvec))))))

        (test-case (format "bitvector addition (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (= (bitvector->integer (bvadd bv1 bv2))
                (r:bitvector->integer (r:bvadd r:bv1 r:bv2))))))

        (test-case (format "bitvector subtraction (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (= (bitvector->integer (bvsub bv1 bv2))
                (r:bitvector->integer (r:bvsub r:bv1 r:bv2))))))

        (test-case (format "bitvector multiplication (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (= (bitvector->integer (bvmul bv1 bv2))
                (r:bitvector->integer (r:bvmul r:bv1 r:bv2))))))

        (test-case (format "bitvector signed division (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define m-safe (if (zero? m) 1 m))
             (define bv1 (bv n bit))
             (define bv2 (bv m-safe bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m-safe bit))

             (= (bitvector->integer (bvsdiv bv1 bv2))
                (r:bitvector->integer (r:bvsdiv r:bv1 r:bv2))))))

        (test-case (format "bitvector unsigned division (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define m-safe (if (zero? m) 1 m))
             (define bv1 (bv n bit))
             (define bv2 (bv m-safe bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m-safe bit))

             (= (bitvector->integer (bvudiv bv1 bv2))
                (r:bitvector->integer (r:bvudiv r:bv1 r:bv2))))))

        (test-case (format "bitvector unsigned remainder (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define m-safe (if (zero? m) 1 m))
             (define bv1 (bv n bit))
             (define bv2 (bv m-safe bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m-safe bit))

             (= (bitvector->integer (bvurem bv1 bv2))
                (r:bitvector->integer (r:bvurem r:bv1 r:bv2))))))

        (test-case (format "bitvector signed remainder (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define m-safe (if (zero? m) 1 m))
             (define bv1 (bv n bit))
             (define bv2 (bv m-safe bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m-safe bit))

             (and (= (bitvector->natural (bvsrem bv1 bv2))
                     (r:bitvector->natural (r:bvsrem r:bv1 r:bv2)))
                  (= (bitvector->integer (bvsrem bv1 bv2))
                     (r:bitvector->integer (r:bvsrem r:bv1 r:bv2)))))))

        (test-case (format "bitvector signed modulo (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define m-safe (if (zero? m) 1 m))
             (define bv1 (bv n bit))
             (define bv2 (bv m-safe bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m-safe bit))

             (and (= (bitvector->natural (bvsmod bv1 bv2))
                     (r:bitvector->natural (r:bvsmod r:bv1 r:bv2)))
                  (= (bitvector->integer (bvsmod bv1 bv2))
                     (r:bitvector->integer (r:bvsmod r:bv1 r:bv2)))))))

        (test-case (format "bitvector signed less or equal (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (boolean=? (bvsle bv1 bv2)
                  (r:bvsle r:bv1 r:bv2)))))

        (test-case (format "bitvector unsigned less or equal (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (boolean=? (bvule bv1 bv2)
                  (r:bvule r:bv1 r:bv2)))))

        (test-case (format "bitvector signed less than (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (boolean=? (bvslt bv1 bv2)
                  (r:bvslt r:bv1 r:bv2)))))

        (test-case (format "bitvector unsigned less than (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (boolean=? (bvult bv1 bv2)
                  (r:bvult r:bv1 r:bv2)))))

        (test-case (format "bitvector signed greater or equal (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (boolean=? (bvsge bv1 bv2)
                  (r:bvsge r:bv1 r:bv2)))))


        (test-case (format "bitvector unsigned greater or equal (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (boolean=? (bvuge bv1 bv2)
                  (r:bvuge r:bv1 r:bv2)))))

        (test-case (format "bitvector signed greater than (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (boolean=? (bvsgt bv1 bv2)
                  (r:bvsgt r:bv1 r:bv2)))))


        (test-case (format "bitvector unsigned greater than (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (boolean=? (bvugt bv1 bv2)
                        (r:bvugt r:bv1 r:bv2)))))

        (test-case (format "bitvector equal (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (boolean=? (bveq bv1 bv2)
                        (r:bveq r:bv1 r:bv2)))))

        (test-case (format "bitvector not (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)])
             (define bvec (bv n bit))
             (define r:bvec (r:bv n bit))

             (= (bitvector->natural (bvnot bvec))
                (r:bitvector->natural (r:bvnot r:bvec))))))

        (test-case (format "bitvector and (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (= (bitvector->natural (bvand bv1 bv2))
                (r:bitvector->natural (r:bvand r:bv1 r:bv2))))))

        (test-case (format "bitvector or (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (= (bitvector->natural (bvor bv1 bv2))
                (r:bitvector->natural (r:bvor r:bv1 r:bv2))))))

        (test-case (format "bitvector xor (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-for-bits bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (= (bitvector->natural (bvxor bv1 bv2))
                (r:bitvector->natural (r:bvxor r:bv1 r:bv2))))))

        (test-case (format "bitvector shift left (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-between 0 bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (= (bitvector->natural (bvshl bv1 bv2))
                (r:bitvector->natural (r:bvshl r:bv1 r:bv2))))))

        (test-case (format "bitvector logical shift right (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-between 0 bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (= (bitvector->natural (bvlshr bv1 bv2))
                (r:bitvector->natural (r:bvlshr r:bv1 r:bv2))))))

        (test-case (format "bitvector arithmetic shift right (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [m (arbitrary-between 0 bit)])
             (define bv1 (bv n bit))
             (define bv2 (bv m bit))
             (define r:bv1 (r:bv n bit))
             (define r:bv2 (r:bv m bit))

             (= (bitvector->natural (bvashr bv1 bv2))
                (r:bitvector->natural (r:bvashr r:bv1 r:bv2))))))

        (test-case "bitvector extraction"
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [i (arbitrary-between 0 (sub1 bit))])
             (property ([j (arbitrary-between 0 i)])
               (define bvec (bv n bit))
               (define r:bvec (r:bv n bit))
               (= (bitvector->natural (extract i j bvec))
                  (r:bitvector->natural (r:extract i j r:bvec)))))))

        (test-case (format "bitvector zero extension (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-natural-for-bits bit)]
                      [e (arbitrary-between bit 1024)])

             (define bvec (bv n bit))
             (define bvec-ext (make-bitvector e))
             (define r:bvec (r:bv n bit))
             (define r:bvec-ext (r:bitvector e))

             (= n
                (bitvector->natural (zero-extend bvec bvec-ext))
                (r:bitvector->natural (r:zero-extend r:bvec r:bvec-ext))))))

        (test-case (format "bitvector sign extension (bit: ~a)" bit)
          (check-property
           (property ([n (arbitrary-for-bits bit)]
                      [e (arbitrary-between bit 1024)])

             (define bvec (bv n bit))
             (define bvec-ext (make-bitvector e))
             (define r:bvec (r:bv n bit))
             (define r:bvec-ext (r:bitvector e))

             (= n
                (bitvector->integer (sign-extend bvec bvec-ext))
                (r:bitvector->integer (r:sign-extend r:bvec r:bvec-ext))))))))

    (test-case "bitvector concatenation"
      (check-property
       (property ([l (arbitrary-list arbitrary-bitvector-pair)])
         (let ([fixed-list (filter (lambda (p) (> (cdr p) 0)) l)])
           (or (null? fixed-list)
               (let ([bv-list (map (lambda (p) (bv (car p) (cdr p))) fixed-list)]
                     (rbv-list (map (lambda (p) (r:bv (car p) (cdr p))) fixed-list)))
                 (= (bitvector->natural (apply concat bv-list))
                    (r:bitvector->natural (apply r:concat rbv-list))))))))))))
