#lang scribble/manual

@(require (for-label bv racket/base)
          bv
          scribble/core scribble/example scribble/html-properties racket/sandbox)

@title{Bitvectors}
@author[(author+email "Paulo Matos" "pmatos@linki.tools")]

@(define bveval
   (parameterize ([print-as-expression #f]
                  [sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket/base #:requires (list 'bv))))

@defmodule[bv]

This module is inspired by the bitvector interface exposed by Rosette.
Functions with the same name will behave the same.

Bitvectors are constant-sized vectors of bits and the provided interface extends the one defined in the @hyperlink["https://docs.racket-lang.org/rosette-guide/sec_bitvectors.html"]{Rosette Bitvector Guide}.

For example, all of the non-symbolic examples in Rosette's Guide work as expected:

@examples[#:eval bveval
(code:line (bv 4 (bitvector 7))        (code:comment "a bitvector literal of size 7"))
(code:line (bv 4 7)                    (code:comment "a shorthand for the same literal"))
(code:line (bvslt (bv 4 7) (bv -1 7))  (code:comment "signed 7-bit < comparison of 4 and -1"))
(code:line (bvult (bv 4 7) (bv -1 7))  (code:comment "unsigned 7-bit < comparison of 4 and -1"))
]

@section{Constructing Bitvectors}

To construct bitvectors you can use the function @racket[bv]. 

@defproc[(bv [val (and/c integer? (not/c term?) (not/c union?))]
             [size (and/c (or/c bitvector? (and/c integer? positive?))
                          (not/c term?) (not/c union?))]) bv?]{
  Returns a bitvector literal of the given @racket[size], which may be given either as a
  concrete @racket[bitvector?] type or a concrete positive integer.}

@defproc[(bitvector [size (and/c integer? positive? (not/c term?) (not/c union?))]) bitvector?]{
  Returns a type predicate that recognizes bitvectors of the given @racket[size].
  Note that @racket[size] must be a concrete positive integer.                                                        
  The type predicate itself is recognized by the @racket[bitvector?] predicate.
  @examples[#:eval bveval
   (define bv6? (bitvector 6))
   (bv6? 1)
   (bv6? (bv 3 6))
   (bv6? (bv 3 5))]
}

@defproc[(bitvector? [v any/c]) boolean?]{
  Returns true if @racket[v] is a concrete type predicate that recognizes bitvector values.
   @examples[#:eval bveval
   (define bv6? (bitvector 6))
   (define bv7? (bitvector 7))
   (code:line (bitvector? bv6?) (code:comment "a concrete bitvector type"))
   (code:line (bitvector? integer?) (code:comment "not a bitvector type"))
   (code:line (bitvector? 3) (code:comment "not a type"))]}

@defproc[(bv? [v any/c]) boolean?]{
  Recognizes concrete or symbolic bitvector values of any size.
  @examples[#:eval bveval
   (bv? 1)
   (bv? (bv 1 1))
   (bv? (bv 2 2))]
}

@section{Comparison Operators}

@defproc*[([(bveq [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvslt [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvult [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvsle [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvule [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvsgt [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvugt [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvsge [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvuge [x (bitvector n)] [y (bitvector n)]) boolean?])]{
Compares two bitvector values of the same bitvector type. 
Comparison relations include 
equality (@racket[bveq]) and signed / unsigned versions of
<, <=, >, and >= (@racket[bvslt], @racket[bvult], @racket[bvsle], @racket[bvule],
@racket[bvsgt], and @racket[bvugt]).
@examples[#:eval bveval
(code:line (bvslt (bv 4 7) (bv -1 7))  (code:comment "signed 7-bit < comparison of 4 and -1"))
(code:line (bvult (bv 4 7) (bv -1 7))  (code:comment "unsigned 7-bit < comparison of 4 and -1"))]
}

@section{Bitwise Operators}

@defproc[(bvnot [x (bitvector n)]) (bitvector n)]{
 Returns the bitwise negation of the given bitvector value.
 @examples[#:eval bveval
 (bvnot (bv -1 4))
 (bvnot (bv 0 4))]
 }

@section{Arithmetic Operators}

@defproc[(bvneg [x (bitvector n)]) (bitvector n)]{
 Returns the arithmetic negation of the given bitvector value.
 @examples[#:eval bveval
 (bvneg (bv -1 4))
 (bvneg (bv 0 4))]
}

@defproc*[([(bvadd [x (bitvector n)] ...+) (bitvector n)]
           [(bvsub [x (bitvector n)] ...+) (bitvector n)]
           [(bvmul [x (bitvector n)] ...+) (bitvector n)])]{
Returns the sum, difference, or product of one or more bitvector values of the same type.
 @examples[#:eval bveval
 (bvadd (bv -1 4) (bv 2 4))
 (bvsub (bv 0 3)  (bv 1 3))
 (bvmul (bv -1 5) (bv 1 5))]
}

@defproc*[([(bvsdiv [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvudiv [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvsrem [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvurem [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvsmod [x (bitvector n)] [y (bitvector n)]) (bitvector n)])]{
Returns (un)signed quotient, remainder, or modulo of two bitvector values of the same type.
@bold{Exception:} None of these operations is valid for a zero denominator.
 @examples[#:eval bveval
 (bvsdiv (bv -3 4) (bv 2 4))
 (bvudiv (bv -3 3) (bv 2 3))]
}

@section{Conversion Operators}

@defproc[(concat [x bv?] ...+) bv?]{
 Returns the bitwise concatenation of the given bitvector values.
 @examples[#:eval bveval
 (concat (bv -1 4) (bv 0 1) (bv -1 3))]
}

@defproc[(extract [i integer?] [j integer?] [x (bitvector n)]) (bitvector (+ 1 (- i j)))]{
 Extracts bits @racket[i] down to @racket[j] from a bitvector of size @racket[n], yielding a
 bitvector of size i - j + 1.  This procedure assumes that @racket[n] > @racket[i] >= @racket[j] >= 0.
 @examples[#:eval bveval
 (extract 2 1 (bv -1 4))
 (extract 3 3 (bv 1 4))]
}

@defproc*[([(sign-extend [x bv?] [t (or/c bitvector? union?)]) bv?]
           [(zero-extend [x bv?] [t (or/c bitvector? union?)]) bv?])]{
Returns a bitvector of type @racket[t] that represents the (un)signed
extension of the bitvector @racket[x].
Note that both @racket[x] and @racket[t] may be symbolic. The size of @racket[t]
must not be smaller than the size of @racket[x]'s type.
 @examples[#:eval bveval
 (sign-extend (bv -3 4) (bitvector 6))
 (zero-extend (bv -3 4) (bitvector 6))]
}

@defproc*[([(bitvector->integer [x bv?]) integer?]
           [(bitvector->natural [x bv?]) integer?])]{
Returns the (un)signed integer value of the given bitvector.
 @examples[#:eval bveval
 (bitvector->integer (bv -1 4))
 (bitvector->natural (bv -1 4))]
}


@defproc*[([(integer->bitvector [i integer?] [t (or/c bitvector? union?)]) bv?])]{
Returns a bitvector of type @racket[t] that represents the @var[k] lowest order bits
of the integer @racket[i], where @var[k] is the size of @racket[t].              
Note that both @racket[i] and @racket[t] may be symbolic.  
 @examples[#:eval bveval
 (integer->bitvector 4 (bitvector 2))
 (integer->bitvector 15 (bitvector 4))]
}

@section{Additional Operators}

@defproc[(bit [i integer?] [x (bitvector n)]) (bitvector 1)]{
 Extracts the @racket[i]th bit from the bitvector @racket[x] of size @racket[n], yielding a
 bitvector of size 1.  This procedure assumes that @racket[n] > @racket[i] >= 0.
 @examples[#:eval bveval
 (bit 1 (bv 3 4))
 (bit 2 (bv 1 4))]
}

@; @defproc*[([(lsb [x (bitvector n)]) (bitvector 1)]
@;            [(msb [x (bitvector n)]) (bitvector 1)])]{
@;  Returns the least or most significant bit of @racket[x].
@;  @examples[#:eval bveval
@;   (lsb (bv 3 4))
@;   (msb (bv 3 4))]
@; }

@defproc[(bvzero? [x (bitvector n)]) boolean?]{
Returns @racket[(bveq x (bv 0 n))].
 @examples[#:eval bveval
 (bvzero? (bv 0 32))
 (bvzero? (bv 2 8))
]
}

@; @defproc*[([(bvadd1 [x (bitvector n)]) (bitvector n)]
@;            [(bvsub1 [x (bitvector n)]) (bitvector n)])]{
@;  Returns @racket[(bvadd x (bv 1 n))] or @racket[(bvsub x (bv 1 n))]. 
@;  @examples[#:eval bveval
@;  (bvadd1 (bv 10 32))
@;  (bvadd1 (bv 31 5))
@;  (bvsub1 (bv 0 8))
@;  (bvsub1 (bv 10 32))
@; ]
@; }

@; @defproc*[([(bvsmin [x (bitvector n)] ...+) (bitvector n)]
@;            [(bvumin [x (bitvector n)] ...+) (bitvector n)]
@;            [(bvsmax [x (bitvector n)] ...+) (bitvector n)]
@;            [(bvumax [x (bitvector n)] ...+) (bitvector n)])]{
@; Returns the (un)signed minimum or maximum of one or more bitvector values of the same type.
@;  @examples[#:eval bveval
@;  (bvsmin (bv -1 4) (bv 2 4))
@;  (bvumin (bv -1 4) (bv 2 4))
@;  (bvsmax (bv -1 4) (bv 2 4))
@;  (bvumax (bv -1 4) (bv 2 4))
@; ]
@; }

@; @defproc*[([(bvrol [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
@;            [(bvror [x (bitvector n)] [y (bitvector n)]) (bitvector n)])]{
@; Returns the left or right rotation of @racket[x] by @racket[(bvurem y n)] bits, where
@; @racket[x] and @racket[y] are bitvector values of the same type. 
@;  @examples[#:eval bveval
@;  (bvrol (bv 3 4) (bv 2 4))
@;  (bvrol (bv 3 4) (bv -2 4))
@; ]
@; }

@; @defproc*[([(rotate-left  [i integer?] [x (bitvector n)]) (bitvector n)]
@;            [(rotate-right [i integer?] [x (bitvector n)]) (bitvector n)])]{
@; Returns the left or right rotation of @racket[x] by @racket[i] bits.
@; These procedures assume that @racket[n] > @racket[i] >= 0. See @racket[bvrol]
@; and @racket[bvror] for an alternative way to perform rotations that usually
@; leads to faster solving times.
@;  @examples[#:eval bveval
@;  (rotate-left 3 (bv 3 4))
@;  (rotate-right 1 (bv 3 4))
@;  ]
@; }

@; @defproc[(bitvector->bits [x (bitvector n)]) (listof (bitvector n))]{
@; Returns the bits of @racket[x] as a list, i.e., @racket[(list (bit 0 x) ... (bit (- n 1) x))].
@;  @examples[#:eval bveval
@;   (bitvector->bits (bv 3 4))
@;  ]
@; }

@; @defproc[(bitvector->bool [x (bitvector n)]) boolean?]{
@; Returns @racket[(not (bvzero? x))].
@; }

@; @defproc[(bool->bitvector [b any/c] [t (or/c positive-integer? (bitvector n)) (bitvector 1)]) bv?]{
@; Returns @racket[(bv 0 t)] if @racket[(false? b)] and otherwise returns @racket[(bv 1 t)], where
@; @racket[t] is @racket[(bitvector 1)] by default. If provided, @racket[t] must be a concrete positive
@; integer or a concrete bitvector type value.
@;  @examples[#:eval bveval
@;   (bool->bitvector #f 3)
@;   (bool->bitvector "non-false-value")
@;   (define-symbolic b boolean?)
@;   (bool->bitvector b)
@;  ]
@; }

@section{Extensions}

The following operators are an extension to what Rosette provides.

@defproc[(random-bv [n exact-positive-integer?]) bv?]{
Returns a random bitvector, of type @racket[(bitvector (* 8 n))], between @racket[0] and @racket[(- (expt 2 (* 8 n)) 1)] (inclusive).
}

@defproc[(random-bv/limits [n exact-positive-integer?]
                           [min exact-nonnegative-integer?]
                           [max exact-nonnegative-integer?]) bv?]{
Returns random bitvector, of type @racket[(bitvector (* 8 n))], between @var[min] and @var[max] (inclusive).
}
