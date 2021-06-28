#lang racket

(require math/bigfloat)

(provide
  (contract-out
   [fx? (-> any/c boolean?)]
   [fx->real (-> boolean? fx-bitwidth? fx-scale? (-> fx? real?))]
   [real->fx (-> boolean? fx-bitwidth? fx-scale? (-> real? fx?))]
   [fx->ordinal (-> boolean? fx-bitwidth? fx-scale? (-> fx? ordinal?))]
   [ordinal->fx (-> boolean? fx-bitwidth? fx-scale? (-> ordinal? fx?))]
   [fx->bigfloat (-> boolean? fx-bitwidth? fx-scale? (-> fx? bigfloat?))]
   [bigfloat->fx (-> boolean? fx-bitwidth? fx-scale? (-> bigfloat? fx?))]

   [fx2+ (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx? fx?))]
   [fx2- (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx? fx?))]
   [fx2* (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx? fx?))]
   [fx2/ (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx? fx?))]

   [fxnot (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx?))]
   [fxand (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx? fx?))]
   [fxor (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx? fx?))]
   [fxxor (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx? fx?))]

   [fxsqrt (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx?))]
   [fxcbrt (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx?))]
   [fxexp (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx?))]
   [fxlog (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx?))]
   [fxpow (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx? fx?))]
   [fxsin (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx?))]
   [fxcos (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx?))]
   [fxtan (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx?))]
   [fxasin (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx?))]
   [fxacos (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx?))]
   [fxatan (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx?))]))
   

(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (limits sign? nbits scale)
  (let ([2scale (expt 2 scale)])
    (values (if sign? (- (* 2scale (expt 2 (- nbits 1)))) 0)
            (* 2scale (- (expt 2 (- nbits (if sign? 1 0))) 1)))))

;;; (define ((check-bounds! sign? nbits scale) x)
;;;   (define-values (lo hi) (limits sign? nbits scale))
;;;   (define lo* (/ lo (expt 2 scale)))
;;;   (define hi* (/ hi (expt 2 scale)))
;;;   (unless (or (<= lo* x hi*) (nan? x))
;;;     (error 'check-bounds! "Not in range: ~a\n" x)))

(define ((normalize sign? nbits scale) x)
  (cond
   [(nan? x) x]
   [else
    (define x* (inexact->exact (truncate x)))
    (define shift (expt 2 (- nbits 1)))
    (- (modulo (+ x* shift) (expt 2 nbits)) shift)]))

(define (fx? x)
  (and (number? x) (or (exact-integer? x) (nan? x))))

(define fx-bitwidth? exact-nonnegative-integer?)
(define fx-scale? exact-integer?)
(define ordinal? exact-nonnegative-integer?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Conversions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((fx->real sign? nbits scale) x)
  (cond
   [(nan? x) x]
   [else (* x (expt 2 scale))]))

(define ((real->fx* sign? nbits scale) x)
  (define-values (lo hi) (limits sign? nbits scale))
  (cond
   [(nan? x) x]
   [(>= x hi) (/ hi (expt 2 scale))]
   [(<= x lo) (/ lo (expt 2 scale))]
   [else (inexact->exact (truncate (/ x (expt 2 scale))))]))

(define ((real->fx sign? nbits scale) x)
  ((real->fx* sign? nbits scale) x))

(define ((fx->ordinal sign? nbits scale) x)
  (cond
   [(nan? x)  (expt 2 nbits)]
   [else
    (define-values (lo hi) (limits sign? nbits scale))
    (define lo* (/ lo (expt 2 scale)))
    (define hi* (/ hi (expt 2 scale)))
    (cond   ; TODO: first two should never fire, bug exists
     [(< x lo*) 0]    
     [(> x hi*) (- (expt 2 nbits) 1)]
     [sign? (+ x (expt 2 (- nbits 1)))]
     [else x])]))

(define ((ordinal->fx sign? nbits scale) x)
  (define nan-ord (expt 2 nbits))
  (cond
   [(< x 0)
    (define-values (lo hi) (limits sign? nbits scale))
    (/ lo (expt 2 scale))]
   [(> x nan-ord)
    (define-values (lo hi) (limits sign? nbits scale))
    (/ hi (expt 2 scale))]
   [(= x nan-ord) +nan.0]
   [sign? (- x (expt 2 (- nbits 1)))]
   [else x]))

(define ((fx->bigfloat sign? nbits scale) x)
  (bf ((fx->real sign? nbits scale) x)))

(define ((bigfloat->fx sign? nbits scale) x)
  ((real->fx sign? nbits scale) (bigfloat->real x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Arithmetic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((fx2+ sign? nbits scale) x y)
  ((normalize sign? nbits scale) (+ x y)))

(define ((fx2- sign? nbits scale) x y)
  ((normalize sign? nbits scale) (- x y)))

(define ((fx2* sign? nbits scale) x y)
  ((normalize sign? nbits scale) (* x y (expt 2 scale))))

(define ((fx2/ sign? nbits scale) x y)
  (if (zero? y)
      +nan.0
      ((normalize sign? nbits scale) (/ x y (expt 2 scale)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Bitwise ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((fxnot sign? nbits scale) x)
  (let ([x* (bitwise-not x)])
    (if (and (not sign?) (negative? x*))
        (+ x* (expt 2 nbits))
        x*)))

(define ((fxand sign? nbits scale) x y)
  (bitwise-and x y))

(define ((fxor sign? nbits scale) x y)
  (bitwise-ior x y))

(define ((fxxor sign? nbits scale) x y)
  (bitwise-xor x y))

; First argument is fixed-point
; Second argument is integer
;
;;; (define ((fxshl sign? nbits scale) x y)
;;;   (define mask (- (expt 2 nbits) 1))
;;;   (bitwise-and mask (arithmetic-shift x y)))

;;; (define ((fxshr sign? nbits scale) x y)
;;;   (define mask (- (expt 2 nbits) 1))
;;;   (bitwise-and mask (arithmetic-shift x (- y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Math functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (log/safe x)
  (if (positive? x) (log x) +nan.0))

(define (no-complex x)
  (if (real? x) x +nan.0))


(define ((fx-1ary-op sign? nbits scale f) x)
  (let ([x* ((fx->real sign? nbits scale) x)])
    ((real->fx sign? nbits scale) (no-complex (f x*)))))

(define ((fx-2ary-op sign? nbits scale f) x y)
  (let ([x* ((fx->real sign? nbits scale) x)]
        [y* ((fx->real sign? nbits scale) y)])
    ((real->fx sign? nbits scale) (no-complex (f x* y*)))))

(define-syntax-rule (fx-1ary-ops [fx-op real-op] ...)
  (begin (define fx-op (curryr fx-1ary-op real-op)) ...))

(define-syntax-rule (fx-2ary-ops [fx-op real-op] ...)
  (begin (define fx-op (curryr fx-2ary-op real-op)) ...))

(fx-1ary-ops
 [fxsqrt sqrt]
 [fxcbrt (curryr expt 1/3)]
 [fxexp exp]
 [fxlog log/safe]
 [fxsin sin]
 [fxcos cos]
 [fxtan tan]
 [fxasin asin]
 [fxacos acos]
 [fxatan atan])

(fx-2ary-ops
 [fxpow expt])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Unit tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  ; real->fx
  (check-equal? ((real->fx #t 16 0) 16) 16)
  (check-equal? ((real->fx #t 32 0) 32) 32)
  (check-equal? ((real->fx #t 32 2) 16) 4)
  (check-equal? ((real->fx #t 32 4) 16) 1)

  ; fx->real
  (check-equal? ((fx->real #t 16 0) 16) 16)
  (check-equal? ((fx->real #t 32 0) 32) 32)
  (check-equal? ((fx->real #t 32 2) 4) 16)
  (check-equal? ((fx->real #t 32 4) 1) 16)

  ; ordinal->fx
  (check-equal? ((ordinal->fx #t 16 0) 16) -32752)
  (check-equal? ((ordinal->fx #t 32 0) 32) -2147483616)
  (check-equal? ((ordinal->fx #t 32 2) 16) -2147483632)
  (check-equal? ((ordinal->fx #t 32 4) 16) -2147483632)

  ; fx->ordinal
  (check-equal? ((fx->bigfloat #t 16 0) 16) (bf 16))
  (check-equal? ((fx->bigfloat #t 32 0) 32) (bf 32))
  (check-equal? ((fx->bigfloat #t 32 2) 4) (bf 16))
  (check-equal? ((fx->bigfloat #t 32 4) 1) (bf 16))

  ; bigfloat->fx
  (check-equal? ((bigfloat->fx #t 16 0) (bf 16)) 16)
  (check-equal? ((bigfloat->fx #t 32 0) (bf 32)) 32)
  (check-equal? ((bigfloat->fx #t 32 2) (bf 16)) 4)
  (check-equal? ((bigfloat->fx #t 32 4) (bf 16)) 1)

  ; fx2+
  (check-equal? ((fx2+ #t 16 0) 16 32) 48)
  (check-equal? ((fx2+ #t 16 0) -16 32) 16)
  (check-equal? ((fx2+ #t 16 2) 16 32) 48)
  (check-equal? ((fx2+ #t 16 4) -16 32) 16)

  ; fx2-
  (check-equal? ((fx2- #t 16 0) 16 32) -16)
  (check-equal? ((fx2- #t 16 0) -16 32) -48)
  (check-equal? ((fx2- #t 16 2) 16 32) -16)
  (check-equal? ((fx2- #t 16 4) -16 32) -48)

  ; fx2*
  (check-equal? ((fx2* #t 16 0) 16 32) 512)
  (check-equal? ((fx2* #t 16 0) -16 32) -512)
  (check-equal? ((fx2* #t 16 2) 16 32) 2048)
  (check-equal? ((fx2* #t 16 4) -16 32) -8192)

  ; fx2/
  (check-equal? ((fx2/ #t 16 0) 32 4) 8)
  (check-equal? ((fx2/ #t 16 0) -32 8) -4)
  (check-equal? ((fx2/ #t 16 2) 32 4) 2)
  (check-equal? ((fx2/ #t 16 4) -32 8) 0)

)


