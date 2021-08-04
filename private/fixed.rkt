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
   [fxshl (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx? fx?))]
   [fxshr (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx? fx?))]

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; arithmetic overflow behavior { 'wrap 'saturate 'nan }
(define *overflow-mode* (make-parameter 'nan))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (limits sign? nbits scale)
  (let ([2scale (expt 2 scale)])
    (if sign?
        (values (- (* 2scale (expt 2 (- nbits 1))))
                (* 2scale (- (expt 2 (- nbits 1)) 1)))
        (values 0
                (* 2scale (- (expt 2 nbits) 1))))))

;;; (define ((check-bounds! sign? nbits scale) x)
;;;   (define-values (lo hi) (limits sign? nbits scale))
;;;   (define lo* (/ lo (expt 2 scale)))
;;;   (define hi* (/ hi (expt 2 scale)))
;;;   (unless (or (<= lo* x hi*) (nan? x))
;;;     (error 'check-bounds! "Not in range: ~a\n" x)))

(define ((normalize sign? nbits scale) x)
  (cond
   [(nan? x) x]
   [(equal? (*overflow-mode*) 'wrap)
    (define x* (inexact->exact (truncate x)))
    (define shift (expt 2 (- nbits 1)))
    (- (modulo (+ x* shift) (expt 2 nbits)) shift)]
   [(equal? (*overflow-mode*) 'saturate)
    (define-values (lo hi) (limits sign? nbits scale))
    (max (min (inexact->exact (truncate x)) hi) lo)]
   [else   ;  (equal? (*overflow-mode*) 'nan)
    (define-values (lo hi) (limits sign? nbits scale))
    (let ([lo (/ lo (expt 2 scale))]
          [hi (/ hi (expt 2 scale))]
          [x (inexact->exact (truncate x))])
      (if (<= lo x hi) x +nan.0))]))

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

(define ((real->fx sign? nbits scale) x)
  (define-values (lo hi) (limits sign? nbits scale))
  (cond
   [(nan? x) x]
   [(>= x hi) (/ hi (expt 2 scale))]
   [(<= x lo) (/ lo (expt 2 scale))]
   [else (inexact->exact (truncate (/ x (expt 2 scale))))]))

(define ((fx->ordinal sign? nbits scale) x)
  (cond
   [(nan? x)  (expt 2 nbits)]
   [sign? (+ x (expt 2 (- nbits 1)))]
   [else x]))

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
  (define fx (real->fx sign? nbits scale))
  (cond
   [(bfinfinite? x) (if (bfnegative? x) (fx -inf.0) (fx +inf.0))]
   [(bfnan? x) (fx +nan.0)]
   [(negative? scale)
    (define x* (bf/ x (bf (expt 2 scale))))
    (fx (* (bigfloat->integer (bftruncate x*)) (expt 2 scale)))]
   [else
    (fx (bigfloat->integer (bftruncate x)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Arithmetic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((fx2+ sign? nbits scale) x y)
  ((normalize sign? nbits scale) (+ x y)))

(define ((fx2- sign? nbits scale) x y)
  ((normalize sign? nbits scale) (- x y)))

(define ((fx2* sign? nbits scale) x y)
  ((normalize sign? nbits scale) (* x y (expt 2 scale))))

(define ((fx2/ sign? nbits scale) x y)
  (cond
   [(zero? y) +nan.0]
   [else ((normalize sign? nbits scale) (/ x y (expt 2 scale)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Bitwise ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((fxnot sign? nbits scale) x)
  (cond
   [(nan? x) +nan.0]
   [else
    (let ([x* (bitwise-not x)])
      (if (and (not sign?) (negative? x*))
          (+ x* (expt 2 nbits))
          x*))]))

(define ((fxand sign? nbits scale) x y)
  (cond
   [(or (nan? x) (nan? y)) +nan.0]
   [else (bitwise-and x y)]))

(define ((fxor sign? nbits scale) x y)
  (cond
   [(or (nan? x) (nan? y)) +nan.0]
   [else (bitwise-ior x y)]))

(define ((fxxor sign? nbits scale) x y)
  (cond
   [(or (nan? x) (nan? y)) +nan.0]
   [else (bitwise-xor x y)]))

(define ((fxshl sign? nbits scale) x y)
  (cond
   [(or (nan? x) (nan? y)) +nan.0]
   [(negative? y) ((fxshr sign? nbits scale) x (- y))]
   [sign?
    (define x* (abs x))
    (define s (arithmetic-shift (bitwise-bit-field x* 0 nbits) y))
    (define r (bitwise-bit-field s 0 31))
    (if (negative? x) (- r) r)]
   [else
    (define s (arithmetic-shift (bitwise-bit-field x 0 nbits) y))
    (bitwise-bit-field s 0 31)]))

(define ((fxshr sign? nbits scale) x y)
  (cond
   [(or (nan? x) (nan? y)) +nan.0]
   [(negative? y) ((fxshl sign? nbits scale) x (- y))]
   [sign?
    (define x* (abs x))
    (define s (arithmetic-shift (bitwise-bit-field x* 0 nbits) (- y)))
    (define r (bitwise-bit-field s 0 31))
    (if (negative? x) (- r) r)]
   [else
    (define s (arithmetic-shift (bitwise-bit-field x 0 nbits) (- y)))
    (bitwise-bit-field s 0 31)]))

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

(define ((fxpow sign? nbits scale) x y)
  (define x* ((fx->real sign? nbits scale) x))
  (define y* ((fx->real sign? nbits scale) y))
  (cond
   [(or (nan? x*) (nan? y*)) +nan.0]
   [(zero? x*)    ; special case: 0^y
    (cond
     [(negative? y*) +nan.0]
     [(zero? y*) ((real->fx sign? nbits scale) 1)]
     [else ((real->fx sign? nbits scale) 0)])]
   [else
    (define b (inexact->exact (ceiling (log (abs x*) 2))))
    ((real->fx sign? nbits scale)
      (cond
       [(and (negative? y*) (> (abs y*) (abs scale))) 0]      ; underflow to 0
       [(> (* b y*) nbits)
        (cond
         [(and (negative? x*) (not (integer? y*)))  +nan.0]   ; complex root
         [(and (negative? x*) (odd? y*)) -inf.0]              ; overflow to inf
         [else +inf.0])]
       [else (no-complex (expt x* y*))]))]))

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

  (check-equal? ((ordinal->fx #f 16 0) 0) 0)
  (check-equal? ((ordinal->fx #f 16 4) 0) 0)
  (check-equal? ((ordinal->fx #f 32 2) 100) 100)

  ; fx->ordinal
  (check-equal? ((fx->ordinal #t 16 0) 0) 32768)
  (check-equal? ((fx->ordinal #t 16 4) 0) 32768)
  (check-equal? ((fx->ordinal #t 32 2) 100) 2147483748)

  (check-equal? ((fx->ordinal #f 16 0) 0) 0)
  (check-equal? ((fx->ordinal #f 16 4) 0) 0)
  (check-equal? ((fx->ordinal #f 32 2) 100) 100)

  ; bigfloat->fx
  (check-equal? ((bigfloat->fx #t 16 0) (bf 16)) 16)
  (check-equal? ((bigfloat->fx #t 32 0) (bf 32)) 32)
  (check-equal? ((bigfloat->fx #t 32 2) (bf 16)) 4)
  (check-equal? ((bigfloat->fx #t 32 4) (bf 16)) 1)

  ; fx->bigfloat
  (check-equal? ((fx->bigfloat #t 16 0) 16) (bf 16))
  (check-equal? ((fx->bigfloat #t 32 0) 32) (bf 32))
  (check-equal? ((fx->bigfloat #t 32 2) 4) (bf 16))
  (check-equal? ((fx->bigfloat #t 32 4) 1) (bf 16))

  ; fx2+
  (check-equal? ((fx2+ #t 16 0) 16 32) 48)
  (check-equal? ((fx2+ #t 16 0) -16 32) 16)
  (check-equal? ((fx2+ #t 16 2) 16 32) 48)
  (check-equal? ((fx2+ #t 16 4) -16 32) 16)

  (check-equal? ((fx2+ #f 16 0) 16 32) 48)
  (check-equal? ((fx2+ #f 16 2) 16 32) 48)

  ; fx2-
  (check-equal? ((fx2- #t 16 0) 16 32) -16)
  (check-equal? ((fx2- #t 16 0) -16 32) -48)
  (check-equal? ((fx2- #t 16 2) 16 32) -16)
  (check-equal? ((fx2- #t 16 4) -16 32) -48)

  (check-equal? ((fx2- #f 16 0) 32 16) 16)

  ; fx2*
  (check-equal? ((fx2* #t 16 0) 16 32) 512)
  (check-equal? ((fx2* #t 16 0) -16 32) -512)
  (check-equal? ((fx2* #t 16 2) 16 32) 2048)
  (check-equal? ((fx2* #t 16 4) -16 32) -8192)

  (check-equal? ((fx2* #f 16 0) 16 32) 512)
  (check-equal? ((fx2* #f 16 2) 16 32) 2048)

  ; fx2/
  (check-equal? ((fx2/ #t 16 0) 32 4) 8)
  (check-equal? ((fx2/ #t 16 0) -32 8) -4)
  (check-equal? ((fx2/ #t 16 2) 32 4) 2)
  (check-equal? ((fx2/ #t 16 4) -32 8) 0)

  (check-equal? ((fx2/ #f 16 0) 32 4) 8)
  (check-equal? ((fx2/ #f 16 2) 32 4) 2)

  ; shl
  (check-equal? ((fxshl #t 16 0) 10 4) 160)
  (check-equal? ((fxshl #t 16 0) -10 4) -160)

  ; shr
  (check-equal? ((fxshr #t 16 0) 160 4) 10)
  (check-equal? ((fxshr #t 16 0) -160 4) -10)

)



