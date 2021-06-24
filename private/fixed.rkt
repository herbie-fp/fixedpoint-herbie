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
   [fx2/ (-> boolean? fx-bitwidth? fx-scale? (-> fx? fx? fx?))]))


(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (limits sign? nbits scale)
  (let ([2scale (expt 2 scale)])
    (values (* 2scale (expt 2 (- nbits (if sign? 1 0))))
            (if sign? (- (* 2scale (expt 2 (- nbits 1)))) 0))))

(define (clamp x lo hi)
  (min hi (max lo x)))

(define (fx? x)
  (and (number? x) (or (exact-integer? x) (nan? x))))

(define fx-bitwidth? exact-nonnegative-integer?)
(define fx-scale? exact-integer?)
(define ordinal? exact-nonnegative-integer?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Conversions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((fx->real sign? nbits scale) x)
  (cond [(nan? x) x]
        [else (* x (expt 2 scale))]))

(define ((real->fx sign? nbits scale) x)
  (define-values (hi lo) (limits sign? nbits scale))
  (cond
   [(nan? x) x]
   [(<= lo x hi) (inexact->exact (round (/ x (expt 2 scale))))]
   [else (clamp x lo hi)]))

(define ((fx->ordinal sign? nbits scale) x)
  (cond
   [(nan? x) (expt 2 nbits)]
   [sign? (+ x (expt 2 (- nbits 1)))]
   [else x]))

(define ((ordinal->fx sign? nbits scale) x)
  (cond
   [(= x (expt 2 nbits)) +nan.0]
   [sign? (- x (expt 2 (- nbits 1)))]
   [else x]))

(define ((fx->bigfloat sign? nbits scale) x)
  (bf ((fx->real sign? nbits scale) x)))

(define ((bigfloat->fx sign? nbits scale) x)
  ((real->fx sign? nbits scale) (bigfloat->real x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Arithmetic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((fx2+ sign? nbits scale) x y)
  (define-values (hi lo) (limits sign? nbits scale))
  (clamp (+ x y) lo hi))

(define ((fx2- sign? nbits scale) x y)
  (define-values (hi lo) (limits sign? nbits scale))
  (clamp (- x y) lo hi))

(define ((fx2* sign? nbits scale) x y)
  (define-values (hi lo) (limits sign? nbits scale))
  (clamp (* x y (expt 2 scale)) lo hi))

(define ((fx2/ sign? nbits scale) x y)
  (define-values (hi lo) (limits sign? nbits scale))
  (clamp (round (/ x y (expt 2 scale))) lo hi))

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



