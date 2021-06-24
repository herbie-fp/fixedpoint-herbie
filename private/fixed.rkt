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
   [bigfloat->fx (-> boolean? fx-bitwidth? fx-scale? (-> bigfloat? fx?))]))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Unit tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (check-equal? ((real->fx #t 16 0) 16) 16)
  (check-equal? ((real->fx #t 32 0) 32) 32)
  (check-equal? ((real->fx #t 32 2) 16) 4)
  (check-equal? ((real->fx #t 32 4) 16) 1))
