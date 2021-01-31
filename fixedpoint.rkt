#lang racket

(require math/bigfloat)
(provide (contract-out
          [fx->real (-> exact-nonnegative-integer? exact-nonnegative-integer? exact-integer? real?)]
          [real->fx (-> exact-nonnegative-integer? exact-nonnegative-integer? real? exact-integer?)]
          [fx->ordinal (-> exact-nonnegative-integer? exact-nonnegative-integer? exact-integer? exact-integer?)]
          [ordinal->fx (-> exact-nonnegative-integer? exact-nonnegative-integer? exact-integer? exact-integer?)]
          [fx->bigfloat (-> exact-nonnegative-integer? exact-nonnegative-integer? exact-integer? bigfloat?)]
          [bigfloat->fx (-> exact-nonnegative-integer? exact-nonnegative-integer? bigfloat? exact-integer?)]
          [fx+ (-> exact-nonnegative-integer? exact-nonnegative-integer?
                   (->* (exact-integer?) #:rest (listof exact-integer?) exact-integer?))]
          [fx- (-> exact-nonnegative-integer? exact-nonnegative-integer?
                   (->* (exact-integer?) #:rest (listof exact-integer?) exact-integer?))]
          [fx* (-> exact-nonnegative-integer? exact-nonnegative-integer?
                   (->* (exact-integer?) #:rest (listof exact-integer?) exact-integer?))]
          [fx/ (-> exact-nonnegative-integer? exact-nonnegative-integer?
                   (->* (exact-integer?) #:rest (listof exact-integer?) exact-integer?))]
          [fxshl (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> exact-integer? exact-integer? exact-integer?))]
          [fxshr (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> exact-integer? exact-integer? exact-integer?))]

          [fxsqrt (-> exact-nonnegative-integer? exact-nonnegative-integer?
                      (-> exact-integer? exact-integer?))]
          [fxcbrt (-> exact-nonnegative-integer? exact-nonnegative-integer?
                      (-> exact-integer? exact-integer?))]
          [fxexp (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> exact-integer? exact-integer?))]
          [fxlog (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> exact-integer? exact-integer?))]
          [fxpow (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> exact-integer? exact-integer? exact-integer?))]

          [fxsin (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> exact-integer? exact-integer?))]
          [fxcos (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> exact-integer? exact-integer?))]
          [fxtan (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> exact-integer? exact-integer?))]
          [fxasin (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> exact-integer? exact-integer?))]
          [fxacos (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> exact-integer? exact-integer?))]
          [fxatan (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> exact-integer? exact-integer?))]
          [fxatan2 (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> exact-integer? exact-integer? exact-integer?))]))

(module+ test
  (require rackunit))

;; Normalization

(define (normalize-fx int frac x)
  (define x* (inexact->exact (truncate x)))
  (define bits (+ int frac 1))
  (define shift (expt 2 (sub1 bits)))
  (- (modulo (+ x* shift) (expt 2 bits)) shift))

(define (clamp-fx int frac x)
  (define v (expt 2 (+ int frac))) ; bits - 1
  (cond
   [(> x (sub1 v)) (sub1 v)]
   [(< x (- v)) (- v)]
   [else x]))

;; Conversions

(define (fx->real int frac x)
  (if (= x (- (expt 2 (+ int frac))))  ; min value
      (- (expt 2 int))
      (let* ([f (bitwise-bit-field (abs x) 0 frac)]
             [i (bitwise-bit-field (abs x) frac (+ frac int))]
             [m (+ i (* f (expt 2 (- frac))))])
        (if (negative? x) (- m) m))))

(define (real->fx int frac x)
  (cond
   [(nan? x) 0]
   [(infinite? x)
    (if (negative? x)
        (sub1 (expt 2 (+ int frac))) ; max
        (- (expt 2 (+ int frac))))] ; min
   [else
    (let ([v (inexact->exact (truncate (* x (expt 2 frac))))])
      (clamp-fx int frac v))]))

(define (fx->ordinal int frac x)  ; bits - 1
  (+ x (expt 2 (+ int frac))))

(define (ordinal->fx int frac x)  ; bits - 1
  (- x (expt 2 (+ int frac))))

(define (bigfloat->fx int frac x)
  (real->fx int frac (bigfloat->real x)))

(define (fx->bigfloat int frac x)
  (bf (fx->real int frac x)))

;; Helper functions

(define (fx-1ary f int frac)
  (λ (x) (normalize-fx int frac (f x))))

(define (fx-2ary f int frac)
  (λ (x y) (normalize-fx int frac (f x y))))

(define (fx-vary f int frac id)
  (λ args
    (normalize-fx int frac
      (let loop ([args (reverse args)])
        (match args
          [(list) (error 'fx-vary "~a expects at least 1 argument" f)]
          [(list head) (f int frac (real->fx int frac id) head)]
          [(list head tail) (f int frac tail head)]
          [(list head rest ...) (f int frac (loop rest) head)])))))

(define (fx-real-op f int frac)
  (λ args 
    (let ([res (apply f (map (curry fx->real int frac) args))])
      (if (real? res)
          (real->fx int frac res)
          0))))

;; Operations

(define (fx+-2ary int frac x y)
  (+ x y))

(define (fx--2ary int frac x y)
  (- x y))

(define (fx*-2ary int frac x y)
  (arithmetic-shift (* x y) (- frac)))

(define (fx/-2ary int frac x y)
  (if (zero? y) 0 (real->fx int frac (/ x y))))

; No-except version
(define (expt-safe x y)
  (cond
   [(and (zero? x) (negative? y))  +inf.0]
   [else (expt x y)])) 

; No-except version
(define (log-safe x)
  (if (positive? x)
      (log x)
      +nan.0)) 

(define (atan2 y x)
  (let ([r (atan (/ y x))])
    (cond
     [(negative? x) (+ r pi)]
     [else          r])))

;; Exported ops

(define (fx+ int frac) (fx-vary fx+-2ary int frac 0))
(define (fx- int frac) (fx-vary fx--2ary int frac 0))
(define (fx* int frac) (fx-vary fx*-2ary int frac 1))
(define (fx/ int frac) (fx-vary fx/-2ary int frac 1))

(define (fxsqrt int frac) (fx-1ary (fx-real-op sqrt int frac) int frac))
(define (fxcbrt int frac) (fx-1ary (fx-real-op (curryr expt 1/3) int frac) int frac))
(define (fxexp int frac) (fx-1ary (fx-real-op exp int frac) int frac))
(define (fxlog int frac) (fx-1ary (fx-real-op log-safe int frac) int frac))
(define (fxpow int frac) (fx-2ary (fx-real-op expt-safe int frac) int frac))

(define (fxsin int frac) (fx-1ary (fx-real-op sin int frac) int frac))
(define (fxcos int frac) (fx-1ary (fx-real-op cos int frac) int frac))
(define (fxtan int frac) (fx-1ary (fx-real-op tan int frac) int frac))

(define (fxasin int frac) (fx-1ary (fx-real-op asin int frac) int frac))
(define (fxacos int frac) (fx-1ary (fx-real-op acos int frac) int frac))
(define (fxatan int frac) (fx-1ary (fx-real-op atan int frac) int frac))
(define (fxatan2 int frac) (fx-2ary (fx-real-op atan2 int frac) int frac))

(define (fxshl int frac)
  (λ (x shift) (normalize-fx int frac (arithmetic-shift x shift))))

(define (fxshr int frac)
  (λ (x shift) (normalize-fx int frac (arithmetic-shift x (- shift)))))

(module+ test
  (define int 20)
  (define frac 20)
  (define err 0.01)
 
  (define vals '(-100 -10 -1 -0.01 0 0.01 1 10 100))
  (define vals2 (cartesian-product vals vals))

  (define ops (list fx+ fx- fx* fx/))
  (define bfops (list + - * /))

  (for ([v vals])
    (define v* (fx->real int frac (real->fx int frac v)))
    (define max (+ v (abs (* v err))))
    (define min (- v (abs (* v err))))
    (check-true (<= min v* max) (format "fx<=>real: ~a != ~a" v v*)))

  (for ([v vals])
    (define x (real->fx int frac v))
    (define x* (ordinal->fx int frac (fx->ordinal int frac x)))
    (check-equal? x x* (format "fx<=>ordinal: ~a != ~a for ~a" x x* v)))

  (for ([v vals])
    (define x (real->fx int frac v))
    (define x* (bigfloat->fx int frac (fx->bigfloat int frac x)))
    (check-equal? x x* (format "fx<=>bigfloat: ~a != ~a for ~a" x x* v)))

  (for ([op ops] [bfop bfops])
    (for ([val vals2])
      (match-define (list x y) val)
      (define x* (real->fx int frac x))
      (define y* (real->fx int frac y))
      (define r (fx->real int frac ((op int frac) x* y*)))
      (define br 
        (with-handlers ([exn:fail? (const 0)])
          (bfop x y)))
      (define max (+ br (abs (* br err))))
      (define min (- br (abs (* br err))))
      (check-true
        (<= min r max)
        (format "For (~a ~a ~a): ~a < ~a < ~a not true" 
                bfop x y (exact->inexact min)
                (exact->inexact r) (exact->inexact max)))))
)