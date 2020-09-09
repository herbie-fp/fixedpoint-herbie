#lang racket

(require math/bigfloat)
(provide fx->real real->fx fx->ordinal ordinal->fx
         fx->bigfloat bigfloat->fx
         fx+ fx- fx* fx/ fxshl fxshr
         fxsqrt fxcbrt fxexp fxlog fxpow)

(module+ test
  (require rackunit))

;; Normalization

(define (normalize-fx x int frac)
  (define x* (inexact->exact (truncate x)))
  (define bits (+ int frac 1))
  (define shift (expt 2 (sub1 bits)))
  (- (modulo (+ x* shift) (expt 2 bits)) shift))

(define (clamp-fx x int frac)
  (define v (expt 2 (+ int frac))) ; bits - 1
  (cond
   [(> x (sub1 v)) (sub1 v)]
   [(< x (- v)) (- v)]
   [else x]))

;; Conversions

(define/contract (fx->real x int frac)
  (-> exact-integer? exact-positive-integer? exact-positive-integer? real?)
  (if (= x (- (expt 2 (+ int frac))))  ; min value
      (- (expt 2 int))
      (let* ([f (bitwise-bit-field (abs x) 0 frac)]
             [i (bitwise-bit-field (abs x) frac (+ frac int))]
             [m (+ i (* f (expt 2 (- frac))))])
        (if (negative? x) (- m) m))))

(define/contract (real->fx x int frac)
  (-> real? exact-positive-integer? exact-positive-integer? exact-integer?)
  (match x
    [+nan.0 0]
    [+inf.0 (sub1 (expt 2 (+ int frac)))] ; max
    [-inf.0 (- (expt 2 (+ int frac)))] ; min
    [_ (clamp-fx (inexact->exact (round (* x (expt 2 frac)))) int frac)]))

(define/contract (fx->ordinal x int frac)  ; bits - 1
  (-> exact-integer? exact-positive-integer? exact-positive-integer? exact-integer?)
  (+ x (expt 2 (+ int frac))))

(define/contract (ordinal->fx x int frac)  ; bits - 1
  (-> exact-integer? exact-positive-integer? exact-positive-integer? exact-integer?)
  (- x (expt 2 (+ int frac))))

(define/contract (bigfloat->fx x int frac)
  (-> bigfloat? exact-positive-integer? exact-positive-integer? exact-integer?)
  (real->fx (bigfloat->real x) int frac))

(define/contract (fx->bigfloat x int frac)
  (-> exact-integer? exact-positive-integer? exact-positive-integer? bigfloat?)
  (bf (fx->real x int frac)))

;; Helper functions

(define/contract (fx-1ary f)
  (-> (-> exact-integer? exact-integer?)
      (-> exact-positive-integer? exact-positive-integer?
          exact-integer?
          exact-integer?))
  (λ (int frac x) (normalize-fx (f x) int frac)))

(define/contract (fx-2ary f)
  (-> (-> exact-integer? exact-integer? exact-integer?)
      (-> exact-positive-integer? exact-positive-integer?
          exact-integer? exact-integer?
          exact-integer?))
  (λ (int frac x y) (normalize-fx (f x y) int frac)))

(define/contract (fx-vary f id)
  (-> (-> exact-positive-integer? exact-positive-integer?
          real? real?
          real?)
      exact-integer?
      (-> exact-positive-integer? exact-positive-integer?
          exact-integer? exact-integer? ...
          exact-integer?))
  (λ (int frac . args)
    (normalize-fx
      (let loop ([args (reverse args)])
        (match args
          [(list) (error 'fx-vary "~a expects at least 1 argument" f)]
          [(list head) (f int frac (real->fx id int frac) head)]
          [(list head tail) (f int frac tail head)]
          [(list head rest ...) (f int frac (loop rest) head)]))
      int frac)))

(define (fx-real-op f int frac)
  (λ args 
    (let ([res (apply f (map (curryr fx->real int frac) args))])
      (if (real? res)
          (real->fx res int frac)
          0))))

;; Operations

(define (fx+-2ary int frac x y)
  (+ x y))

(define (fx--2ary int frac x y)
  (- x y))

(define (fx*-2ary int frac x y)
  (arithmetic-shift (* x y) (- frac)))

(define (fx/-2ary int frac x y)
  (if (zero? y)
      0
      (real->fx (/ x y) int frac)))

(define (fx+ int frac . args)
  (apply (fx-vary fx+-2ary 0) int frac args))

(define (fx- int frac . args)
  (apply (fx-vary fx--2ary 0) int frac args))

(define (fx* int frac . args)
  (apply (fx-vary fx*-2ary 1) int frac args))

(define (fx/ int frac . args)
  (apply (fx-vary fx/-2ary 1) int frac args))

(define (fxshl int frac x shift)
  ((fx-1ary (curryr arithmetic-shift shift)) int frac x))

(define (fxshr int frac x shift)
  ((fx-1ary (curryr arithmetic-shift (- shift))) int frac x))

(define (fxsqrt int frac x)
  ((fx-1ary (fx-real-op sqrt int frac)) int frac x))

(define (fxcbrt int frac x)
  ((fx-1ary (fx-real-op (curryr expt 1/3) int frac)) int frac x))

(define (fxexp int frac x)
  ((fx-1ary (fx-real-op exp int frac)) int frac x))

(define (fxlog int frac x)
  ((fx-1ary (fx-real-op exp int frac)) int frac x))

; Non-crashing version
(define (expt-safe x y)
  (cond
   [(and (zero? x) (negative? y))  +inf.0]
   [else (expt x y)]))  

(define (fxpow int frac x y)
  ((fx-2ary (fx-real-op expt-safe int frac)) int frac x y))

(module+ test
  (define int 20)
  (define frac 20)
  (define err 0.01)
 
  (define vals '(-100 -10 -1 -0.01 0 0.01 1 10 100))
  (define vals2 (cartesian-product vals vals))

  (define ops (list fx+ fx- fx* fx/))
  (define bfops (list + - * /))

  (for ([v vals])
    (define v* (fx->real (real->fx v int frac) int frac))
    (define max (+ v (abs (* v err))))
    (define min (- v (abs (* v err))))
    (check-true (<= min v* max) (format "fx<=>real: ~a != ~a" v v*)))

  (for ([v vals])
    (define x (real->fx v int frac))
    (define x* (ordinal->fx (fx->ordinal x int frac) int frac))
    (check-equal? x x* (format "fx<=>ordinal: ~a != ~a for ~a" x x* v)))

  (for ([v vals])
    (define x (real->fx v int frac))
    (define x* (bigfloat->fx (fx->bigfloat x int frac) int frac))
    (check-equal? x x* (format "fx<=>bigfloat: ~a != ~a for ~a" x x* v)))

  (for ([op ops] [bfop bfops])
    (for ([val vals2])
      (match-define (list x y) val)
      (define x* (real->fx x int frac))
      (define y* (real->fx y int frac))
      (define r (fx->real (op int frac x* y*) int frac))
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