#lang racket

(provide (all-defined-out))

;; Normalization

(define (normalize-fx x int frac)
  (define bits (+ int frac 1))
  (define shift (expt 2 (sub1 bits)))
  (- (modulo (+ (round x) shift) (expt 2 bits)) shift))

(define (clamp-fx x int frac)
  (define v (expt 2 (+ int frac))) ; bits - 1
  (cond
   [(> x (sub1 v)) (sub1 v)]
   [(< x (- v)) (- v)]
   [else x]))

;; Fixed point conversions

(define (fixed->real x int frac)
  (define s (bitwise-bit-field x (+ frac int) (+ frac int 1)))
  (define x* (if (= s 1) (- x) x))
  (define f (bitwise-bit-field x* 0 frac))
  (define i (bitwise-bit-field x* frac (+ frac int)))
  (define m (+ i (* f (expt 2 (- frac)))))
  (if (= s 1) (- m) m))

(define (real->fixed x int frac)
  (match x
    [(? nan?) 0]
    [_ (round (* x (expt 2 frac)))]))

(define (fx->ordinal x int frac)  ; bits - 1
  (+ x (expt 2 (+ int frac))))

(define (ordinal->fx x int frac)  ; bits - 1
  (- x (expt 2 (+ int frac))))

;; Fixed point operations

(define (fx+ int frac . args)
  (normalize-fx (apply + args) int frac))

(define (fx- int frac . args)
  (normalize-fx (apply - args) int frac))

(define (fx* int frac . args)
  (normalize-fx (apply * args) int frac))

(define (fx/ int frac arg . rest)
  (if (ormap (λ (x) (= (real->fixed 0 int frac) x)) rest)
      (real->fixed 0 int frac)
      (normalize-fx (apply / arg rest) int frac)))

(define (fxshl int frac x shift)
  (normalize-fx (arithmetic-shift x shift) int frac))

(define (fxshr int frac x shift)
  (normalize-fx (arithmetic-shift x (- shift)) int frac))