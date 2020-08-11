#lang racket

(provide (all-defined-out))

;; Integer operations

(define (normalize-int x int frac)
  (define bits (+ int frac 1))
  (define shift (expt 2 (sub1 bits)))
  (- (modulo (+ (round x) shift) (expt 2 bits)) shift))

(define (clamp-int x bits)
  (define v (expt 2 (sub1 bits)))
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
  (round (* x (expt 2 frac))))

(define (int->ordinal x bits)
  (+ x (expt 2 (sub1 bits))))

(define (ordinal->int x bits)
  (- x (expt 2 (sub1 bits))))

;; Fixed point operations

(define (fx+ x y int frac)
  (normalize-int (+ x y) int frac))

(define (fx- x y int frac)
  (normalize-int (- x y) int frac))

(define (fx* x y int frac)
  (normalize-int (* x y) int frac))

(define (fx/ x y int frac)
  (normalize-int (/ x y) int frac))

(define (fxshl x shift int frac)
  (normalize-int (arithmetic-shift x shift) int frac))

(define (fxshr x shift int frac)
  (normalize-int (arithmetic-shift x (- shift)) int frac))