#lang racket

(require math/bigfloat)
(provide (contract-out
          [fx? (-> any/c boolean?)]
          [fx->real (-> exact-nonnegative-integer? exact-nonnegative-integer? fx? real?)]
          [real->fx (-> exact-nonnegative-integer? exact-nonnegative-integer? real? fx?)]
          [fx->ordinal (-> exact-nonnegative-integer? exact-nonnegative-integer? fx? fx?)]
          [ordinal->fx (-> exact-nonnegative-integer? exact-nonnegative-integer? fx? fx?)]
          [fx->bigfloat (-> exact-nonnegative-integer? exact-nonnegative-integer? fx? bigfloat?)]
          [bigfloat->fx (-> exact-nonnegative-integer? exact-nonnegative-integer? bigfloat? fx?)]

          [fx+ (-> exact-nonnegative-integer? exact-nonnegative-integer?
                   (-> fx? fx? fx?))]
          [fx- (-> exact-nonnegative-integer? exact-nonnegative-integer?
                   (-> fx? fx? fx?))]
          [fx* (-> exact-nonnegative-integer? exact-nonnegative-integer?
                   (-> fx? fx? fx?))]
          [fx/ (-> exact-nonnegative-integer? exact-nonnegative-integer?
                   (-> fx? fx? fx?))]
          [fxshl (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> fx? fx? fx?))]
          [fxshr (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> fx? fx? fx?))]

          [fxsqrt (-> exact-nonnegative-integer? exact-nonnegative-integer?
                      (-> fx? fx?))]
          [fxcbrt (-> exact-nonnegative-integer? exact-nonnegative-integer?
                      (-> fx? fx?))]
          [fxexp (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> fx? fx?))]
          [fxlog (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> fx? fx?))]
          [fxpow (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> fx? fx? fx?))]

          [fxsin (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> fx? fx?))]
          [fxcos (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> fx? fx?))]
          [fxtan (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> fx? fx?))]
          [fxasin (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> fx? fx?))]
          [fxacos (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> fx? fx?))]
          [fxatan (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> fx? fx?))]
          [fxatan2 (-> exact-nonnegative-integer? exact-nonnegative-integer?
                     (-> fx? fx? fx?))]

          [fxbytes->binary64 (-> exact-nonnegative-integer? exact-nonnegative-integer?
                                 (-> fx? real?))]
          [binary64->fxbytes (-> exact-nonnegative-integer? exact-nonnegative-integer?
                                 (-> real? fx?))]
          [fxbytes->binary32 (-> exact-nonnegative-integer? exact-nonnegative-integer?
                                 (-> fx? real?))]
          [binary32->fxbytes (-> exact-nonnegative-integer? exact-nonnegative-integer?
                                 (-> real? fx?))]))

(module+ test
  (require rackunit))

;; Normalization

(define (normalize-fx int frac x)
  (cond
   [(nan? x) x]
   [else
    (define x* (inexact->exact (truncate x)))
    (define bits (+ int frac 1))
    (define shift (expt 2 (sub1 bits)))
    (- (modulo (+ x* shift) (expt 2 bits)) shift)]))

;; Conversions

(define (fx->real int frac x)
  (if (nan? x) x (/ x (expt 2 frac))))

(define (real->fx int frac x)
  (define max (sub1 (expt 2 int)))
  (define min (- (expt 2 int)))
  (cond
   [(nan? x) x]
   [(infinite? x) (if (negative? x) (* min (expt 2 frac)) (* max (expt 2 frac)))]
   [(> x max) (* max (expt 2 frac))]
   [(< x min) (* min (expt 2 frac))]
   [else (inexact->exact (truncate (* x (expt 2 frac))))]))

(define (fx->ordinal int frac x)  ; bits - 1
  (if (nan? x) (expt 2 (+ int frac 1)) (+ x (expt 2 (+ int frac)))))

(define (ordinal->fx int frac x)  ; bits - 1
  (if (= x (expt 2 (+ int frac 1))) +nan.0 (- x (expt 2 (+ int frac)))))

(define (bigfloat->fx int frac x)
  (real->fx int frac (bigfloat->real x)))

(define (fx->bigfloat int frac x)
  (bf (fx->real int frac x)))

;; Predicates

(define (fx? x)
  (and (real? x) (or (exact-integer? x) (nan? x))))

;; Helper functions

;; Arithmetic

(define (fx+ int frac)
  (λ (x y) (normalize-fx int frac (+ x y))))

(define (fx- int frac)
  (λ (x y) (normalize-fx int frac (- x y))))

(define (fx* int frac)
  (λ (x y) (normalize-fx int frac (/ (* x y) (arithmetic-shift 1 frac)))))

(define (fx/ int frac)
  (λ (x y)
    (cond [(zero? y) 0]
          [else (normalize-fx int frac (/ (* x (arithmetic-shift 1 frac)) y))])))

(define (fxsqrt int frac)
  (λ (x)
    (let ([res (sqrt (fx->real int frac x))])
      (real->fx int frac (if (real? res) res +nan.0)))))

(define (fxcbrt int frac)
    (λ (x)
    (let ([res (expt (fx->real int frac x) 1/3)])
      (real->fx int frac (if (real? res) res +nan.0)))))

;; Exponential / Logarithmic

(define (fxexp int frac)
  (λ (x) (real->fx int frac (exp (fx->real int frac x)))))

(define (fxlog int frac)
  (λ (x) 
    (let ([x* (fx->real int frac x)])
      (real->fx int frac
        (cond [(<= x* 0) +nan.0]
              [(> (* (log 2) x*) int) +inf.0]
              [else (log x*)])))))

(define (fxpow int frac)
  (λ (x y)
    (let ([x* (fx->real int frac x)]
          [y* (fx->real int frac y)])
      (real->fx int frac 
        (cond
         [(and (zero? x*) (negative? y*)) +nan.0]
         [(and (negative? x*) (not (integer? y*))) +nan.0]
         [else (expt (exact->inexact x*) (exact->inexact y*))])))))

;; Trigonometric

(define (fxsin int frac)
  (λ (x) (real->fx int frac (sin (fx->real int frac x)))))

(define (fxcos int frac)
  (λ (x) (real->fx int frac (cos (fx->real int frac x)))))

(define (fxtan int frac)
  (λ (x) (real->fx int frac (tan (fx->real int frac x)))))

(define (fxasin int frac)
  (λ (x)
    (let ([r (asin (fx->real int frac x))])
      (real->fx int frac
        (cond [(real? r) r]
              [else +nan.0])))))

(define (fxacos int frac)
  (λ (x)
    (let ([r (acos (fx->real int frac x))])
      (real->fx int frac
        (cond [(real? r) r]
              [else +nan.0])))))

(define (fxatan int frac)
  (λ (x)
    (let ([r (atan (fx->real int frac x))])
      (real->fx int frac
        (cond [(real? r) r]
              [else +nan.0])))))

(define (fxatan2 int frac)
  (λ (y x)
    (let ([r (atan (fx->real int frac y) (fx->real int frac x))])
      (real->fx int frac
        (cond [(real? r) r]
              [else +nan.0])))))

;; Bitwise operators

(define (fxshl int frac)
  (λ (x shift) (normalize-fx int frac (arithmetic-shift x shift))))

(define (fxshr int frac)
  (λ (x shift) (normalize-fx int frac (arithmetic-shift x (- shift)))))

(define (fxbytes->binary64 int frac)
  (unless (= (+ int frac 1) 64)
    (error 'fxbytes->binary64 "Operator does not exist for (fixed ~a ~a)\n" int frac))
  (λ (x)
    (let ([bstr (integer->integer-bytes x 8 #t)])
      (floating-point-bytes->real bstr))))

(define (binary64->fxbytes int frac)
  (unless (= (+ int frac 1) 64)
    (error 'binary64->fxbytes "Operator does not exist for (fixed ~a ~a)\n" int frac))
  (λ (x)
    (let ([bstr (real->floating-point-bytes x 8)])
      (integer-bytes->integer bstr #t))))

(define (fxbytes->binary32 int frac)
  (unless (= (+ int frac 1) 32)
    (error 'fxbytes->binary32 "Operator does not exist for (fixed ~a ~a)\n" int frac))
  (λ (x)
    (let ([bstr (integer->integer-bytes x 4 #t)])
      (floating-point-bytes->real bstr))))

(define (binary32->fxbytes int frac)
  (unless (= (+ int frac 1) 32)
    (error 'binary32->fxbytes "Operator does not exist for (fixed ~a ~a)\n" int frac))
  (λ (x)
    (let ([bstr (real->floating-point-bytes x 4)])
      (integer-bytes->integer bstr #t))))

;;
;;  Tests
;;

(module+ test
  (define int 20)
  (define frac 20)
  (define err 0.01)
 
  (define vals '(-100 -10 -1 -0.01 0 0.01 1 10 100))
  (define vals* (cons +nan.0 vals))
  (define vals2 (cartesian-product vals vals))

  (define ops (list fx+ fx- fx* fx/))
  (define bfops (list + - * /))

  (for ([v vals*])
    (define v* (fx->real int frac (real->fx int frac v)))
    (define max (+ v (abs (* v err))))
    (define min (- v (abs (* v err))))
    (check-true (or (<= min v* max) (and (nan? v) (nan? v*)))
                (format "fx<=>real: ~a != ~a" v v*)))

  (for ([v vals*])
    (define x (real->fx int frac v))
    (define x* (ordinal->fx int frac (fx->ordinal int frac x)))
    (check-true (or (= x x*) (and (nan? x) (nan? x*)))
                (format "fx<=>ordinal: ~a != ~a for ~a" x x* v)))

  (for ([v vals*])
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