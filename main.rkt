#lang racket

(require herbie/plugin math/bigfloat "fixedpoint.rkt")

(eprintf "Loading fixed-point support...\n")

(define (bfshl x y)
  (bf* x (bfexpt 2.bf y)))

(define (bfshr x y)
  (bf/ x (bfexpt 2.bf y)))

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define (!=-fn . args)
  (not (check-duplicates args =)))

(define (bf!=-fn . args)
  (not (check-duplicates args bf=)))

;; Generator for fixed-point representations
(define (generate-fixed-point name)
  (match name
   [(list 'fixed int frac)
    (define (add-suffix x)
      (string->symbol 
        (string-append (~s x) ".fx" (~s int) "-" (~s frac))))
  
    ; Representation
    (register-representation! name 'real exact-integer?
      (curryr bigfloat->fx int frac)
      (curryr fx->bigfloat int frac)
      (curryr ordinal->fx int frac)
      (curryr fx->ordinal int frac)
      (+ int frac 1)
      (const #f))

    ; Operators

    (register-operator! '+ (add-suffix '+) (list name name) name
      (list (cons 'fl (curry fx+ int frac)) (cons 'bf bf+)
            (cons 'ival #f) (cons 'nonffi (curry fx+ int frac))))

    (register-operator! '- (add-suffix 'neg) (list name) name
      (list (cons 'fl (curry fx- int frac)) (cons 'bf bf-)
            (cons 'ival #f) (cons 'nonffi (curry fx- int frac))))

    (register-operator! '- (add-suffix '-) (list name name) name
      (list (cons 'fl (curry fx- int frac)) (cons 'bf bf-)
            (cons 'ival #f) (cons 'nonffi (curry fx- int frac))))

    (register-operator! '* (add-suffix '*) (list name name) name
      (list (cons 'fl (curry fx* int frac)) (cons 'bf bf*)
            (cons 'ival #f) (cons 'nonffi (curry fx+ int frac))))
    
    (register-operator! '/ (add-suffix '/) (list name name) name
      (list (cons 'fl (curry fx/ int frac)) (cons 'bf bf/)
            (cons 'ival #f) (cons 'nonffi (curry fx/ int frac))))

    (register-operator! 'shl (add-suffix 'shl) (list name name) name
      (list (cons 'fl (curry fxshl int frac)) (cons 'bf bfshl)
            (cons 'ival #f) (cons 'nonffi (curry fxshl int frac))))

    (register-operator! 'shr (add-suffix 'shr) (list name name) name
      (list (cons 'fl (curry fxshr int frac)) (cons 'bf bfshr)
            (cons 'ival #f) (cons 'nonffi (curry fxshr int frac))))

    (register-operator! 'sqrt (add-suffix 'sqrt) (list name) name
      (list (cons 'fl (curry fxsqrt int frac)) (cons 'bf bfsqrt)
            (cons 'ival #f) (cons 'nonffi (curry fxsqrt int frac))))

    (register-operator! 'cbrt (add-suffix 'cbrt) (list name) name
      (list (cons 'fl (curry fxcbrt int frac)) (cons 'bf bfcbrt)
            (cons 'ival #f) (cons 'nonffi (curry fxcbrt int frac))))

    (register-operator! 'exp (add-suffix 'exp) (list name) name
      (list (cons 'fl (curry fxexp int frac)) (cons 'bf bfexp)
            (cons 'ival #f) (cons 'nonffi (curry fxexp int frac))))

    (register-operator! 'log (add-suffix 'log) (list name) name
      (list (cons 'fl (curry fxlog int frac)) (cons 'bf bflog)
            (cons 'ival #f) (cons 'nonffi (curry fxlog int frac))))

    (register-operator! 'pow (add-suffix 'pow) (list name name) name
      (list (cons 'fl (curry fxpow int frac)) (cons 'bf bfexpt)
            (cons 'ival #f) (cons 'nonffi (curry fxpow int frac))))

    (register-operator! '== (add-suffix '==) (list name name) name 
      (list (cons 'itype name) (cons 'otype 'bool)  ; override number of arguments
            (cons 'fl (comparator =)) (cons 'bf (comparator bf=))
            (cons 'ival #f) (cons 'nonffi (comparator =))))

    (register-operator! '!= (add-suffix '!=) (list name name) name 
      (list (cons 'itype name) (cons 'otype 'bool)  ; override number of arguments
            (cons 'fl !=-fn) (cons 'bf bf!=-fn)
            (cons 'ival #f) (cons 'nonffi !=-fn)))

    (register-operator! '< (add-suffix '<) (list name name) name 
      (list (cons 'itype name) (cons 'otype 'bool)  ; override number of arguments
            (cons 'fl (comparator <)) (cons 'bf (comparator bf<))
            (cons 'ival #f) (cons 'nonffi (comparator <))))

    (register-operator! '> (add-suffix '>) (list name name) name 
      (list (cons 'itype name) (cons 'otype 'bool)  ; override number of arguments
            (cons 'fl (comparator >)) (cons 'bf (comparator bf>))
            (cons 'ival #f) (cons 'nonffi (comparator >))))

    (register-operator! '<= (add-suffix '<=) (list name name) name 
      (list (cons 'itype name) (cons 'otype 'bool)  ; override number of arguments
            (cons 'fl (comparator <=)) (cons 'bf (comparator bf<=))
            (cons 'ival #f) (cons 'nonffi (comparator <=))))

    (register-operator! '>= (add-suffix '>=) (list name name) name 
      (list (cons 'itype name) (cons 'otype 'bool)  ; override number of arguments
            (cons 'fl (comparator >=)) (cons 'bf (comparator bf>=))
            (cons 'ival #f) (cons 'nonffi (comparator >=))))

    #t]
   [_ #f]))

(register-generator! generate-fixed-point)