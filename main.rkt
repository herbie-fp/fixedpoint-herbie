#lang racket

(require herbie/plugin math/bigfloat rival "fixedpoint.rkt")

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

(define (sym-append . args)
  (string->symbol (apply string-append (map ~s args))))

;; Generator for fixed-point representations
(define (generate-fixed-point name)
  (match name
   [(list 'fixed int frac)

    (define (register-fx-operator! op op-name argc fl-impl bf-impl ival-impl
                #:nonffi [nonffi-imlp #f] #:itype [itype #f] #:otype [otype #f])
      (define nonffi* (if nonffi-imlp nonffi-imlp fl-impl))
      (define op-name* (sym-append op-name '.fx int '- frac))
      (define info-dict
        (let ([base-dict (list (cons 'fl fl-impl) (cons 'bf bf-impl)
                               (cons 'ival ival-impl) (cons 'nonffi nonffi*))])
          (cond
            [(and itype otype) (dict-set* base-dict 'itype itype 'otype otype)]
            [itype (dict-set base-dict 'itype itype)]
            [otype (dict-set base-dict 'otype otype)]
            [else base-dict])))
      (register-operator! op op-name* (make-list argc name) name info-dict))
  
    ; Representation
    (register-representation! name 'real fx?
      (curry bigfloat->fx int frac)
      (curry fx->bigfloat int frac)
      (curry ordinal->fx int frac)
      (curry fx->ordinal int frac)
      (+ int frac 1)
      (const #f))

    ; Operators

    (register-fx-operator! '- 'neg 1 - bf- ival-neg)
    (register-fx-operator! '+ '+ 2 (fx+ int frac) bf+ ival-add)
    (register-fx-operator! '- '- 2 (fx- int frac) bf- ival-sub)
    (register-fx-operator! '* '* 2 (fx* int frac) bf* ival-mult)
    (register-fx-operator! '/ '/ 2 (fx/ int frac) bf/ ival-div)
    (register-fx-operator! 'sqrt 'sqrt 1 (fxsqrt int frac) bfsqrt ival-sqrt)
    (register-fx-operator! 'cbrt 'cbrt 1 (fxcbrt int frac) bfcbrt ival-cbrt)
    (register-fx-operator! 'abs 'abs 1 abs bfabs ival-fabs)

    ; (register-fx-operator! 'shl 'shl 2 (fxshl int frac) bfshl #f)
    ; (register-fx-operator! 'shr 'shr 2 (fxshr int frac) bfshr #f)

    (register-fx-operator! 'exp 'exp 1 (fxexp int frac) bfexp ival-exp)
    (register-fx-operator! 'log 'log 1 (fxlog int frac) bflog ival-log)
    (register-fx-operator! 'pow 'pow 2 (fxpow int frac) bfexpt ival-pow)

    (register-fx-operator! 'sin 'sin 1 (fxsin int frac) bfsin ival-sin)
    (register-fx-operator! 'cos 'cos 1 (fxcos int frac) bfcos ival-cos)
    (register-fx-operator! 'tan 'tan 1 (fxtan int frac) bftan ival-tan)
    (register-fx-operator! 'asin 'asin 1 (fxasin int frac) bfasin ival-asin)
    (register-fx-operator! 'acos 'acos 1 (fxacos int frac) bfacos ival-acos)
    (register-fx-operator! 'atan 'atan 1 (fxatan int frac) bfatan ival-atan)

    (register-fx-operator! '== '== 2 (comparator =) (comparator bf=) (comparator ival-==)
                           #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '!= '!= 2 !=-fn bf!=-fn ival-!=
                           #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '< '< 2 (comparator <) (comparator bf<) (comparator ival-<)
                           #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '> '> 2 (comparator >) (comparator bf>) (comparator ival->)
                           #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '<= '<= 2 (comparator <=) (comparator bf<=) (comparator ival-<=)
                           #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '>= '>= 2 (comparator >=) (comparator bf>=) (comparator ival->=)
                           #:itype name #:otype 'bool) ; override number of arguments

    #t]
   [_ #f]))

(register-generator! generate-fixed-point)