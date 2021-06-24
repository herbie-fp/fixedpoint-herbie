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

(define (sym-append . args)
  (string->symbol (apply string-append (map ~s args))))

;; Generator for fixed-point representations
(define (generate-fixed-point name)
  (match name
   [(list 'fixed nbits scale)

    (define (register-fx-operator! op op-name argc fl-impl
                                   #:bf [bf-impl #f] #:ival [ival-impl #f]
                                   #:nonffi [nonffi-imlp #f]
                                   #:itype [itype #f] #:otype [otype #f])
      (define base-dict (list (cons 'fl fl-impl) (cons 'bf bf-impl) (cons 'ival ival-impl)
                              (cons 'ival ival-impl) (cons 'itype itype) (cons 'otype otype)))
      (define info-dict (filter cdr base-dict))
      (define op-name* (sym-append op-name '.fx nbits '- scale))
      (register-operator-impl! op op-name* (make-list argc name) name info-dict))
  
    ; Representation
    (register-representation! name 'real fx?
      (curry bigfloat->fx nbits scale)
      (curry fx->bigfloat nbits scale)
      (curry ordinal->fx nbits scale)
      (curry fx->ordinal nbits scale)
      nbits
      (const #f))

    ; Operators

    (register-fx-operator! 'neg 'neg 1)
    (register-fx-operator! '+ '+ 2 (fx+ nbits scale))
    (register-fx-operator! '- '- 2 (fx- nbits scale))
    (register-fx-operator! '* '* 2 (fx* nbits scale))
    (register-fx-operator! '/ '/ 2 (fx/ nbits scale))
    (register-fx-operator! 'sqrt 'sqrt 1 (fxsqrt nbits scale))
    (register-fx-operator! 'cbrt 'cbrt 1 (fxcbrt nbits scale))
    (register-fx-operator! 'fabs 'fabs 1 abs)

    ; (register-fx-operator! 'shl 'shl 2 (fxshl nbits scale) bfshl #f)
    ; (register-fx-operator! 'shr 'shr 2 (fxshr nbits scale) bfshr #f)

    (register-fx-operator! 'exp 'exp 1 (fxexp nbits scale))
    (register-fx-operator! 'log 'log 1 (fxlog nbits scale))
    (register-fx-operator! 'pow 'pow 2 (fxpow nbits scale))

    (register-fx-operator! 'sin 'sin 1 (fxsin nbits scale))
    (register-fx-operator! 'cos 'cos 1 (fxcos nbits scale))
    (register-fx-operator! 'tan 'tan 1 (fxtan nbits scale))
    (register-fx-operator! 'asin 'asin 1 (fxasin nbits scale))
    (register-fx-operator! 'acos 'acos 1 (fxacos nbits scale))
    (register-fx-operator! 'atan 'atan 1 (fxatan nbits scale))

    (register-fx-operator! '== '== 2 (comparator =) #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '!= '!= 2 (negate (comparator =)) #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '< '< 2 (comparator <) #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '> '> 2 (comparator >) #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '<= '<= 2 (comparator <=) #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '>= '>= 2 (comparator >=) #:itype name #:otype 'bool) ; override number of arguments

    #t]
   [_ #f]))

(register-generator! generate-fixed-point)
