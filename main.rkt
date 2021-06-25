#lang racket

(require herbie/plugin math/bigfloat rival)
(require "private/fixed.rkt")

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
      (bigfloat->fx #t nbits scale)
      (fx->bigfloat #t nbits scale)
      (ordinal->fx #t nbits scale)
      (fx->ordinal #t nbits scale)
      nbits
      nan?)

    ; Operators

    (register-fx-operator! 'neg 'neg 1 (curry (fx2- #t nbits scale) 0))
    (register-fx-operator! '+ '+ 2 (fx2+ #t nbits scale))
    (register-fx-operator! '- '- 2 (fx2- #t nbits scale))
    (register-fx-operator! '* '* 2 (fx2* #t nbits scale))
    (register-fx-operator! '/ '/ 2 (fx2/ #t nbits scale))
    (register-fx-operator! 'sqrt 'sqrt 1 (fxsqrt #t nbits scale))
    (register-fx-operator! 'cbrt 'cbrt 1 (fxcbrt #t nbits scale))
    (register-fx-operator! 'fabs 'fabs 1 abs)

    ; (register-fx-operator! 'shl 'shl 2 (fxshl nbits scale) bfshl #f)
    ; (register-fx-operator! 'shr 'shr 2 (fxshr nbits scale) bfshr #f)

    (register-fx-operator! 'exp 'exp 1 (fxexp #t nbits scale))
    (register-fx-operator! 'log 'log 1 (fxlog #t nbits scale))
    (register-fx-operator! 'pow 'pow 2 (fxpow #t nbits scale))

    (register-fx-operator! 'sin 'sin 1 (fxsin #t nbits scale))
    (register-fx-operator! 'cos 'cos 1 (fxcos #t nbits scale))
    (register-fx-operator! 'tan 'tan 1 (fxtan #t nbits scale))
    (register-fx-operator! 'asin 'asin 1 (fxasin #t nbits scale))
    (register-fx-operator! 'acos 'acos 1 (fxacos #t nbits scale))
    (register-fx-operator! 'atan 'atan 1 (fxatan #t nbits scale))

    (register-fx-operator! '== '== 2 (comparator =) #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '!= '!= 2 (negate (comparator =)) #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '< '< 2 (comparator <) #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '> '> 2 (comparator >) #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '<= '<= 2 (comparator <=) #:itype name #:otype 'bool) ; override number of arguments
    (register-fx-operator! '>= '>= 2 (comparator >=) #:itype name #:otype 'bool) ; override number of arguments

    #t]
   [_ #f]))

(register-generator! generate-fixed-point)
