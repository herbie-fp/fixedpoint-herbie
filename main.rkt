#lang racket

(require herbie/plugin math/bigfloat rival)
(require "private/fixed.rkt")

(eprintf "Loading fixed-point support...\n")

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define (sym-append . args)
  (string->symbol (apply string-append (map ~s args))))

;; Common ops

; bitwise ops (must override `bf` to use)

(define-operator (bnot real) real
  [bf #f] [ival #f] [nonffi bitwise-not])

(define-operator (bor real real) real
  [bf #f] [ival #f] [nonffi bitwise-ior])

(define-operator (bxor real real) real
  [bf #f] [ival #f] [nonffi bitwise-xor])

(define-operator (band real real) real
  [bf #f] [ival #f] [nonffi bitwise-and])

;; Integer 

(define (bfldexp x y)
  (bf* x (bfexpt 2.bf y)))

(define (ival-ldexp x y)
  (ival-mult x (ival-pow (mk-ival 2.bf) y)))

(define (nonffi-ldexp x y)
  (* x (expt 2 y)))

(module hairy racket/base
  (require ffi/unsafe)
  (provide ldexp ldexpf)

  (define ldexp (get-ffi-obj 'ldexp #f (_fun _double _int -> _double)))
  (define ldexpf (get-ffi-obj 'ldexpf #f (_fun _float _int -> _float)))
)

(require (submod "." hairy))

;; Integer-specific operators and rules
(define (generate-int32)
  (register-operator! 'ldexp (list 'real 'real) 'real
    `((bf . ,bfldexp) (ival . ,ival-ldexp) (nonffi . ,nonffi-ldexp)))

  (when ldexp
    (register-operator-impl! 'ldexp 'ldexp.f64 (list 'binary64 'integer) 'binary64
      `((fl . ,ldexp)))

    (register-ruleset! 'ldexp-f64 '(arithmetic) '((x . binary64) (y . binary64))
      '((ldexp_binary64 (*.f64 x (pow.f64 2 y)) (ldexp.f64 x (binary64->integer y)))
        (un_ldexp_binary64 (ldexp.f64 x (binary64->integer y)) (*.f64 x (pow.f64 2 y))))))

  (when ldexpf
    (register-operator-impl! 'ldexp 'ldexp.f32 (list 'binary32 'integer) 'binary32
      `((fl . ,ldexpf)))
    
    (register-ruleset! 'ldexp-f32 '(arithmetic) '((x . binary32) (y . binary32))
      '((ldexp_binary32 (*.f32 x (pow.f32 2 y)) (ldexp.f32 x (binary32->integer y)))
        (un_ldexp_binary32 (ldexp.f32 x (binary32->integer y)) (*.f32 x (pow.f32 2 y))))))

  ; Hacker's Delight: average of two integers
  ;;; (define-ruleset average-i32 (arithmetic integer numerics)
  ;;; #:type ([a integer] [b integer])
  ;;; [i32-avg2   (/.i32 (+.i32 a b) 2)   (+.i32 (+.i32 (and.i32 a b) (shr.i32 (xor.i32 a b) 1)) 
  ;;;                                          (and.i32 (neg.i32 (shr.i32 (+.i32 (and.i32 a b) (shr.i32 (xor.i32 a b) 1)) 63))
  ;;;                                                   (xor.i32 a b)))])

  #t)

; General fixed-point operations
(define (generate-fixed-point* nbits scale name)
  (define bf->fx (bigfloat->fx #t nbits scale))
  (define fx->bf (fx->bigfloat #t nbits scale))
  (define ord->fx (ordinal->fx #t nbits scale))
  (define fx->ord (fx->ordinal #t nbits scale))

  (define (fx-name name)
    (sym-append name '.fx nbits '- scale))

  ; Representation
  (register-representation! name 'real fx?
    bf->fx fx->bf ord->fx fx->ord
    nbits nan?)

  ; Operator implementations

  ; Helper function to declare operator implementations
  (define (register-fx-operator! op op-name argc fl-impl
                                #:bf [bf-impl #f] #:ival [ival-impl #f]
                                #:nonffi [nonffi-imlp #f]
                                #:itype [itype #f] #:otype [otype #f])
    (define base-dict (list (cons 'fl fl-impl) (cons 'bf bf-impl) (cons 'ival ival-impl)
                            (cons 'ival ival-impl) (cons 'itype itype) (cons 'otype otype)))
    (define info-dict (filter cdr base-dict))
    (register-operator-impl! op (fx-name op-name) (make-list argc name) name info-dict))

  (register-fx-operator! 'neg 'neg 1 (curry (fx2- #t nbits scale) 0))
  (register-fx-operator! '+ '+ 2 (fx2+ #t nbits scale))
  (register-fx-operator! '- '- 2 (fx2- #t nbits scale))
  (register-fx-operator! '* '* 2 (fx2* #t nbits scale))
  (register-fx-operator! '/ '/ 2 (fx2/ #t nbits scale))
  (register-fx-operator! 'sqrt 'sqrt 1 (fxsqrt #t nbits scale))
  (register-fx-operator! 'cbrt 'cbrt 1 (fxcbrt #t nbits scale))
  (register-fx-operator! 'fabs 'fabs 1 abs)

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

  ; Bitwise operator implemenetations

  (define fxnot* (fxnot #t nbits scale))
  (define fxor* (fxor #t nbits scale))
  (define fxxor* (fxxor #t nbits scale))
  (define fxand* (fxand #t nbits scale))

  (define (bfnot x)
    (let ([x (bf->fx x)])
      (fx->bf (fxnot* x))))

  (define (bfor x y)
    (let ([x (bf->fx x)] [y (bf->fx y)])
      (fx->bf (fxor* x y))))

  (define (bfxor x y)
    (let ([x (bf->fx x)] [y (bf->fx y)])
      (fx->bf (fxxor* x y))))

  (define (bfand x y)
    (let ([x (bf->fx x)] [y (bf->fx y)])
      (fx->bf (fxand* x y))))

  (register-fx-operator! 'bnot 'bnot 1 fxnot* #:bf bfnot #:nonffi fxnot*)
  (register-fx-operator! 'bor 'bor 2 fxor* #:bf bfor #:nonffi fxor*)
  (register-fx-operator! 'bxor 'bxor 2 fxxor* #:bf bfxor #:nonffi fxxor*)
  (register-fx-operator! 'band 'band 2 fxand* #:bf bfand #:nonffi fxand*)

  ; Rules

  (define bnot (fx-name 'bnot))
  (define bor (fx-name 'bor))
  (define bxor (fx-name 'bxor))
  (define band (fx-name 'band))

  (register-ruleset! 'bitwise-commutativity '(arithmetic bitwise simplify fp-safe)
    `((x . ,name) (y . ,name))
    `((,(fx-name 'commutate-or)    (,bor x y)       (,bor y x))
      (,(fx-name 'commutate-xor)   (,bxor x y)      (,bxor y x))
      (,(fx-name 'commutate-and)   (,band x y)      (,band y x))))

  (register-ruleset! 'bitwise-associativity '(arithmetic bitwise simplify fp-safe)
    `((x . ,name) (y . ,name) (z . ,name))
    `((,(fx-name 'associate-or-r)    (,bor x (,bor y z))       (,bor (,bor x y) z))
      (,(fx-name 'associate-or-l)    (,bor (,bor x y) z)       (,bor x (,bor y z)))
      (,(fx-name 'associate-xor-r)   (,bxor (,bxor x y) z)     (,bxor x (,bxor y z)))
      (,(fx-name 'associate-xor-l)   (,bxor (,bxor x y) z)     (,bxor x (,bxor y z)))
      (,(fx-name 'associate-and-r)   (,band (,band x y) z)     (,band x (,band y z)))
      (,(fx-name 'associate-and-l)   (,band (,band x y) z)     (,band x (,band y z)))))

  (register-ruleset! 'bitwise-identity '(arithmetic bitwise simplify fp-safe)
    `((x . ,name))
    `((,(fx-name 'or-identity)    (,bor x 0)    x)
      (,(fx-name 'xor-identity)   (,bxor x 0)   x)
      (,(fx-name 'or-identity-2)  (,bxor x x)   x)))

  (register-ruleset! 'bitwise-reduce '(arithmetic bitwise simplify fp-safe)
    `((x . ,name))
    `((,(fx-name 'not-reduce)     (,bnot (,bnot x))    x)
      (,(fx-name 'xor-reduce)     (,bxor x x)         0)
      (,(fx-name 'and-reduce)     (,band x 0)         0)))

  (register-ruleset! 'bitwise-xor-def '(arithmetic bitwise fp-safe)
    `((x . ,name) (y . ,name))
    `((,(fx-name 'xor-def-1)   (,bxor x y)   (,band (,bor x y) (,bor (,bnot x) (,bnot y))))
      (,(fx-name 'xor-def-2)   (,bxor x y)   (,bor (,band x (,bnot y)) (,band (,bnot x) y)))))

  (register-ruleset! 'bitwise-xor-reduce '(arithmetic bitwise simplify fp-safe)
    `((x . ,name) (y . ,name))
    `((,(fx-name 'xor-reduce-1)   (,band (,bor x y) (,bor (,bnot x) (,bnot y)))   (,bxor x y))
      (,(fx-name 'xor-reduce-2)   (,bor (,band x (,bnot y)) (,band (,bnot x) y))  (,bxor x y))))

  (register-ruleset! 'bitwise-distribute '(arithmetic bitwise simplify fp-safe)
    `((x . ,name) (y . ,name) (z . ,name))
    `((,(fx-name 'distribute-or-and-l)    (,bor x (,band y z))    (,band (,bor x y) (,bor x z)))
      (,(fx-name 'distribute-or-and-r)    (,bor (,band y z) x)    (,band (,bor x y) (,bor x z)))
      (,(fx-name 'distribute-and-or-l)    (,band x (,bor y z))    (,bor (,band x y) (,band x z)))
      (,(fx-name 'distribute-and-or-r)    (,band (,bor y z) x)    (,bor (,band x y) (,band x z)))
      (,(fx-name 'distribute-and-xor-l)   (,band x (,bxor y z))   (,bxor (,band x y) (,band x z)))
      (,(fx-name 'distribute-and-xor-r)   (,band (,bxor y z) x)   (,bxor (,band x y) (,band x z)))
      (,(fx-name 'distribute-not-or)      (,bnot (,bor x y))      (,band (,bnot x) (,bnot y)))
      (,(fx-name 'distribute-not-and)     (,bnot (,band x y))     (,bor (,bnot x) (,bnot y)))))

  (register-ruleset! 'bitwise-eliminate '(arithmetic bitwise simplify fp-safe)
    `((x . ,name) (y . ,name))
    `((,(fx-name 'eliminate-or-and-l)     (,bor x (,band x y))    x)
      (,(fx-name 'eliminate-or-and-r)     (,bor (,band x y) x)    x)
      (,(fx-name 'eliminate-and-or-l)     (,band x (,bor x y))    x)
      (,(fx-name 'eliminate-and-or-r)     (,band (,bor x y) x)    x)))

  #t)

;; Generator for fixed-point representations
(define (generate-fixed-point name)
  (match name
    [(list 'fixed nbits scale) (generate-fixed-point* nbits scale name)]
    [_ #f]))

;; Generator for integer representations
(define (generate-integer name)
  (match name
   ['integer
    (generate-fixed-point* 32 0 name)
    (generate-int32)]
   [(list 'integer n) (generate-fixed-point* n 0 name)]
   [_ #f]))

(register-generator! generate-integer)
(register-generator! generate-fixed-point)
