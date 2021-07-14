#lang racket

(require herbie/plugin math/bigfloat math/flonum rival)
(require "private/fixed.rkt")

(eprintf "Loading fixed-point support...\n")

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define (sym-append . args)
  (string->symbol (apply string-append (map ~s args))))

(define at-least-racket-8?
  (>= (string->number (substring (version) 0 1)) 8))

; Need a placeholder for < 8.0
(define cast-single
  (let ([flsingle identity])
    (local-require racket/flonum)
    flsingle))

(define (->float32 x)
  (if at-least-racket-8?
      (cast-single (exact->inexact x))
      (real->single-flonum x)))

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

(define-operator (shl real real) real
  [bf #f] [ival #f] [nonffi arithmetic-shift])

(define-operator (shr real real) real
  [bf #f] [ival #f] [nonffi arithmetic-shift])

; reinterpret
(define-operator (reinterpret real) real
  [bf identity] [ival identity] [nonffi identity])
  

; General fixed-point operations
(define (generate-fixed-point* sign? nbits scale name)
  (define fx->re (fx->real sign? nbits scale))
  (define re->fx (real->fx sign? nbits scale))
  (define bf->fx (bigfloat->fx sign? nbits scale))
  (define fx->bf (fx->bigfloat sign? nbits scale))
  (define ord->fx (ordinal->fx sign? nbits scale))
  (define fx->ord (fx->ordinal sign? nbits scale))

  (define (fx-name name)
    (sym-append name (if sign? '.fx '.ufx) nbits '- scale))

  ; Representation
  (register-representation! name 'real fx?
    bf->fx fx->bf ord->fx fx->ord
    nbits nan?)

  ; Constant implementations

  (define (register-fx-constant! cnst fx-impl #:bf [bf-impl #f] #:ival [ival-impl #f])
      (define base-dict
        (list (cons 'fl fx-impl)
              (cons 'bf bf-impl)
              (cons 'ival ival-impl)))
      (define info-dict (filter cdr base-dict))
      (register-constant-impl! cnst (fx-name cnst) name info-dict))

  (register-fx-constant! 'PI
      (const ((real->fx sign? nbits scale) pi)))

  (register-fx-constant! 'E
      (const ((real->fx sign? nbits scale) (exp 1.0))))

  (register-fx-constant! 'INFINITY
      (const ((real->fx sign? nbits scale) +inf.0)))

  (register-fx-constant! 'NAN
      (const ((real->fx sign? nbits scale) +nan.0)))

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

  (register-fx-operator! 'neg 'neg 1 (curry (fx2- sign? nbits scale) 0))
  (register-fx-operator! '+ '+ 2 (fx2+ sign? nbits scale))
  (register-fx-operator! '- '- 2 (fx2- sign? nbits scale))
  (register-fx-operator! '* '* 2 (fx2* sign? nbits scale))
  (register-fx-operator! '/ '/ 2 (fx2/ sign? nbits scale))
  (register-fx-operator! 'sqrt 'sqrt 1 (fxsqrt sign? nbits scale))
  (register-fx-operator! 'cbrt 'cbrt 1 (fxcbrt sign? nbits scale))
  (register-fx-operator! 'fabs 'fabs 1 abs)

  (register-fx-operator! 'exp 'exp 1 (fxexp sign? nbits scale))
  (register-fx-operator! 'log 'log 1 (fxlog sign? nbits scale))
  (register-fx-operator! 'pow 'pow 2 (fxpow sign? nbits scale))

  (register-fx-operator! 'sin 'sin 1 (fxsin sign? nbits scale))
  (register-fx-operator! 'cos 'cos 1 (fxcos sign? nbits scale))
  (register-fx-operator! 'tan 'tan 1 (fxtan sign? nbits scale))
  (register-fx-operator! 'asin 'asin 1 (fxasin sign? nbits scale))
  (register-fx-operator! 'acos 'acos 1 (fxacos sign? nbits scale))
  (register-fx-operator! 'atan 'atan 1 (fxatan sign? nbits scale))

  (register-fx-operator! '== '== 2 (comparator =) #:itype name #:otype 'bool) ; override number of arguments
  (register-fx-operator! '!= '!= 2 (negate (comparator =)) #:itype name #:otype 'bool) ; override number of arguments
  (register-fx-operator! '< '< 2 (comparator <) #:itype name #:otype 'bool) ; override number of arguments
  (register-fx-operator! '> '> 2 (comparator >) #:itype name #:otype 'bool) ; override number of arguments
  (register-fx-operator! '<= '<= 2 (comparator <=) #:itype name #:otype 'bool) ; override number of arguments
  (register-fx-operator! '>= '>= 2 (comparator >=) #:itype name #:otype 'bool) ; override number of arguments

  ; Bitwise operator implemenetations

  (define fxnot* (fxnot sign? nbits scale))
  (define fxor* (fxor sign? nbits scale))
  (define fxxor* (fxxor sign? nbits scale))
  (define fxand* (fxand sign? nbits scale))

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

  (define (real-fx-op f)
    (λ (a b)
      (let ([a (re->fx a)] [b (re->fx b)])
        (fx->re (f a b)))))

  (register-fx-operator! 'bnot 'bnot 1 fxnot* #:bf bfnot #:nonffi (real-fx-op fxnot*))
  (register-fx-operator! 'bor 'bor 2 fxor* #:bf bfor #:nonffi (real-fx-op fxor*))
  (register-fx-operator! 'bxor 'bxor 2 fxxor* #:bf bfxor #:nonffi (real-fx-op fxxor*))
  (register-fx-operator! 'band 'band 2 fxand* #:bf bfand #:nonffi (real-fx-op fxand*))

  ; Rules

  (define bnot (fx-name 'bnot))
  (define bor (fx-name 'bor))
  (define bxor (fx-name 'bxor))
  (define band (fx-name 'band))

  (register-ruleset! (fx-name 'bitwise-commutativity) '(arithmetic bitwise simplify fp-safe)
    `((x . ,name) (y . ,name))
    `((,(fx-name 'commutate-or)    (,bor x y)       (,bor y x))
      (,(fx-name 'commutate-xor)   (,bxor x y)      (,bxor y x))
      (,(fx-name 'commutate-and)   (,band x y)      (,band y x))))

  (register-ruleset! (fx-name 'bitwise-associativity) '(arithmetic bitwise simplify fp-safe)
    `((x . ,name) (y . ,name) (z . ,name))
    `((,(fx-name 'associate-or-r)    (,bor x (,bor y z))       (,bor (,bor x y) z))
      (,(fx-name 'associate-or-l)    (,bor (,bor x y) z)       (,bor x (,bor y z)))
      (,(fx-name 'associate-xor-r)   (,bxor (,bxor x y) z)     (,bxor x (,bxor y z)))
      (,(fx-name 'associate-xor-l)   (,bxor (,bxor x y) z)     (,bxor x (,bxor y z)))
      (,(fx-name 'associate-and-r)   (,band (,band x y) z)     (,band x (,band y z)))
      (,(fx-name 'associate-and-l)   (,band (,band x y) z)     (,band x (,band y z)))))

  (register-ruleset! (fx-name 'bitwise-identity) '(arithmetic bitwise simplify fp-safe)
    `((x . ,name))
    `((,(fx-name 'or-identity)    (,bor x 0)    x)
      (,(fx-name 'xor-identity)   (,bxor x 0)   x)
      (,(fx-name 'or-identity-2)  (,bxor x x)   x)))

  (register-ruleset! (fx-name 'bitwise-reduce) '(arithmetic bitwise simplify fp-safe)
    `((x . ,name))
    `((,(fx-name 'not-reduce)     (,bnot (,bnot x))    x)
      (,(fx-name 'xor-reduce)     (,bxor x x)         0)
      (,(fx-name 'and-reduce)     (,band x 0)         0)))

  (register-ruleset! (fx-name 'bitwise-xor-def) '(arithmetic bitwise fp-safe)
    `((x . ,name) (y . ,name))
    `((,(fx-name 'xor-def-1)   (,bxor x y)   (,band (,bor x y) (,bor (,bnot x) (,bnot y))))
      (,(fx-name 'xor-def-2)   (,bxor x y)   (,bor (,band x (,bnot y)) (,band (,bnot x) y)))))

  (register-ruleset! (fx-name 'bitwise-xor-reduce) '(arithmetic bitwise simplify fp-safe)
    `((x . ,name) (y . ,name))
    `((,(fx-name 'xor-reduce-1)   (,band (,bor x y) (,bor (,bnot x) (,bnot y)))   (,bxor x y))
      (,(fx-name 'xor-reduce-2)   (,bor (,band x (,bnot y)) (,band (,bnot x) y))  (,bxor x y))))

  (register-ruleset! (fx-name 'bitwise-distribute) '(arithmetic bitwise simplify fp-safe)
    `((x . ,name) (y . ,name) (z . ,name))
    `((,(fx-name 'distribute-or-and-l)    (,bor x (,band y z))    (,band (,bor x y) (,bor x z)))
      (,(fx-name 'distribute-or-and-r)    (,bor (,band y z) x)    (,band (,bor x y) (,bor x z)))
      (,(fx-name 'distribute-and-or-l)    (,band x (,bor y z))    (,bor (,band x y) (,band x z)))
      (,(fx-name 'distribute-and-or-r)    (,band (,bor y z) x)    (,bor (,band x y) (,band x z)))
      (,(fx-name 'distribute-and-xor-l)   (,band x (,bxor y z))   (,bxor (,band x y) (,band x z)))
      (,(fx-name 'distribute-and-xor-r)   (,band (,bxor y z) x)   (,bxor (,band x y) (,band x z)))
      (,(fx-name 'distribute-not-or)      (,bnot (,bor x y))      (,band (,bnot x) (,bnot y)))
      (,(fx-name 'distribute-not-and)     (,bnot (,band x y))     (,bor (,bnot x) (,bnot y)))))

  (register-ruleset! (fx-name 'bitwise-eliminate) '(arithmetic bitwise simplify fp-safe)
    `((x . ,name) (y . ,name))
    `((,(fx-name 'eliminate-or-and-l)     (,bor x (,band x y))    x)
      (,(fx-name 'eliminate-or-and-r)     (,bor (,band x y) x)    x)
      (,(fx-name 'eliminate-and-or-l)     (,band x (,bor x y))    x)
      (,(fx-name 'eliminate-and-or-r)     (,band (,bor x y) x)    x)))

  ; mul as shift-left rules                      
  (register-ruleset! (fx-name 'mul-shl) '(arithmetic bitwise)
    `((a . ,name))
    (for/list ([i (in-range 1 nbits)])
      (let ([name-lhs (string->symbol (format "~a-mul-shl-~a-l" name i))]
            [name-rhs (string->symbol (format "~a-mul-shl-~a-r" name i))])
        (list name-lhs `(,(fx-name '*) ,(expt 2 i) a) `(,(fx-name 'shl) a ,i))
        (list name-rhs `(,(fx-name '*) a ,(expt 2 i)) `(,(fx-name 'shl) a ,i)))))

  ; div as shift-right rules                      
  (register-ruleset! (fx-name 'mul-shr) '(arithmetic integer)
    `((a . ,name))
    (for/list ([i (in-range 1 32)])
      (let ([name (string->symbol (format "~a-mul-shr-~a" name i))])
        (list name `(,(fx-name '/) a ,(expt 2 i)) `(,(fx-name 'shr) a ,i)))))

  ; average
  (register-ruleset! (fx-name 'average) '(arithmetic integer)
    `((a . ,name) (b . ,name))
    `((,(fx-name 'average) (,(fx-name '/) (,(fx-name '+) a b) 2)
                           (,(fx-name '+) a (,(fx-name '/) (,(fx-name '-) b a) 2)))))

  #t)

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

;; 32-bit integer operators and rules
(define (generate-int32)

  ; Operators

  (register-operator! 'ldexp (list 'real 'real) 'real
    `((bf . ,bfldexp) (ival . ,ival-ldexp) (nonffi . ,nonffi-ldexp)))

  ; Operator implementations

  (when ldexp
    (register-operator-impl! 'ldexp 'ldexp.f64 (list 'binary64 'integer) 'binary64
      `((fl . ,ldexp))))

  (when ldexpf
    (register-operator-impl! 'ldexp 'ldexp.f32 (list 'binary32 'integer) 'binary32
      `((fl . ,ldexpf))))

  (define (bfshl x y)
    (let ([x* (bigfloat->integer x)]
          [y* (bigfloat->integer y)])
      (bf ((fxshl #t 32 0) x* y*))))

  (define (bfshr x y)
    (let ([x* (bigfloat->integer x)]
          [y* (bigfloat->integer y)])
      (bf ((fxshr #t 32 0) x* y*))))

  (register-operator-impl! 'shl 'shl.fx32-0 (list 'integer 'integer) 'integer
    `((fl . ,(fxshl #t 32 0)) (bf . ,bfshl) (nonffi . ,(fxshl #t 32 0))))

  (register-operator-impl! 'shr 'shr.fx32-0 (list 'integer 'integer) 'integer
    `((fl . ,(fxshr #t 32 0)) (bf . ,bfshr) (nonffi . ,(fxshr #t 32 0))))

  (register-operator-impl! 'cast 'binary64->integer (list 'binary64) 'integer
    `((fl . ,(compose (real->fx #t 32 0) truncate))))

  (register-operator-impl! 'cast 'binary32->integer (list 'binary32) 'integer
    `((fl . ,(compose (real->fx #t 32 0) truncate))))

  (register-operator-impl! 'cast 'integer->binary64 (list 'integer) 'binary64
    `((fl . ,(compose real->double-flonum (fx->real #t 32 0)))))

  (register-operator-impl! 'cast 'integer->binary32 (list 'integer) 'binary32
    `((fl . ,(compose ->float32 real->double-flonum (fx->real #t 32 0)))))

  ; Rules

  (when ldexp
    (register-ruleset! 'ldexp-f64 '(arithmetic) '((x . binary64) (y . binary64))
      '((ldexp_binary64 (*.f64 x (pow.f64 2 y)) (ldexp.f64 x (binary64->integer y)))
        (un_ldexp_binary64 (ldexp.f64 x (binary64->integer y)) (*.f64 x (pow.f64 2 y))))))

  (when ldexpf
    (register-ruleset! 'ldexp-f32 '(arithmetic) '((x . binary32) (y . binary32))
      '((ldexp_binary32 (*.f32 x (pow.f32 2 y)) (ldexp.f32 x (binary32->integer y)))
        (un_ldexp_binary32 (ldexp.f32 x (binary32->integer y)) (*.f32 x (pow.f32 2 y))))))

  ; Average
  (register-ruleset! 'average-int-special '(arithmetic integer numerics)
    '((a . integer) (b . integer))
    '((int32-avg  (/.fx32-0 (+.fx32-0 a b) 2)   ; Hacker's Delight: average of two integers
                  (+.fx32-0 (+.fx32-0 (band.fx32-0 a b) (shr.fx32-0 (bxor.fx32-0 a b) 1))
                            (band.fx32-0 (neg.fx32-0 (shr.fx32-0 (+.fx32-0 (band.fx32-0 a b) (shr.fx32-0 (bxor.fx32-0 a b) 1)) 31))
                                         (bxor.fx32-0 a b))))))                       
  #t)

; 64-bit integer operators and rules
(define (generate-int64)

  ; Operator implementations

  (register-operator-impl! 'cast 'binary64->integer_64 (list 'binary64) '(integer 64)
    `((fl . ,(compose (real->fx #t 64 0) truncate))))

  (register-operator-impl! 'cast 'integer_64->binary64 (list '(integer 64)) 'binary64
    `((fl . ,(compose real->double-flonum (fx->real #t 32 0)))))

  (define (reinterpret-as-double x)
    (cond
     [(nan? x) +nan.0]
     [else (floating-point-bytes->real (integer->integer-bytes x 8 #t))]))

  (define (reinterpret-as-int64 x)
     (integer-bytes->integer (real->floating-point-bytes x 8) #t))

  (define (bfreinterpret-as-double x)
    (let ([x* (bigfloat->integer x)])
      (bf (floating-point-bytes->real (integer->integer-bytes x* 8 #t)))))

  (define (bfreinterpret-as-int64 x)
    (let ([x* (bigfloat->real x)])
      (bf (integer-bytes->integer (real->floating-point-bytes x* 8) #t))))

  (register-operator-impl! 'reinterpret 'reinterpret_int64_double (list '(integer 64)) 'binary64
    `((fl . ,reinterpret-as-double) (bf . ,bfreinterpret-as-double)
      (nonffi . ,reinterpret-as-double)))

  (register-operator-impl! 'reinterpret 'reinterpret_double_int64 (list 'binary64) '(integer 64)
    `((fl . ,reinterpret-as-int64) (bf . ,bfreinterpret-as-int64)
      (nonffi . ,reinterpret-as-int64)))

  ; Rules

  ; Taylor phase messes this up
  (register-ruleset! 'reinterpret_binary64_integer64 '(arithmetic integer)
    '((a . binary64) (b . (integer 64)))
    '((insert_integer64   a     (reinterpret_int64_double (reinterpret_double_int64 a)))
      (insert_double      b     (reinterpret_double_int64 (reinterpret_int64_double b)))))

  #t)


;; Generator for fixed-point representations
(define (generate-fixed-point name)
  (match name
    [(list 'fixed nbits scale) (generate-fixed-point* #t nbits scale name)]
    [(list 'ufixed nbits scale) (generate-fixed-point* #f nbits scale name)]
    [_ #f]))

;; Generator for integer representations
(define (generate-integer name)
  (match name
   ['integer
    (generate-fixed-point* #t 32 0 name)
    (generate-int32)]
   [(list 'integer n)
    (generate-fixed-point* #t n 0 name)
    (when (= n 64) (generate-int64))]
   ['uinteger
    (generate-fixed-point* #f 32 0 name)]
   [(list 'uinteger n)
    (generate-fixed-point* #f n 0 name)]
   [_ #f]))

(register-generator! generate-integer)
(register-generator! generate-fixed-point)
