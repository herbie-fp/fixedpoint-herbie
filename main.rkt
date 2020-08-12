#lang racket

(require math/bigfloat herbie/plugin "fixedpoint.rkt")

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

; parametrized declaration of a fixed-point representation, its operators, and rules
(define-syntax (define-fixed-point stx)
  (syntax-case stx ()
   [(_ int frac)
    (let* ([int* (syntax-e (cadr (syntax-e stx)))]
           [frac* (syntax-e (caddr (syntax-e stx)))]
           [repr-name (list 'fixed int* frac*)]
           [op-name 
            (λ (name) 
              (string->symbol (string-append (symbol->string name) ".fx" (number->string int*) 
                                             "." (number->string frac*))))])
    #`(begin
        ;; Representations
        (define-representation (#,repr-name real integer?)
          (compose (curryr normalize-fx 'int 'frac) (curryr clamp-fx 'int 'frac)
                   (curryr real->fixed 'int 'frac) bigfloat->real)
          (compose bf (curryr fixed->real 'int 'frac))
          (compose (curryr normalize-fx 'int 'frac) (curryr ordinal->fx 'int 'frac))
          (compose (curryr fx->ordinal 'int 'frac) (curryr clamp-fx 'int 'frac))
          #,(+ int* frac* 1)
          (const #f))

        ;; Operators

        (define-operator (+ #,(op-name '+) #,repr-name #,repr-name) #,repr-name
          [fl (curry fx+ 'int 'frac)] [bf bf+] [ival #f]
          [nonffi (curry fx+ 'int 'frac)])

        (define-operator (- #,(op-name 'neg) #,repr-name) #,repr-name
          [fl (curry fx- 'int 'frac)] [bf bf-] [ival #f]
          [nonffi (curry fx- 'int 'frac)])

        (define-operator (- #,(op-name '-) #,repr-name #,repr-name) #,repr-name
          [fl (curry fx- 'int 'frac)] [bf bf-] [ival #f]
          [nonffi (curry fx- 'int 'frac)])

        (define-operator (* #,(op-name '*) #,repr-name #,repr-name) #,repr-name
          [fl (curry fx* 'int 'frac)] [bf bf*] [ival #f]
          [nonffi (curry fx* 'int 'frac)])

        (define-operator (/ #,(op-name '/) #,repr-name #,repr-name) #,repr-name
          [fl (curry fx/ 'int 'frac)] [bf bf/] [ival #f]
          [nonffi (curry fx/ 'int 'frac)])

        (define-operator (shl #,(op-name 'shl) #,repr-name integer) #,repr-name
          [fl (curry fxshl 'int 'frac)] [bf bfshl] [ival #f]
          [nonffi (curry fxshl 'int 'frac)])

        (define-operator (shr #,(op-name 'shr) #,repr-name integer) #,repr-name
          [fl (curry fxshr 'int 'frac)] [bf bfshr] [ival #f]
          [nonffi (curry fxshr 'int 'frac)])    

        (define-operator (== #,(op-name '==) #,repr-name #,repr-name) bool
          [itype '#,repr-name] [otype 'bool] ; Override number of arguments
          [fl (comparator =)] [bf (comparator bf=)] [ival #f]
          [nonffi (comparator =)])

        (define-operator (!= #,(op-name '!=) #,repr-name #,repr-name) bool
          [itype '#,repr-name] [otype 'bool] ; Override number of arguments
          [fl !=-fn] [bf bf!=-fn] [ival #f]
          [nonffi !=-fn])

        (define-operator (< #,(op-name '<) #,repr-name #,repr-name) bool
          [fl (comparator <)] [bf (comparator bf<)] [ival #f]
          [nonffi (comparator <)])

        (define-operator (> #,(op-name '>) #,repr-name #,repr-name) bool
          [itype (list 'fixed 'int 'frac)] [otype 'bool] ; Override number of arguments
          [fl (comparator >)] [bf (comparator bf>)] [ival #f]
          [nonffi (comparator >)])

        (define-operator (<= #,(op-name '<=) #,repr-name #,repr-name) bool
          [itype (list 'fixed 'int 'frac)] [otype 'bool] ; Override number of arguments
          [fl (comparator <=)] [bf (comparator bf<=)] [ival #f]
          [nonffi (comparator <=)])

        (define-operator (>= #,(op-name '>=) #,repr-name #,repr-name) bool
          [itype (list 'fixed 'int 'frac)] [otype 'bool] ; Override number of arguments
          [fl (comparator >=)] [bf (comparator bf>=)] [ival #f]
          [nonffi (comparator >=)])    

        ;; Rules

        (define-ruleset commutativity-fx (arithmetic simplify)
          #:type ([a #,repr-name] [b #,repr-name])
          [fx-commutative+     (#,(op-name '+) a b)       (#,(op-name '+) b a)]
          [fx-commutative*     (#,(op-name '*) a b)       (#,(op-name '*) b a)])

        (define-ruleset add2-fx (arithmetic simplify #,repr-name)
          #:type ([a #,repr-name])
          [fx-add2-mul   (#,(op-name '+) a a)     (#,(op-name '*) 2 a)]
          [fx-add2-shl   (#,(op-name '+) a a)     (shl.fx a 1)])

        (define-ruleset associativity-fx (arithmetic simplify)
          #:type ([a #,repr-name] [b #,repr-name] [c #,repr-name])
          [fx-associate-+r+     (#,(op-name '+) a (#,(op-name '+) b c))
                                 (#,(op-name '+) (#,(op-name '+) a b) c)]
          [fx-associate-+l+     (#,(op-name '+) (#,(op-name '+) a b) c)
                                 (#,(op-name '+) a (#,(op-name '+) b c))]
          [fx-associate-+r-     (#,(op-name '+) a (#,(op-name '-) b c))
                                 (#,(op-name '-) (#,(op-name '+) a b) c)]
          [fx-associate-+l-     (#,(op-name '+) (#,(op-name '-) a b) c)
                                 (#,(op-name '-) a (#,(op-name '-) b c))]
          [fx-associate--r+     (#,(op-name '-) a (#,(op-name '+) b c))
                                 (#,(op-name '-) (#,(op-name '-) a b) c)]
          [fx-associate--l+     (#,(op-name '-) (#,(op-name '+) a b) c)
                                 (#,(op-name '+) a (#,(op-name '-) b c))]
          [fx-associate--l-     (#,(op-name '-) (#,(op-name '-) a b) c)
                                 (#,(op-name '-) a (#,(op-name '+) b c))]
          [fx-associate--r-     (#,(op-name '-) a (#,(op-name '-) b c))
                                 (#,(op-name '+) (#,(op-name '-) a b) c)]
          [fx-associate-*r*     (#,(op-name '*) a (#,(op-name '*) b c))
                                 (#,(op-name '*) (#,(op-name '*) a b) c)]
          [fx-associate-*l*     (#,(op-name '*) (#,(op-name '*) a b) c)
                                 (#,(op-name '*) a (#,(op-name '*) b c))]
          [fx-associate-*r/     (#,(op-name '*) a (#,(op-name '/) b c))
                                 (#,(op-name '/) (#,(op-name '*) a b) c)]
          [fx-associate-*l/     (#,(op-name '*) (#,(op-name '/) a b) c)
                                 (#,(op-name '/) (#,(op-name '*) a c) b)]
          [fx-associate-/r*     (#,(op-name '/) a (#,(op-name '*) b c))
                                 (#,(op-name '/) (#,(op-name '/) a b) c)]
          [fx-associate-/l*     (#,(op-name '/) (#,(op-name '*) b c) a)
                                 (#,(op-name '/) b (#,(op-name '/) a c))]
          [fx-associate-/r/     (#,(op-name '/) a (#,(op-name '/) b c))
                                 (#,(op-name '*) (#,(op-name '/) a b) c)]
          [fx-associate-/l/     (#,(op-name '/) (#,(op-name '/) b c) a)
                                 (#,(op-name '/) b (#,(op-name '*) a c))])

        (define-ruleset distributivity-fx (arithmetic simplify)
          #:type ([a #,repr-name] [b #,repr-name] [c #,repr-name])
          [fx-distribute-lft-in      (#,(op-name '*) a (#,(op-name '+) b c))
                                      (#,(op-name '+) (#,(op-name '*) a b) (#,(op-name '*) a c))]
          [fx-distribute-rgt-in      (#,(op-name '*) a (#,(op-name '+) b c))
                                      (#,(op-name '+) (#,(op-name '*) b a) (#,(op-name '*) c a))]
          [fx-distribute-lft-out     (#,(op-name '+) (#,(op-name '*) a b) (#,(op-name '*) a c))
                                      (#,(op-name '*) a (#,(op-name '+) b c))]
          [fx-distribute-lft-out--   (#,(op-name '-) (#,(op-name '*) a b) (#,(op-name '*) a c))
                                      (#,(op-name '*) a (#,(op-name '-) b c))]
          [fx-distribute-rgt-out     (#,(op-name '+) (#,(op-name '*) b a) (#,(op-name '*) c a))
                                      (#,(op-name '*) a (#,(op-name '+) b c))]
          [fx-distribute-rgt-out--   (#,(op-name '-) (#,(op-name '*) b a) (#,(op-name '*) c a))
                                      (#,(op-name '*) a (#,(op-name '-) b c))]
          [fx-distribute-lft1-in     (#,(op-name '+) (#,(op-name '*) b a) a)
                                      (#,(op-name '*) (#,(op-name '+) b 1) a)]
          [fx-distribute-rgt1-in     (#,(op-name '+) a (#,(op-name '*) c a))
                                      (#,(op-name '*) (#,(op-name '+) c 1) a)])

        (define-ruleset id-reduce-fx (arithmetic simplify)
          #:type ([a #,repr-name])
          [fx-remove-double-div  (#,(op-name '/) 1 (#,(op-name '/) 1 a))     a]
          [fx-rgt-mult-inverse   (#,(op-name '*) a (#,(op-name '/) 1 a))     1]
          [fx-lft-mult-inverse   (#,(op-name '*) (#,(op-name '/) 1 a) a)     1]
          [fx-+-inverses         (#,(op-name '-) a a)               0]
          [fx-*-inverses         (#,(op-name '/) a a)               1]
          [fx-div0               (#,(op-name '/) 0 a)               0]
          [fx-mul0-lft           (#,(op-name '*) 0 a)               0]
          [fx-mul0-rgt           (#,(op-name '*) a 0)               0])

        (define-ruleset id-fx (arithmetic simplify)
          #:type ([a #,repr-name])
          [+fx-lft-identity-reduce     (#,(op-name '+) 0 a)     a]
          [+fx-rgt-identity-reduce     (#,(op-name '+) a 0)     a]
          [-fx-rgt-identity-reduce     (#,(op-name '-) a 0)     a]
          [*fx-lft-identity-reduce     (#,(op-name '*) 1 a)     a]
          [*fx-rgt-identity-reduce     (#,(op-name '*) a 1)     a]
          [/fx-rgt-identity-reduce     (#,(op-name '/) a 1)     a])

        (define-ruleset unid-fx (arithmetic)
          #:type ([a #,repr-name])
          [+fx-lft-identity-expand    a    (#,(op-name '+) 0 a)]
          [+fx-rgt-identity-expand    a    (#,(op-name '+) a 0)]
          [-fx-rgt-identity-expand    a    (#,(op-name '-) a 0)]
          [*fx-lft-identity-expand    a    (#,(op-name '*) 1 a)]
          [*fx-rgt-identity-expand    a    (#,(op-name '*) a 1)]
          [/fx-rgt-identity-expand    a    (#,(op-name '/) a 1)])

    ))]))

(define-fixed-point 3 4)
(define-fixed-point 15 16)

; Debugging
; (require herbie/syntax/syntax)
; (for ([(name op) (in-hash parametric-operators)])
;   (printf "~a : ~a\n" name op))