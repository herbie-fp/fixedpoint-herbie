#lang racket

(require math/bigfloat herbie/plugin "fixedpoint.rkt")

(eprintf "Loading fixed-point support...\n")

(define (bfshl x y)
  (bf* x (bfexpt 2.bf y)))

(define (bfshr x y)
  (bf/ x (bfexpt 2.bf y)))

; parametrized declaration of a fixed-point representation, its operators, and rules
(define-syntax (define-fixed-point stx)
  (syntax-case stx ()
   [(_ int frac)
    (let* ([int* (syntax-e (cadr (syntax-e stx)))]
           [frac* (syntax-e (caddr (syntax-e stx)))]
           [repr-name (string->symbol (string-append "fixed" (number->string int*) 
                                                     "." (number->string frac*)))]
           [op-name (λ (name) 
                      (string->symbol
                        (string-append (symbol->string name) ".fx" (number->string int*) 
                                       "." (number->string frac*))))])
    #`(begin
        ;; Representations
        (define-representation (#,repr-name real integer?)
          (compose (curryr normalize-int #,(+ int* frac* 1))    ; bf->repr
                   (curry (curryr real->fixed 'int 'frac))
                   bigfloat->real)
          (compose bf (curryr fixed->real 'int 'frac))          ; repr->bf 
          (compose (curryr normalize-int #,(+ int* frac* 1))    ; ordinal->repr
                   (curryr ordinal->int #,(+ int* frac* 1)))
          (compose (curryr int->ordinal #,(+ int* frac* 1))     ; repr->ordinal
                   (curryr clamp-int #,(+ int* frac* 1)))
          #,(+ int* frac* 1)                                    ; bit width
          (const #f))                                           ; special-values

        ;; Operators

        (define-operator (+ #,(op-name '+) #,repr-name #,repr-name) #,repr-name
          [fl fx+] [bf bf+] [ival #f]
          [nonffi fx+])

        (define-operator (- #,(op-name 'neg) #,repr-name) #,repr-name
          [fl fx-] [bf bf-] [ival #f]
          [nonffi fx-])

        (define-operator (- #,(op-name '-) #,repr-name #,repr-name) #,repr-name
          [fl fx-] [bf bf-] [ival #f]
          [nonffi fx-])

        (define-operator (* #,(op-name '*) #,repr-name #,repr-name) #,repr-name
          [fl fx*] [bf bf*] [ival #f]
          [nonffi fx*])

        (define-operator (/ #,(op-name '/) #,repr-name #,repr-name) #,repr-name
          [fl fx/] [bf bf/] [ival #f]
          [nonffi fx/])

        (define-operator (shl #,(op-name 'shl) #,repr-name integer) #,repr-name
          [fl fxshl] [bf bfshl] [ival #f]
          [nonffi fxshl])

        (define-operator (shr #,(op-name 'shr) #,repr-name integer) #,repr-name
          [fl fxshr] [bf bfshr] [ival #f]
          [nonffi fxshr])        

        ;; Rules

        (define-ruleset commutativity-i32 (arithmetic simplify)
          #:type ([a #,repr-name] [b #,repr-name])
          [i32-commutative+     (#,(op-name '+) a b)       (#,(op-name '+) b a)]
          [i32-commutative*     (#,(op-name '*) a b)       (#,(op-name '*) b a)])

        (define-ruleset add2-i32 (arithmetic simplify #,repr-name)
          #:type ([a #,repr-name])
          [i32-add2-mul   (#,(op-name '+) a a)     (#,(op-name '*) 2 a)]
          [i32-add2-shl   (#,(op-name '+) a a)     (shl.i32 a 1)])

        (define-ruleset associativity-i32 (arithmetic simplify)
          #:type ([a #,repr-name] [b #,repr-name] [c #,repr-name])
          [i32-associate-+r+     (#,(op-name '+) a (#,(op-name '+) b c))         (#,(op-name '+) (#,(op-name '+) a b) c)]
          [i32-associate-+l+     (#,(op-name '+) (#,(op-name '+) a b) c)         (#,(op-name '+) a (#,(op-name '+) b c))]
          [i32-associate-+r-     (#,(op-name '+) a (#,(op-name '-) b c))         (#,(op-name '-) (#,(op-name '+) a b) c)]
          [i32-associate-+l-     (#,(op-name '+) (#,(op-name '-) a b) c)         (#,(op-name '-) a (#,(op-name '-) b c))]
          [i32-associate--r+     (#,(op-name '-) a (#,(op-name '+) b c))         (#,(op-name '-) (#,(op-name '-) a b) c)]
          [i32-associate--l+     (#,(op-name '-) (#,(op-name '+) a b) c)         (#,(op-name '+) a (#,(op-name '-) b c))]
          [i32-associate--l-     (#,(op-name '-) (#,(op-name '-) a b) c)         (#,(op-name '-) a (#,(op-name '+) b c))]
          [i32-associate--r-     (#,(op-name '-) a (#,(op-name '-) b c))         (#,(op-name '+) (#,(op-name '-) a b) c)]
          [i32-associate-*r*     (#,(op-name '*) a (#,(op-name '*) b c))         (#,(op-name '*) (#,(op-name '*) a b) c)]
          [i32-associate-*l*     (#,(op-name '*) (#,(op-name '*) a b) c)         (#,(op-name '*) a (#,(op-name '*) b c))]
          [i32-associate-*r/     (#,(op-name '*) a (#,(op-name '/) b c))         (#,(op-name '/) (#,(op-name '*) a b) c)]
          [i32-associate-*l/     (#,(op-name '*) (#,(op-name '/) a b) c)         (#,(op-name '/) (#,(op-name '*) a c) b)]
          [i32-associate-/r*     (#,(op-name '/) a (#,(op-name '*) b c))         (#,(op-name '/) (#,(op-name '/) a b) c)]
          [i32-associate-/l*     (#,(op-name '/) (#,(op-name '*) b c) a)         (#,(op-name '/) b (#,(op-name '/) a c))]
          [i32-associate-/r/     (#,(op-name '/) a (#,(op-name '/) b c))         (#,(op-name '*) (#,(op-name '/) a b) c)]
          [i32-associate-/l/     (#,(op-name '/) (#,(op-name '/) b c) a)         (#,(op-name '/) b (#,(op-name '*) a c))])

        (define-ruleset distributivity-i32 (arithmetic simplify)
          #:type ([a #,repr-name] [b #,repr-name] [c #,repr-name])
          [i32-distribute-lft-in      (#,(op-name '*) a (#,(op-name '+) b c))
                                      (#,(op-name '+) (#,(op-name '*) a b) (#,(op-name '*) a c))]
          [i32-distribute-rgt-in      (#,(op-name '*) a (#,(op-name '+) b c))
                                      (#,(op-name '+) (#,(op-name '*) b a) (#,(op-name '*) c a))]
          [i32-distribute-lft-out     (#,(op-name '+) (#,(op-name '*) a b) (#,(op-name '*) a c))
                                      (#,(op-name '*) a (#,(op-name '+) b c))]
          [i32-distribute-lft-out--   (#,(op-name '-) (#,(op-name '*) a b) (#,(op-name '*) a c))
                                      (#,(op-name '*) a (#,(op-name '-) b c))]
          [i32-distribute-rgt-out     (#,(op-name '+) (#,(op-name '*) b a) (#,(op-name '*) c a))
                                      (#,(op-name '*) a (#,(op-name '+) b c))]
          [i32-distribute-rgt-out--   (#,(op-name '-) (#,(op-name '*) b a) (#,(op-name '*) c a))
                                      (#,(op-name '*) a (#,(op-name '-) b c))]
          [i32-distribute-lft1-in     (#,(op-name '+) (#,(op-name '*) b a) a)
                                      (#,(op-name '*) (#,(op-name '+) b 1) a)]
          [i32-distribute-rgt1-in     (#,(op-name '+) a (#,(op-name '*) c a))
                                      (#,(op-name '*) (#,(op-name '+) c 1) a)])

        (define-ruleset id-reduce-i32 (arithmetic simplify)
          #:type ([a #,repr-name])
          [i32-remove-double-div  (#,(op-name '/) 1 (#,(op-name '/) 1 a))     a]
          [i32-rgt-mult-inverse   (#,(op-name '*) a (#,(op-name '/) 1 a))     1]
          [i32-lft-mult-inverse   (#,(op-name '*) (#,(op-name '/) 1 a) a)     1]
          [i32-+-inverses         (#,(op-name '-) a a)               0]
          [i32-*-inverses         (#,(op-name '/) a a)               1]
          [i32-div0               (#,(op-name '/) 0 a)               0]
          [i32-mul0-lft           (#,(op-name '*) 0 a)               0]
          [i32-mul0-rgt           (#,(op-name '*) a 0)               0])

        (define-ruleset id-i32 (arithmetic simplify)
          #:type ([a #,repr-name])
          [+i32-lft-identity-reduce     (#,(op-name '+) 0 a)     a]
          [+i32-rgt-identity-reduce     (#,(op-name '+) a 0)     a]
          [-i32-rgt-identity-reduce     (#,(op-name '-) a 0)     a]
          [*i32-lft-identity-reduce     (#,(op-name '*) 1 a)     a]
          [*i32-rgt-identity-reduce     (#,(op-name '*) a 1)     a]
          [/i32-rgt-identity-reduce     (#,(op-name '/) a 1)     a])

        (define-ruleset unid-i32 (arithmetic)
          #:type ([a #,repr-name])
          [+i32-lft-identity-expand    a    (#,(op-name '+) 0 a)]
          [+i32-rgt-identity-expand    a    (#,(op-name '+) a 0)]
          [-i32-rgt-identity-expand    a    (#,(op-name '-) a 0)]
          [*i32-lft-identity-expand    a    (#,(op-name '*) 1 a)]
          [*i32-rgt-identity-expand    a    (#,(op-name '*) a 1)]
          [/i32-rgt-identity-expand    a    (#,(op-name '/) a 1)])

    ))]))

(define-fixed-point 15 16)

(require herbie/syntax/syntax)
(for ([(name op) (in-hash parametric-operators)])
  (printf "~a : ~a\n" name op))