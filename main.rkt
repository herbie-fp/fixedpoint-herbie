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

; Uniquify each operator
(define (add-suffix x int frac)
  (string->symbol 
    (string-append (~s x) ".fx" (~s int) "." (~s frac))))

;; Generator for fixed-point representations
(define (generate-fixed-point name)
  (match name
   [(list 'fixed int frac)
    (define (add-suffix x)
      (string->symbol 
        (string-append (~s x) ".fx" (~s int) "." (~s frac))))
  
    ; Representation
    (register-representation! name 'real integer?
      (compose (curryr normalize-fx int frac) (curryr clamp-fx int frac)
                (curryr real->fixed int frac) bigfloat->real)
      (compose bf (curryr fixed->real int frac))
      (compose (curryr normalize-fx int frac) (curryr ordinal->fx int frac))
      (compose (curryr fx->ordinal int frac) (curryr clamp-fx int frac))
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

    (register-operator! '= (add-suffix '=) (list name name) name 
      (list (cons 'itype name) (cons 'otype name)  ; override number of arguments
            (cons 'fl (comparator =)) (cons 'bf (comparator bf=))
            (cons 'ival #f) (cons 'nonffi (comparator =))))

    (register-operator! '!= (add-suffix '!=) (list name name) name 
      (list (cons 'itype name) (cons 'otype name)  ; override number of arguments
            (cons 'fl !=-fn) (cons 'bf bf!=-fn)
            (cons 'ival #f) (cons 'nonffi !=-fn)))

    (register-operator! '< (add-suffix '<) (list name name) name 
      (list (cons 'itype name) (cons 'otype name)  ; override number of arguments
            (cons 'fl (comparator <)) (cons 'bf (comparator bf<))
            (cons 'ival #f) (cons 'nonffi (comparator <))))

    (register-operator! '> (add-suffix '>) (list name name) name 
      (list (cons 'itype name) (cons 'otype name)  ; override number of arguments
            (cons 'fl (comparator >)) (cons 'bf (comparator bf>))
            (cons 'ival #f) (cons 'nonffi (comparator >))))

    (register-operator! '<= (add-suffix '<=) (list name name) name 
      (list (cons 'itype name) (cons 'otype name)  ; override number of arguments
            (cons 'fl (comparator <=)) (cons 'bf (comparator bf<=))
            (cons 'ival #f) (cons 'nonffi (comparator <=))))

    (register-operator! '>= (add-suffix '>=) (list name name) name 
      (list (cons 'itype name) (cons 'otype name)  ; override number of arguments
            (cons 'fl (comparator >=)) (cons 'bf (comparator bf>=))
            (cons 'ival #f) (cons 'nonffi (comparator >=))))

    #t]
   [_ #f]))

(register-generator! generate-fixed-point)