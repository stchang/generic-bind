#lang racket
(require rackunit)
(require "../generic-bind.rkt"
         syntax/parse ; need this so that ~seq and syntax-classes are bound
         (for-syntax (only-in "../stx-utils.rkt" syntax-local-match-introduce-available?)))

;; sugar for contract testing
(define-check (check-exn-contract thunk)
  (check-exn exn:fail:contract? thunk))

(define-check (check-exn-arity thunk)
  (check-exn exn:fail:contract:arity? thunk))

;; define non-fns
(test-case "define non-fns"
  (~define x1 (+ 1 2))
  (check-equal? x1 3)
  
  (~define x2 (list 1 2))
  (check-equal? x2 (list 1 2))
  
  (~define ($ (list y1 y2)) (list 10 20))
  (check-equal? y1 10) (check-equal? y2 20)
  (~define ($ (list (cons y3 y4) y5)) (list (cons 20 30) 40))
  (check-equal? y3 20) (check-equal? y4 30) (check-equal? y5 40)
  (~define (⋈ v1 v2) (values 101 202))
  (check-equal? v1 101) (check-equal? v2 202)
  (~define (⋈ ($ (list x4 x5)) x6) (values (list 99 999) 9999))
  (check-equal? x4 99)(check-equal? x5 999)(check-equal? x6 9999)
  
  (struct A (x y))
  (~define ($ (A x7 y7)) (A 101 202))
  (check-equal? x7 101) (check-equal? y7 202)
  )

;; $stx
(test-case "$stx"
  (~define ($stx ((~seq kw:keyword arg:expr) ...)) #'(#:a 1 #:b 2 #:c 3))
  (check-equal? (syntax->datum #'(kw ...)) '(#:a #:b #:c))
  (check-equal? (syntax->datum #'(arg ...)) '(1 2 3))
  (check-equal? (syntax->datum #'([kw . arg] ...)) '([#:a . 1] [#:b . 2] [#:c . 3]))
  )

;; $and
(test-case "$and"
  (~define ($and lst ($ (list lst-0 lst-1))) '(0 1))
  (check-equal? lst '(0 1))
  (check-equal? lst-0 0)
  (check-equal? lst-1 1)
  (~define ($and (~vs ($and lst0 lst_0) lst1) (~vs ($list lst0-0) ($list lst1-0 lst1-1)))
           (values '(lst0-0) '(lst1-0 lst1-1)))
  (check-equal? lst0 '(lst0-0))
  (check-equal? lst_0 '(lst0-0))
  (check-equal? lst1 '(lst1-0 lst1-1))
  (check-equal? lst0-0 'lst0-0)
  (check-equal? lst1-0 'lst1-0)
  (check-equal? lst1-1 'lst1-1)
  )

;; $c
(test-case "$c"
  (~define ($c x real?) 3.14159)
  (check-equal? x 3.14159)
  (check-exn-contract (thunk (~define ($c x real?) "not a real number") (void)))
  (~define ($c ($list f0) (list/c (-> real? real?)))
           (list (λ (x) x)))
  (check-equal? (f0 3.14159) 3.14159)
  (check-exn-contract (thunk (f0 "not a real number"))))
(test-case "~define/contract"
  (~define/contract x real? 3.14159)
  (check-equal? x 3.14159)
  (check-exn-contract (thunk (~define/contract x real? "not a real number") (void)))
  (~define/contract (f x) (-> real? real?)
                    x)
  (check-equal? (f 3.14159) 3.14159)
  (check-exn-contract (thunk (f "not a real number")))
  (~define/contract ($list f0) (list/c (-> real? real?))
                    (list (λ (x) x)))
  (check-equal? (f0 3.14159) 3.14159)
  (check-exn-contract (thunk (f0 "not a real number"))))

(define-syntax do-if-syntax-local-match-introduce-available
  (lambda (stx)
    (if syntax-local-match-introduce-available?
        (syntax-case stx ()
          [(_ stuff ...) #'(begin stuff ...)])
        #'(begin))))

;; nested generic binding instances as match-patterns
(do-if-syntax-local-match-introduce-available
 (test-case "nested generic binding instances as match-patterns"
   (~define ($list ($list x)) '((1)))
   (check-equal? x 1)
   (~define ($ (list ($ (list y z)))) '((2 3)))
   (check-equal? y 2)
   (check-equal? z 3)
   (check-exn exn:misc:match?
    (λ ()
      (~define ($list ($list xx)) '(1))
      xx))
   (check-exn exn:misc:match?
    (λ ()
      (~define ($ (list ($ (list yy zz)))) '(2 3))
      (+ yy zz)))
   (~define ($list x1 ($list x2 x3 ($list ($stx ((~seq kw:keyword arg:expr) ...))) ($list $null)))
            (list 1 (list 2 3 (list #'(#:a 1 #:b 2 #:c 3)) (list null))))
   (check-equal? x1 1)
   (check-equal? x2 2)
   (check-equal? x3 3)
   (check-equal? (syntax->datum #'(kw ...))
                 '(#:a #:b #:c))
   (check-equal? (syntax->datum #'(arg ...))
                 '(1 2 3))
   (~struct A (one two three))
   (~struct B (four five))
   (~struct C (six seven))
   (~struct D (eight nine))
   (~define ($A ($B four five) ($C ($D eight nine) seven) three)
     (A (B 4 5) (C (D 8 9) 7) 3))
   (check-equal? three 3)
   (check-equal? four 4)
   (check-equal? five 5)
   (check-equal? seven 7)
   (check-equal? eight 8)
   (check-equal? nine 9)
   (check-exn exn:misc:match?
     (λ ()
       (~define ($A ($B fou fiv) ($C ($D eigh nin) seve) thre)
         (A (B 4 5) (C 6 7) 3))
       fou))
   )
 )

;; define fns
(test-case "define fns"
  (~define (f1 x [y 10] #:z [z 0]) (+ x y z))
  (check-equal? (f1 100) 110)
  (check-equal? (f1 100 200) 300)
  (check-equal? (f1 100 200 #:z 300) 600)
  
  (~define (f3 . rst) rst)
  (check-equal? (f3 1 2 3) (list 1 2 3))
  (check-equal? (f3) null)
  (~define (f4 x y . rst) (cons x (cons y rst)))
  (check-equal? (f4 1 2 3 4 5 6) (list 1 2 3 4 5 6))
  (check-equal? (f4 1 2) (list 1 2))
  
  (~define (f2 ($ (list x y))) (- x y))
  (check-equal? (f2 (list 145 45)) 100)
  (~define (g1 ($ (list (list a b) y ...))) (apply + a b y))
  (check-equal? (g1 (list (list 101 202) 303)) 606)
  
  (~define (f5 ($ (list (list x y) z)) . rst) (cons x (cons y (cons z rst))))
  (check-equal? (f5 (list (list 1 2) 3)) (list 1 2 3))
  (check-equal? (f5 (list (list 1 2) 3) 4 5) (list 1 2 3 4 5))
  )

(test-case "curried function shorthand"
  ;; plain identifiers
  (~define ((f1 x) y) (+ x y))
  (check-equal? ((f1 1) 2) 3)
  
  ;; gen-bind for outer argument
  (~define ((f2 x) ($list y)) (+ x y))
  (check-equal? ((f2 1) '(2)) 3)
  
  ;; gen-bind for inner argument
  (~define ((f3 ($list x)) y) (+ x y))
  (check-equal? ((f3 '(1)) 2) 3)
  
  ;; gen-bind for inner argument binding stuff for default outer argument
  (~define ((f4 ($list x)) [y x]) (+ x y))
  (check-equal? ((f4 '[1]) 2) 3)
  (check-equal? ((f4 '[1])) 2)
  
  ;; gen-bind for inner argument binging stuff for gen-bind outer argument
  (~define ((f5 ($list x)) [($ (and y (== x))) x]) (+ x y))
  (check-equal? ((f5 '[1]) 1) 2)
  (check-equal? ((f5 '[1])) 2)
  (check-exn exn:misc:match? (thunk ((f5 '[1]) 2)))
  )

;; generic-bind in keyword or arg-with-default positions
(test-case "define fns generic-bind in keyword or arg-with-default positions"
  (~define (fkw1 [($list x y) (list 1 2)]) (+ x y 10))
  (check-equal? (fkw1) 13)
  (check-equal? (fkw1 (list 10 20)) 40)
  (check-exn exn:misc:match? (λ () (fkw1 10)))
  
  (~define (fkw2 #:A ($list x y)) (+ x y 10))
  (check-equal? (fkw2 #:A (list 1 2)) 13)
  
  (~define (fkw3 #:B [($list x y) (list 1 2)]) (+ x y 10))
  (check-equal? (fkw3 #:B (list 10 20)) 40)
  (check-exn exn:misc:match? (λ () (fkw3 #:B 10)))
  )



;; test non-list pats
(test-case "test non-list pats"
  (~define (gg ($ xxx)) (add1 xxx))
  (check-equal? (gg 10001) 10002)
  (~define (ggg ($ _)) 12345)
  (check-equal? (ggg 5432) 12345)
  (~define (gggg ($ 11111)) 22222)
  (check-exn exn:misc:match? (λ () (gggg 111))) ; match fail
  (check-equal? (gggg 11111) 22222)
  )

;; lambda tests
(test-case "lambda tests"
  (check-equal? ((~λ (x) x) 111) 111)
  (check-equal? ((~λ rst rst) 1 2 3) (list 1 2 3))
  (check-equal? ((~λ (x [y 0] #:z z #:a [a 10]) (+ x y z a)) 1 #:z 10) 21)
  (check-equal? ((~λ (x [y 0] #:z z #:a [a 10]) (+ x y z a)) 1 22 #:z 10) 43)
  (check-equal? ((~λ (x [y 0] #:z z #:a [a 10]) (+ x y z a)) 1 #:z 10 #:a 111) 122)
  (check-equal? ((~lambda (x [y 0] #:z z #:a [a 10]) (+ x y z a)) 1 #:z 10 #:a 111) 122)
  
  ;; single arg
  (check-equal? ((~λ ($ (list x y)) (+ x y)) (list 14 56)) 70)
  (check-equal? ((~λ (~vs v1 v2) (+ ~vs v1 v2)) 1 2 3) 6) ;; shadow ~vs
  ; (~λ ((~vs v1 v2)) (+ v1 v2)) ; error
  (check-equal? ((~λ (($ (list x y)) ($ (cons a b))) (+ a b x y)) 
                 (list 1 2) (cons 3 4))
                10)
  (struct A (x y))
  (check-equal? ((~λ (x y ($ (A a b))) (+ x y a b)) 10 20 (A 30 40)) 100)
  
  ;; kw and default-arg lambda examples
  (check-equal? ((~λ ([($: x xs) (list 1 2)]) (+ x (length xs) 20))) 22)
  (check-equal? ((~λ ([($: x xs) (list 1 2)]) (+ x (length xs) 20)) (list 1 2 3 4)) 24)
  (check-equal? ((~λ (#:C ($: x xs)) (append xs (list x))) #:C (list 1 2 3)) (list 2 3 1))
  (check-equal? ((~λ (#:D [($: x xs) (list 10 20 30)]) (append xs (list x)))) (list 20 30 10))
  (check-equal? ((~λ (#:D [($: x xs) (list 10 20 30)]) (append xs (list x))) #:D (list 1 2 3)) (list 2 3 1))
  )

;; example for spurious defines in nested generic binds
(~define (⋈ ($ (cons x20 x21)) y22) (values (cons 56 67) 78))
(check-equal? x20 56) (check-equal? x21 67) (check-equal? y22 78)

;; ~let
(test-case "~let"
  ;; (2013-08-25: doesn't support values)
  ;; (2013-08-26: values works)
  ;; some corner cases
  (define x55 155)
  (check-equal? (~let ([x55 (add1 x55)]) x55) 156)
  (define x56 156)
  (check-equal? (~let ([x56 157] [x57 x56]) x57) 156)
  
  (check-equal? (~let ([($ (list x58 x59)) (list 158 159)]) (list x59 x58))
                (list 159 158))
  
  (check-equal? (~let ([($stx ((~seq kw:keyword arg:expr) ...))
                        #'(#:a 1 #:b 2 #:c 3)])
                  (syntax->datum #'(kw ...)))
                '(#:a #:b #:c))
  
  ;; values
  (check-equal? (~let ([(~vs v17 v27) (values 17 27)]) (+ v17 v27)) 44)
  (check-equal? (~let ([(~vs ($ (list x18 y18)) v18) (values (list 18 19) 20)])
                  (+ x18 y18 v18))
                57)
  
  ;; check dup ids
  ;(~let ([x 1] [x 2]) x) ; fail
  ;(~let ([($ (list x y)) (list 1 2)] [x 3]) x) ; fail
  ;(~let ([x 3] [($ (list x y)) (list 1 2)]) x) ; fail
  ;(~let ([($ (list x y)) (list 1 2)] [($ (list x y)) (list 3 4)]) (list x y)) ; fail
  ;(~let ([($ (list x y)) (list 1 2)] [(~vs x y) (list 3 4)]) (list x y)) ; fail
  ;; dups in single match pat is ok
  (check-equal? (~let ([($ (list x x)) (list 2 2)]) x) 2)
  
  ;; check ~let bindings cant see previous bindings
  (check-equal? (let ([x 12345]) (~let ([x 40] [y x]) y)) 12345)
  (check-equal? (let ([x 12345]) (~let ([x 40] [y 1] [z x]) z)) 12345)
  (check-equal? (let ([x 12345] [y 5432]) (~let ([x 40] [y 23] [z y]) z)) 5432)
  
  ;; check rec defs fail
  ;(~let ([x59 x59]) x59) ; fail
  ; (~let ([f (λ (x) (if (zero? x) 1 (* x (f (sub1 x)))))]) (f 10)) ; fail
  
  ;; named let
  (check-equal? 20
                (~let L ([($ (list x y ...)) (list 1 2 3 4 5)] [n 4])
                  (if (zero? n) 0
                      (+ n x (L y (sub1 n))))))
  
  ;; check named let corner cases
  ;(let loop ([x 1] [x 2]) x) ; duplicate id error
  ;(~let loop ([x 1] [x 2]) x) ; duplicate id error (but references define)
  (define x8 109)
  (check-equal? 109 (let loop ([x8 1] [y x8]) y))
  (check-equal? 109 (~let loop ([x8 1] [y x8]) y))
  )

;; ~let*
(test-case "~let*"
  (check-equal? (~let* ([x 46436] [y x]) y) 46436)
  (check-equal? (~let* ([($ (list x y)) (list 1010 2020)] [z (+ x y)]) z) 3030)
  (check-equal? (~let* ([(⋈ v1 v2) (values 4040 5050)] [z (+ v1 v2)]) z) 9090)
  (check-equal? (~let* ([(⋈ v3 ($ (list xy yx))) (values 202 (list 303 404))] 
                        [z (+ v3 (- yx xy))])
                  z)
                303)
  (check-equal? (~let* ([x 1122] 
                        [(⋈ ($ (cons ab bc)) ($ (list-rest y ys)))
                         (values (cons x (* x 2)) (list x (add1 x) (sub1 x)))])
                  (+ (- bc ab) (- y (+ (car ys) (cadr ys))))) 
                0)
  (check-equal? (~let* ([x 101] [x 202]) x) 202)
  )

;; ~letrec*
(require math/number-theory) ; factorial
(test-case "~letrec*"
  ;(check-exn exn:fail? (~letrec ([x x]) x)) ; this used to work but now fails
  (check-equal? (~letrec ([f (λ (x) (if (zero? x) 1 (* x (f (sub1 x)))))]) (f 10)) 
                (factorial 10))
  (check-true
   (~letrec ([evn? (λ (x) (if (zero? x) true (od? (sub1 x))))]
             [od? (λ (x) (if (zero? x) false (evn? (sub1 x))))])
     (evn? 10)))
  (check-true
   (~letrec ([(⋈ evn? od?)
              (values (λ (x) (if (zero? x) true (od? (sub1 x))))
                      (λ (x) (if (zero? x) false (evn? (sub1 x)))))])
     (evn? 10)))
  (check-true
   (~letrec ([($ (list evn? od?))
              (list (λ (x) (if (zero? x) true (od? (sub1 x))))
                    (λ (x) (if (zero? x) false (evn? (sub1 x)))))])
     (and (od? 101) (evn? 10))))
  (check-true
   (~letrec ([(⋈ evn? ($ (list od?)))
              (values (λ (x) (if (zero? x) true (od? (sub1 x))))
                      (list (λ (x) (if (zero? x) false (evn? (sub1 x))))))])
     (and (od? 101) (evn? 10))))
  )

;; test list matching forms
(test-case "test list matching forms"
  (~define (f20 ($list x y z : xs)) (+ x y z (length xs)))
  (check-equal? (f20 (list 10 20 30 40 50 60 70 80 90)) 66)
  (~define (f21 ($: x xs)) (+ x (length xs)))
  (check-equal? (f21 (list 10 20 30 40 50 60 70 80 90)) 18)
  
  
  (check-equal? ((~lambda ($: x xs) (append xs (list x))) (list 1 2 3 4 5)) (list 2 3 4 5 1))
  (check-equal? ((~lambda (f ($: x xs)) (cons (f x) xs)) add1 (list 1 2 3 4)) (list 2 2 3 4))
  (check-equal? ((~lambda (f ($list)) (f 1)) add1 null) 2)
  (check-exn exn:misc:match? (thunk ((~lambda (f ($list)) (f 1)) add1 (list 1 2 3))))
  )

;; ~case-lambda
(test-case "~case-lambda, etc."
  (define casemap1 
    (~case-lambda [(f $null) null]
                  [(f ($: x xs)) (cons (f x) (casemap1 f xs))]))
  (check-equal? (casemap1 add1 null) null)
  (check-equal? (casemap1 add1 (list 1)) (list 2))
  (check-equal? (casemap1 add1 (list 1 2 3 4)) (list 2 3 4 5))
  
  (~case-define new-map1 
                [(f $null) null]
                [(f ($: x xs)) (cons (f x) (new-map1 f xs))])
  (check-equal? (new-map1 add1 null) null)
  (check-equal? (new-map1 add1 (list 1)) (list 2))
  (check-equal? (new-map1 add1 (list 1 2)) (list 2 3))
  
  (~case-def new-map [f $null → null]
             [f ($: x xs) → (cons (f x) (new-map f xs))])
  (check-equal? (new-map add1 null) null)
  (check-equal? (new-map add1 (list 1)) (list 2))
  (check-equal? (new-map add1 (list 1 2)) (list 2 3))
  
  (~case-define new-filter1 
                [(p? $null) null]
                [(p? ($: x xs)) (if (p? x) (cons x (new-filter1 p? xs)) (new-filter1 p? xs))])
  (check-equal? (new-filter1 even? null) null)
  (check-equal? (new-filter1 even? (list 1)) null)
  (check-equal? (new-filter1 even? (list 1 2 3 4 5)) (list 2 4))
  
  (~case-def new-filter [p? $null → null]
             [p? ($: (? p? x) xs) → (cons x (new-filter p? xs))]
             [p? ($: x xs) → (new-filter p? xs)])
  (check-equal? (new-filter even? null) null)
  (check-equal? (new-filter even? (list 1)) null)
  (check-equal? (new-filter even? (list 1 2 3 4 5)) (list 2 4))
  
  (~case-define new-foldl1
                [(f base $null) base]
                [(f base ($: x xs)) (new-foldl1 f (f x base) xs)])
  (check-equal? (new-foldl1 - 0 null) 0)
  (check-equal? (new-foldl1 - 0 (list 1)) 1)
  (check-equal? (new-foldl1 - 0 (list 1)) (foldl - 0 (list 1)))
  (check-equal? (new-foldl1 - 0 (list 1 2)) 1)
  (check-equal? (new-foldl1 - 0 (list 1 2)) (foldl - 0 (list 1 2)))
  
  (~case-def new-foldl [f base $null → base]
             [f base ($: x xs) → (new-foldl f (f x base) xs)])
  (check-equal? (new-foldl - 0 null) 0)
  (check-equal? (new-foldl - 0 (list 1)) 1)
  (check-equal? (new-foldl - 0 (list 1)) (foldl - 0 (list 1)))
  (check-equal? (new-foldl - 0 (list 1 2)) 1)
  (check-equal? (new-foldl - 0 (list 1 2)) (foldl - 0 (list 1 2)))
  
  (~case-define new-foldr1
                [(f base $null) base]
                [(f base ($: x xs)) (f x (new-foldr1 f base xs))])
  (check-equal? (new-foldr1 - 0 null) 0)
  (check-equal? (new-foldr1 - 0 (list 1)) 1)
  (check-equal? (new-foldr1 - 0 (list 1)) (foldr - 0 (list 1)))
  (check-equal? (new-foldr1 - 0 (list 1 2)) -1)
  (check-equal? (new-foldr1 - 0 (list 1 2)) (foldr - 0 (list 1 2)))
  
  (~case-def new-foldr [f base $null → base]
             [f base ($: x xs) → (f x (new-foldr f base xs))])
  (check-equal? (new-foldr - 0 null) 0)
  (check-equal? (new-foldr - 0 (list 1)) 1)
  (check-equal? (new-foldr - 0 (list 1)) (foldr - 0 (list 1)))
  (check-equal? (new-foldr - 0 (list 1 2)) -1)
  (check-equal? (new-foldr - 0 (list 1 2)) (foldr - 0 (list 1 2)))
  )


;; ~for -----------------------------------------------------------------------
(require racket/generator)
(test-case "~for, etc."
  (check-equal? (~for/list ([($ (list x y)) '((1 2) (3 4) (5 6))]) (list y x))
                '((2 1) (4 3) (6 5)))
  (check-equal? 
   (~for/list 
       ([($ (list x y)) '((1 2) (3 4))]
        [($ (list a b)) '((5 6) (7 8))])
     (list y x b a))
   '((2 1 6 5) (4 3 8 7)))
  (check-equal? 
   (~for/list 
       ([($ (list x y)) '((1 2) (3 4))]
        [($ (list a b)) '((5 6) (7 8))]
        #:when (= x 1)) 
     (list y x b a))
   '((2 1 6 5)))
  (check-equal? 
   (~for/list 
       ([($ (list x y)) '((1 2) (3 4))]
        [($ (list a b)) '((5 6) (7 8))]
        #:unless (= x 1)) 
     (list y x b a))
   '((4 3 8 7)))
  (check-equal? 
   (~for/list 
       ([($ (list x y)) '((2 2) (1 4) (1 4))]
        [($ (list a b)) '((5 6) (6 7) (7 8))]
        #:when (= x 1)
        #:unless (= a 6))
     (list y x b a))
   '((4 1 8 7)))
  (check-equal? (~for/list ([x '(1 2 3)] #:break (= x 2)) x) '(1))
  (check-equal? (~for/list ([x '(1 2 3)] #:final (= x 2)) x) '(1 2))
  (check-equal? (~for*/list ([x '(1 2 3)]) #:break (= x 2) x) '(1))
  (check-equal? (~for*/list ([x '(1 2 3)]) #:final (= x 2) x) '(1 2))
  
  (check-equal?
   (~for/list ([x '(1 2 3)] #:when #t [y '(4 5 6)] #:when #t) (list x y))
   '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)))
  (check-equal?
   (~for*/list ([x '(1 2 3)] [y '(4 5 6)]) (list x y))
   '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)))
  
  (check-equal?
   (~for/list ([($ (list x y)) (in-generator (let loop ([n 3])
                                               (unless (zero? n)
                                                 (yield (list n (add1 n)))
                                                 (loop (sub1 n)))))])
     (list x y))
   '((3 4) (2 3) (1 2)))
  ;; sequence-generate is broken?
  ;(check-equal?
  ; (~for/list ([(~vs x y) (in-generator (let loop ([n 3])
  ;                                        (unless (zero? n)
  ;                                          (yield n (add1 n))
  ;                                          (loop (sub1 n)))))])
  ;   (list x y))
  ; '((3 4) (2 3) (1 2)))
  
  ;; for/fold
  (check-equal?
   (~for/fold ([sum 0]) ([x '(1 2 3 4 5 6)]) (+ x sum))
   21)
  
  (check-equal?
   (~for/fold ([sum 0]) ([x '(1 2 3 4 5 6)]) (+ x sum))
   (for/fold ([sum 0]) ([x '(1 2 3 4 5 6)]) (+ x sum)))

  (let ([out (open-output-string)])
    (check-false (~for/and ([x (list #t #f #t)]) (displayln x out) x))
    (check-equal? (get-output-string out) "#t\n#f\n")
    (close-output-port out))
  
  (check-equal?
   (~for*/list ([($ (list x y)) (list (list (list 1 2 3) (list 4 5 6))
                                      (list (list 10 11 12) (list 13 14 15)))]
                [a x] [b y])
     (list a b))
   '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6) (10 13)
           (10 14) (10 15) (11 13) (11 14) (11 15) (12 13) (12 14) (12 15)))
  (check-equal? 
   (~for*/list ([($ (list x y)) (list (list (list 1 2 3) (list 4 5 6))
                                      (list (list 10 11 12) (list 13 14 15)))]
                [a x] [b y])
     (list a b))
   (for*/list ([lst (list (list (list 1 2 3) (list 4 5 6))
                          (list (list 10 11 12) (list 13 14 15)))]
               [a (car lst)] [b (cadr lst)])
     (list a b)))
  (check-equal?
   (~for*/list ([(~vs v1 v2) (in-hash (hash 1 2 3 4))]
                [x v1] [y v2])
     (cons x y))
   '((0 . 0) (0 . 1) (0 . 0) (0 . 1) (0 . 2) (0 . 3)
             (1 . 0) (1 . 1) (1 . 2) (1 . 3) (2 . 0) (2 . 1) (2 . 2) (2 . 3)))
  (check-equal?
   (~for*/list ([(~vs v1 v2) (in-hash (hash 1 2 3 4))]
                [x v1] [y v2])
     (cons x y))
   (for*/list ([(v1 v2) (in-hash (hash 1 2 3 4))]
               [x v1] [y v2])
     (cons x y)))
  (check-equal?
   (~for*/list ([(v1 v2) (in-hash (hash 1 2 3 4))]
                [x v1] [y v2])
     (cons x y))
   (~for*/list ([(~vs v1 v2) (in-hash (hash 1 2 3 4))]
                [x v1] [y v2])
     (cons x y)))
  
  (check-equal?
   (~for*/list ([(~vs ($ (list x y)) ($ (list a b))) 
                 (in-hash (hash (list 1 2) (list 3 4) (list 5 6) (list 7 8)))]
                [c x])
     (list x y a b c))
   (for*/list ([(lst1 lst2) (in-hash (hash (list 1 2) (list 3 4) (list 5 6) (list 7 8)))]
               [c (car lst1)])
     (list (car lst1) (cadr lst1) (car lst2) (cadr lst2) c)))
  
  ;; this test tripped up my ~for implementation bc there are no "inner bindings"
  (check-equal? (~for/list ([v (in-list (list 1))]) (add1 v)) (list 2))
  ;; check that a gen-bind works in this example
  (check-equal? (~for/list ([($list x y) (in-list (list (list 1 2)))]) (list x y))
                (list (list 1 2)))
  (check-equal? (~for/list ([($ x) (in-list (list 1))]) x)
                (list 1))
  
  ;; testing #:break and #:final
  (check-equal? (~for/list ([i 4] #:break (= i 2)) i) (list 0 1))
  (check-equal? (~for/list ([i 4] #:final (= i 2)) i) (list 0 1 2))
  (check-equal? (~for/list ([i 4]) #:break (= i 2) i) (list 0 1))
  (check-equal? (~for/list ([i 4]) #:final (= i 2) i) (list 0 1 2))
  
  (check-equal? (~for*/list ([i 4] [j 2] #:break (= i 2)) (list i j))
                '((0 0) (0 1) (1 0) (1 1)))
  (check-equal? (~for*/list ([i 4] #:break (= i 2) [j 2]) (list i j))
                '((0 0) (0 1) (1 0) (1 1)))
  (check-equal? (~for/list ([i 4]  #:break (= i 2) [j 2]) (list i j))
                '((0 0) (0 1) (1 0) (1 1)))
  (check-equal? (~for*/list ([i 4] [j 2] #:break (= i 2)) (list i j))
                (for*/list ([i 4][j 2] #:break (= i 2)) (list i j)))
  
  (check-equal? (~for*/list ([i 4][j 2] #:final (= i 2)) (list i j))
                '((0 0) (0 1) (1 0) (1 1) (2 0)))
  (check-equal? (~for*/list ([i 4][j 2] #:final (= i 2)) (list i j))
                (for*/list ([i 4][j 2] #:final (= i 2)) (list i j)))
  (check-equal? (~for/list ([i 4]  #:final (= i 2) [j 2]) (list i j))
                '((0 0) (0 1) (1 0) (1 1) (2 0)))
  (check-equal? (~for/list ([i 4]  #:final (= i 2) [j 2]) (list i j))
                (for/list ([i 4]  #:final (= i 2) [j 2]) (list i j)))
  
  (check-equal? 
   (~for*/list ([i 4][j 2] #:final (= i 2) #:break (= i 2)) (list i j))
   '((0 0) (0 1) (1 0) (1 1)))
  
  (check-equal? 
   (~for*/list ([i 4][j 2] #:final (= i 2) #:break (= i 2)) (list i j))
   (for*/list ([i 4][j 2] #:final (= i 2) #:break (= i 2)) (list i j)))
  
  (check-equal? 
   (~for*/list ([i 4][j 2] #:break (= i 2) #:final (= i 2)) (list i j))
   '((0 0) (0 1) (1 0) (1 1)))
  
  (check-equal?
   (~for*/list ([i 4][j 2] #:break (= i 2) #:final (= i 2)) (list i j))
   (for*/list ([i 4][j 2] #:break (= i 2) #:final (= i 2)) (list i j)))
  
  (check-equal?
   (~for*/list ([i 4][j 2] #:final (= i 2) #:unless (= j 0)) (list i j))
   '((0 1) (1 1)))
  
  (check-equal?
   (~for*/list ([i 4][j 2] #:final (= i 2) #:unless (= j 0)) (list i j))
   (for*/list ([i 4][j 2] #:final (= i 2) #:unless (= j 0)) (list i j)))
  
  (check-equal? (~for/list ([i 4]) (define j (add1 i)) #:break (= j 3) i) 
                (list 0 1))
  (check-equal? (~for/list ([i 4]) (define j (add1 i)) #:break (= j 3) i)
                (for/list ([i 4]) (define j (add1 i)) #:break (= j 3) i))
  (check-equal? (~for/list ([i 4]) (define j (add1 i)) #:final (= j 3) i) 
                (list 0 1 2))
  (check-equal? (~for/list ([i 4]) (define j (add1 i)) #:final (= j 3) i) 
                (for/list ([i 4]) (define j (add1 i)) #:final (= j 3) i))
  (check-equal? (~for*/list ([i 4][j 2]) 
                  (define k i) #:final (= k 2)
                  (define m i) #:break (= m 2) 
                  (list i j))
                '((0 0) (0 1) (1 0) (1 1)))
  
  (check-equal? (~for*/list ([i 4][j 2]) 
                  (define k i) #:final (= k 2)
                  (define m i) #:break (= m 2) 
                  (list i j))
                (for*/list ([i 4][j 2]) 
                  (define k i) #:final (= k 2)
                  (define m i) #:break (= m 2) 
                  (list i j)))
  (check-equal? (~for*/list ([i 4][j 2]) 
                  (define m i) #:break (= m 2) 
                  (define k i) #:final (= k 2)
                  (list i j))
                '((0 0) (0 1) (1 0) (1 1)))
  (check-equal? (~for*/list ([i 4][j 2]) 
                  (define m i) #:break (= m 2) 
                  (define k i) #:final (= k 2)
                  (list i j))
                (for*/list ([i 4][j 2]) 
                  (define m i) #:break (= m 2) 
                  (define k i) #:final (= k 2)
                  (list i j)))
  
  ;; check 0 accums ----------
  ;; example due to khinsen
  (define (gen2 seq)
    (in-generator
     (let ([refs (make-hash)])
       (~for ([($: a b) seq])
         (hash-set! refs a b)
         (yield a)))))
  
  (check-equal? '(a c) (sequence->list (gen2 (list (cons 'a 'b) (cons 'c 'd)))))
  (define (gen3 seq)
    (in-generator
     (let ([refs (make-hash)])
       (~for* ([($: a b) seq])
         (hash-set! refs a b)
         (yield a)))))
  
  (check-equal? '(a c) (sequence->list (gen3 (list (cons 'a 'b) (cons 'c 'd)))))
  (check-equal? 
   null 
   (call-with-values (thunk (~for/fold () ([x '(1 2)]) (values))) (λ x x)))
  (check-equal? 
   (call-with-values (thunk (~for/fold () ([x '(1 2)]) (values))) (λ x x))
   (call-with-values (thunk (for/fold () ([x '(1 2)]) (values))) (λ x x)))
  (check-equal? 
   null 
   (call-with-values (thunk (~for*/fold () ([x '(1 2)]) (values))) (λ x x)))
  (check-equal? 
   (call-with-values (thunk (~for*/fold () ([x '(1 2)]) (values))) (λ x x))
   (call-with-values (thunk (for*/fold () ([x '(1 2)]) (values))) (λ x x)))
  ;; should still fail when wrong number of accums produced
  (check-exn exn:fail? (thunk (~for/fold () ([x '(1 2)]) x)))
  (check-exn exn:fail? (thunk (~for*/fold () ([x '(1 2)]) x)))
  
  ;; ~for and ~for* should drop any results and result should be void
  (check-equal? (void) (~for ([x (list 1 2 3)]) x))
  (check-equal? (void) (~for* ([x (list 1 2 3)]) x))
  (check-equal? (call-with-values (thunk (~for ([x (list 1 2 3)]) x)) (λ x x))
                (call-with-values (thunk (for ([x (list 1 2 3)]) x)) (λ x x)))
  (check-equal? (call-with-values (thunk (~for* ([x (list 1 2 3)]) x)) (λ x x))
                (call-with-values (thunk (for* ([x (list 1 2 3)]) x)) (λ x x)))
  
  ;; ~for/hash and friends
  (check-equal? (~for/hash ([x (list 1 2 3)] [y '(a b c)]) (values x y))
                (hash 1 'a 2 'b 3 'c))
  (check-exn-arity (thunk (~for/hash ([x (list 1 2 3)] [y '(a b c)]) x)))
  (check-exn-arity (thunk (~for/hash ([x (list 1 2 3)] [y '(a b c)]) (values x x x))))
  
  (check-equal? (~for*/hash ([x (list 1 2 3)] [y '(a b c)]) (values x y))
                (hash 1 'c 2 'c 3 'c))
  (check-exn-arity (thunk (~for*/hash ([x (list 1 2 3)] [y '(a b c)]) x)))
  (check-exn-arity (thunk (~for*/hash ([x (list 1 2 3)] [y '(a b c)]) (values x x x))))
  
  (check-equal? (~for/hasheq ([x (list 1 2 3)] [y '(a b c)]) (values x y))
                (hasheq 1 'a 2 'b 3 'c))
  (check-exn-arity (thunk (~for/hasheq ([x (list 1 2 3)] [y '(a b c)]) x)))
  (check-exn-arity (thunk (~for/hasheq ([x (list 1 2 3)] [y '(a b c)]) (values x x x))))
  
  (check-equal? (~for*/hasheq ([x (list 1 2 3)] [y '(a b c)]) (values x y))
                (hasheq 1 'c 2 'c 3 'c))
  (check-exn-arity (thunk (~for*/hasheq ([x (list 1 2 3)] [y '(a b c)]) x)))
  (check-exn-arity (thunk (~for*/hasheq ([x (list 1 2 3)] [y '(a b c)]) (values x x x))))
  
  (check-equal? (~for/hasheqv ([x (list 1 2 3)] [y '(a b c)]) (values x y))
                (hasheqv 1 'a 2 'b 3 'c))
  (check-exn-arity (thunk (~for/hasheqv ([x (list 1 2 3)] [y '(a b c)]) x)))
  (check-exn-arity (thunk (~for/hasheqv ([x (list 1 2 3)] [y '(a b c)]) (values x x x))))
  
  (check-equal? (~for*/hasheqv ([x (list 1 2 3)] [y '(a b c)]) (values x y))
                (hasheqv 1 'c 2 'c 3 'c))
  (check-exn-arity (thunk (~for*/hasheqv ([x (list 1 2 3)] [y '(a b c)]) x)))
  (check-exn-arity (thunk (~for*/hasheqv ([x (list 1 2 3)] [y '(a b c)]) (values x x x))))
  )

;; define-match-bind and ~struct tests ----------------------------------------
(test-case "define-match-bind and ~struct tests"
  (struct B (x y z))
  (define-match-bind (B x y z))
  
  (~define (bf ($B x y z)) (+ x y z))
  (check-equal? (bf (B 20 40 60)) 120)
  
  (~struct C (a b c [d #:mutable]))
  (~define (cf ($C e f g h)) (+ e f g h))
  (define c (C 9 8 7 6))
  (set-C-d! c 20)
  (check-true (C? c))
  (check-equal? (C-a c) 9)
  (check-equal? (C-b c) 8)
  (check-equal? (C-c c) 7)
  (check-equal? (C-d c) 20)
  (check-equal? (cf c) 44)
  
  ;; define-match-bind on just an id
  (define-match-bind hash-table)
  (~define ($hash-table [keys vals] ...) (hash 'a 1 'b 2 'c 3))
  (check-match keys (list-no-order 'a 'b 'c))
  (check-match vals (list-no-order 1 2 3))
  (for ([key (in-list keys)]
        [val (in-list vals)])
    (case key
      [(a) (check-equal? val 1)]
      [(b) (check-equal? val 2)]
      [(c) (check-equal? val 3)]
      [else (error "this should never happen")]))
  )

