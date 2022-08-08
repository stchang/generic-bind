#lang scribble/manual
@(require scribble/eval
          "version-utils.rkt"
          (for-label generic-bind racket syntax/parse))

@title{Racket Generic Binding Forms}

@defmodule[generic-bind]

This library implements new Racket binding forms (ie, @racket[~define], @racket[~lambda], @racket[~let], etc.) that support generic binding instances. A generic binding instance is a syntax object that implements an "interface" (currently just some syntax properties) that the binding forms then use to determine how to proceed with the binding. 

This moves the binding "logic" to the binding site itself and enables one binding form to support many different binding modes (ie, @racket[match] or @racket[values]), rather than have to manually implement a version of each binding form for all the different combinations (ie, @racket[define-values], @racket[match-define], @racket[match-define-values], etc.). 

The hope is that the forms in this library can be used in place of their analogous Racket counterparts.

@(define the-eval (make-base-eval))
@(the-eval '(require generic-bind racket/match syntax/parse))

@author[@author+email["Stephen Chang" "stchang@racket-lang.org" #:obfuscate? #t]
        @author+email["Alexander Knauth" "alexander@knauth.org" #:obfuscate? #t]]

@; Generic Binding Instances --------------------------------------------------
@section{Generic Binding Instances}

The forms described in this section may be used only in binding positions of binding forms that support generic bindings. Any other use is invalid. For examples, see the next section, @secref{binding-forms}.

Generic binding instances are further separated into two categories:
@itemize[@item{bindings that may be used anywhere, and}
         @item{bindings that may only be used in "define" contexts.}]

The second category is a subset of the first and is needed in order to handle Racket's multiple return @racket[values]. Since Racket functions cannot receive @racket[values], we must know what we are binding at the time of the binding. For example, this means that @racket[~let] may support @racket[values] binding, but @racket[~define] or @racket[~lambda] may not. Thus, the second category essentially the first, but without @racket[values]-bindings.

A few generic binding instances are currently supported. Defining new generic bindings is currently limited to new @racket[match]-specific binding instances. See @secref{new-instances} for more information.

@defform[($ match-pattern)]{
  The @racket[match] binding instance. The required pattern is a @racket[match] pattern. May be used in any context.
      
  Other generic binding instances can be nested within the match pattern.}

@defform/subs[(~vs b ...) ([b define-allowable-generic-binding])]{
  Binding instance for @racket[values]. May only be used in "let" contexts (which means not in @racket[~define] or @racket[~lambda] for example).
      
  Currently supports one-level of nested generic binding instances. In other words, each binding site in the @racket[~vs] form may be either an identifier, or another generic binding instance. However, any nested binding positions are considered "define" contexts. This means that one cannot nest @racket[~vs] bindings.}

@defform[(⋈ b ...)]{ Same as @racket[~vs]. }

@defform[($: x xs)]{
  A @racket[match] binding instance where the outer @racket[match] pattern is @racket[cons]. Equivalent to @racket[($ (cons x xs))]. 
}

@defform*[(($list x ...)
           ($list x ... : rst))]{
  A @racket[match] binding instance where the outer @racket[match] pattern is either @racket[list] or @racket[list-rest], depending whether a rest argument is supplied. Equivalent to either @racket[($ (list x ...))] or @racket[($ (list-rest x ... rst))].
}

@defthing[$null _]{ A @racket[match] binding instance for @racket[null].}

@defform[($stx pattern pattern-directive ...)]{
A binding instance for parsing syntax-objects with @racket[syntax-parse].
}

@defform/subs[($and b ...) ([b generic-binding])]{
A binding instance that binds each @racket[b] binding.  
}

@defform*/subs[(($c db contract-expr)
                ($c b (values contract-expr ...)))
               ([db define-allowable-generic-binding]
                [b generic-binding])]{
A binding instance that binds @racket[b] or @racket[db] with a contract.  
}


@; Core Generic Binding Forms ------------------------------------------------------
@section[#:tag "binding-forms"]{Core Generic Binding Forms}

@defform*/subs[((~define b body)
                (~define (f db ...) body ...))
               ([b generic-binding identifier?]
                [db define-allowable-generic-binding identifier?])]{
  Same as @racket[define] but each binding site (except for the function name) may be either an identifier or a generic binding instance. 

When using @racket[~define] to define a function, any generic binding must be define-allowable. (So the @racket[~vs] values-binding form is not allowed.)
          
@racket[define]-like, non-function examples:
@interaction[#:eval the-eval
(~define x1 (+ 1 2))
x1 ; 3

(~define x2 (list 1 2))
x2 ; (list 1 2)
]

Non-function examples with generic bind forms:
@interaction[#:eval the-eval
(~define ($ (list y1 y2)) (list 10 20))
y1 ; 10 
y2 ; 20
(~define ($ (list (cons y3 y4) y5)) (list (cons 20 30) 40))
y3 ; 20 
y4 ; 30 
y5 ; 40
(~define (⋈ v1 v2) (values 101 202))
v1 ; 101
v2 ; 202
(~define (⋈ ($ (list x4 x5)) x6) (values (list 99 999) 9999))
x4 ;99
x5 ;999
x6 ;9999

(struct A (x y))
(~define ($ (A x7 y7)) (A 101 202))
x7 ;101
y7 ;202

(~define ($stx ((~seq kw:keyword arg:expr) ...)) #'(#:a 1 #:b 2 #:c 3))
#'(kw ...)
#'(arg ...)
#'([kw . arg] ...)
]

@racket[define]-like function examples:
@interaction[#:eval the-eval
(~define (f1 x [y 10] #:z [z 0]) (+ x y z))
(f1 100) ; 110
(f1 100 200) ; 300
(f1 100 200 #:z 300) ; 600

(~define (f3 . rst) rst)
(f3 1 2 3); (list 1 2 3))
(f3); null)
(~define (f4 x y . rst) (cons x (cons y rst)))
(f4 1 2 3 4 5 6); (list 1 2 3 4 5 6))
(f4 1 2); (list 1 2))
]

Function examples using generic bind forms:
@interaction[#:eval the-eval
(~define (f2 ($ (list x y))) (- x y))
(f2 (list 145 45)); 100)
(~define (g1 ($ (list (list a b) y ...))) (apply + a b y))
(g1 (list (list 101 202) 303)); 606)

(~define (f5 ($ (list (list x y) z)) . rst) (cons x (cons y (cons z rst))))
(f5 (list (list 1 2) 3)); (list 1 2 3))
(f5 (list (list 1 2) 3) 4 5); (list 1 2 3 4 5))

;; test non-list pats
(~define (gg ($ xxx)) (add1 xxx))
(gg 10001); 10002)
(~define (ggg ($ _)) 12345)
(ggg 5432); 12345)
(~define (gggg ($ 11111)) 22222)
(gggg 111) ; match fail
(gggg 11111); 22222)

(~define (f20 ($list x y z : xs)) (+ x y z (length xs)))
(f20 (list 10 20 30 40 50 60 70 80 90)); 66)
(~define (f21 ($: x xs)) (+ x (length xs)))
(f21 (list 10 20 30 40 50 60 70 80 90)); 18)
]}

Argument-with-default and keyword binding positions support generic bindings too:
@interaction[#:eval the-eval
;; generic-bind in keyword or arg-with-default positions
(~define (fkw1 [($list x y) (list 1 2)]) (+ x y 10))
(fkw1); 13)
(fkw1 (list 10 20)); 40)
(fkw1 10) ; exn:misc:match

(~define (fkw2 #:A ($list x y)) (+ x y 10))
(fkw2 #:A (list 1 2)); 13)

(~define (fkw3 #:B [($list x y) (list 1 2)]) (+ x y 10))
(fkw3 #:B (list 10 20)); 40)
(fkw3 #:B 10) ;exn:misc:match
]
                                                                   
@defform[(~def ...)]{ Same as @racket[~define].}
@defform[(~d ...)]{ Same as @racket[~define].}

@defform*/subs[((~define/contract b contract-expr body)
                (~define/contract (f db ...) contract-expr body ...))
               ([b generic-binding identifier?]
                [db define-allowable-generic-binding identifier?])]{
like @racket[define/contract], except allowing generic binding instances like @racket[~define].
}
                                         
@defform*/subs[((~lambda db body ...)
                (~lambda (db ...) body ...))
               ([db define-allowable-generic-binding identifier?])]{
  Same as @racket[lambda] but each binding site may be either an identifier or a (define-allowable) generic binding instance. (So the @racket[~vs] values-binding form is not allowed.)
          
  If a single identifier is given with no parentheses, then the arguments are put in a list, just like @racket[lambda]. A single generic bind instance may also be used without parens. If a list of bindings if given, then each may be either an identifier or a generic bind instance.

@defform[(~lam ...)]{ Same as @racket[~lambda].}
@defform[(~l ...)]{ Same as @racket[~lambda].}
@defform[(~λ ...)]{ Same as @racket[~lambda].}

Examples of standard, @racket[lambda]-like behavior:
@interaction[#:eval the-eval
((~λ (x) x) 111); 111)
((~λ rst rst) 1 2 3); (list 1 2 3))
((~λ (x [y 0] #:z z #:a [a 10]) (+ x y z a)) 1 #:z 10); 21)
((~λ (x [y 0] #:z z #:a [a 10]) (+ x y z a)) 1 22 #:z 10); 43)
((~λ (x [y 0] #:z z #:a [a 10]) (+ x y z a)) 1 #:z 10 #:a 111); 122)
((~lambda (x [y 0] #:z z #:a [a 10]) (+ x y z a)) 1 #:z 10 #:a 111); 122)
]

Examples with generic bind forms:
@interaction[#:eval the-eval
((~lambda ($: x xs) (append xs (list x))) (list 1 2 3 4 5)); (list 2 3 4 5 1))
((~lambda (f ($: x xs)) (cons (f x) xs)) add1 (list 1 2 3 4)); (list 2 2 3 4))
((~lambda (f ($list)) (f 1)) add1 null); 2)
((~lambda (f ($list)) (f 1)) add1 (list 1 2 3)) ; err
((~λ (($ (list x y)) ($ (cons a b))) (+ a b x y)) 
               (list 1 2) (cons 3 4));
;              10)
((~λ (x y ($ (A a b))) (+ x y a b)) 10 20 (A 30 40)); 100)
]

Example of single-arg generic bind support:
@interaction[#:eval the-eval
;; single arg
((~λ ($ (list x y)) (+ x y)) (list 14 56)); 70)
]

The next example may look like a single-arg generic bind, but since @racket[~vs] is not allowed in this context, it is treated as an identifier and shadows the existing binding for @racket[~vs].
@interaction[#:eval the-eval
((~λ (~vs v1 v2) (+ ~vs v1 v2)) 1 2 3); 6) ;; shadow ~vs
]
Here is a disallowed generic bind used in the standard way, resulting in an error:
@interaction[#:eval the-eval
(~λ ((~vs v1 v2)) (+ v1 v2)) ; error
]

Generic bind forms are allowed in keyword and arg-with-default binding positions too:
@interaction[#:eval the-eval
((~λ ([($: x xs) (list 1 2)]) (+ x (length xs) 20))); 22)
((~λ ([($: x xs) (list 1 2)]) (+ x (length xs) 20)) (list 1 2 3 4)); 24)
((~λ (#:C ($: x xs)) (append xs (list x))) #:C (list 1 2 3)); (list 2 3 1))
((~λ (#:D [($: x xs) (list 10 20 30)]) (append xs (list x)))); (list 20 30 10))
((~λ (#:D [($: x xs) (list 10 20 30)]) (append xs (list x))) #:D (list 1 2 3)); (list 2 3 1))
]}

@defform/subs[(~case-lambda clause ...)
              ([clause (header body ...)])]{
  A "simulation" of a @racket[case-lambda] that also acccepts generic binds. 
                      
  Since @racket[case-lambda] only distinguishes between cases based on the number of arguments, @racket[~case-lambda] cannot be defined in terms of @racket[case-lambda]. Instead @racket[~case-lambda] defines a series of @racket[lambda]s, and then tries to apply each one, backtracking on @racket[exn:misc:match?].
@examples[#:eval the-eval
(define casemap1 
  (~case-lambda [(f $null) null]
                [(f ($: x xs)) (cons (f x) (casemap1 f xs))]))
(casemap1 add1 null); null)
(casemap1 add1 (list 1)); (list 2))
(casemap1 add1 (list 1 2 3 4)); (list 2 3 4 5))
(casemap1 add1 1)
]}

@defform[(~case-lam clause ...)]{ Same as @racket[~case-lambda]
}

@defform*/subs[((~case-define f clause1 ...)
                (~case-define f clause2 ...))
               ([clause1 (header body ...)]
                [clause2 (b ... → body ...)])]{
  Syntax for naming a @racket[~case-lambda]
@examples[#:eval the-eval
(~case-define new-map1 
  [(f $null) null]
  [(f ($: x xs)) (cons (f x) (new-map1 f xs))])
(new-map1 add1 null); null)
(new-map1 add1 (list 1)); (list 2))
(new-map1 add1 (list 1 2)); (list 2 3))

(~case-define new-filter1 
  [(p? $null) null]
  [(p? ($: x xs)) (if (p? x) (cons x (new-filter1 p? xs)) (new-filter1 p? xs))])
(new-filter1 even? null); null)
(new-filter1 even? (list 1)); null)
(new-filter1 even? (list 1 2 3 4 5)); (list 2 4))

(~case-define new-foldl1
  [(f base $null) base]
  [(f base ($: x xs)) (new-foldl1 f (f x base) xs)])
(new-foldl1 - 0 null); 0)
;(new-foldl1 - 0 (list 1)); 1)
(new-foldl1 - 0 (list 1))
(foldl - 0 (list 1))
;(new-foldl1 - 0 (list 1 2)); 1)
(new-foldl1 - 0 (list 1 2))
(foldl - 0 (list 1 2))


(~case-define new-foldr1
  [(f base $null) base]
  [(f base ($: x xs)) (f x (new-foldr1 f base xs))])
(new-foldr1 - 0 null); 0)
;(new-foldr1 - 0 (list 1)); 1)
(new-foldr1 - 0 (list 1))
(foldr - 0 (list 1))
;(new-foldr1 - 0 (list 1 2)); -1)
(new-foldr1 - 0 (list 1 2))
(foldr - 0 (list 1 2))

]}
                                             
@defform[(~case-def clause ...)]{ Same as @racket[~case-define]
@examples[#:eval the-eval
(~case-def new-map [f $null → null]
                   [f ($: x xs) → (cons (f x) (new-map f xs))])
(new-map add1 null); null)
(new-map add1 (list 1)); (list 2))
(new-map add1 (list 1 2)); (list 2 3))

(~case-def new-filter [p? $null → null]
                      [p? ($: (? p? x) xs) → (cons x (new-filter p? xs))]
                      [p? ($: x xs) → (new-filter p? xs)])
(new-filter even? null); null)
(new-filter even? (list 1)); null)
(new-filter even? (list 1 2 3 4 5)); (list 2 4))

(~case-def new-foldl [f base $null → base]
                     [f base ($: x xs) → (new-foldl f (f x base) xs)])
(new-foldl - 0 null); 0)
;(new-foldl - 0 (list 1)); 1)
(new-foldl - 0 (list 1))
(foldl - 0 (list 1))
;(new-foldl - 0 (list 1 2)); 1)
(new-foldl - 0 (list 1 2))
(foldl - 0 (list 1 2))

(~case-def new-foldr [f base $null → base]
                     [f base ($: x xs) → (f x (new-foldr f base xs))])
(new-foldr - 0 null); 0)
;(new-foldr - 0 (list 1)); 1)
(new-foldr - 0 (list 1))
(foldr - 0 (list 1))
;(new-foldr - 0 (list 1 2)); -1)
(new-foldr - 0 (list 1 2))
(foldr - 0 (list 1 2))
]}
                                             
@defform*/subs[((~let loop ([db e] ...) body ...)
                (~let ([b e] ...) body ...))
               ([db define-allowable-generic-bind identifier?]
                [b generic-bind identifier?])]{
  Same as @racket[let], but with generic bind support. If a name is given to the let, then the bindings must be define-allowable, since it's essentially defining a function. If no name is given to the @racket[~let], then any generic bind is allowed.

Note that when using the match @racket[$] binding with @racket[~let], the behavior is NOT like @racket[match-let], in that there is no coupling between between the binding positions. This means that trying to bind duplicate identifiers in a @racket[$] match pattern will produce the same results or errors as in @racket[let], @racket[let*], or @racket[letrec].

@examples[#:eval the-eval
(define x55 155)
(~let ([x55 (add1 x55)]) x55); 156)
(define x56 156)
(~let ([x56 157] [x57 x56]) x57); 156)

(~let ([($ (list x58 x59)) (list 158 159)]) (list x59 x58));
;              (list 159 158))

;; values
(~let ([(~vs v17 v27) (values 17 27)]) (+ v17 v27)); 44)
(~let ([(~vs ($ (list x18 y18)) v18) (values (list 18 19) 20)])
                    (+ x18 y18 v18));
;              57)

;; check dup ids
(~let ([x 1] [x 2]) x) ; fail
(~let ([($ (list x y)) (list 1 2)] [x 3]) x) ; fail
(~let ([x 3] [($ (list x y)) (list 1 2)]) x) ; fail
(~let ([($ (list x y)) (list 1 2)] [($ (list x y)) (list 3 4)]) (list x y)) ; fail
(~let ([($ (list x y)) (list 1 2)] [(~vs x y) (list 3 4)]) (list x y)) ; fail
;; dups in single match pat is ok
(~let ([($ (list x x)) (list 2 2)]) x); 2)

;; check ~let bindings cant see previous bindings
(let ([x 12345]) (~let ([x 40] [y x]) y)); 12345)
(let ([x 12345]) (~let ([x 40] [y 1] [z x]) z)); 12345)
(let ([x 12345] [y 5432]) (~let ([x 40] [y 23] [z y]) z)); 5432)

;; check rec defs fail
(~let ([x59 x59]) x59) ; fail
 (~let ([f (λ (x) (if (zero? x) 1 (* x (f (sub1 x)))))]) (f 10)) ; fail
;(check-equal? 20
              (~let L ([($ (list x y ...)) (list 1 2 3 4 5)] [n 4])
                    (if (zero? n) 0
                        (+ n x (L y (sub1 n)))))

;; check named let corner cases
(let loop ([x 1] [x 2]) x) ; duplicate id error
(~let loop ([x 1] [x 2]) x) ; duplicate id error (but references define)
(define x8 109)
(let loop ([x8 1] [y x8]) y) ; 109
(~let loop ([x8 1] [y x8]) y) ; 109
]}
                                  
@defform/subs[(~let* ([b e] ...) body ...)
              ([b generic-bind identifier?])]{ 
Same as @racket[let*], but with generic bind support.
@examples[#:eval the-eval
(~let* ([x 46436] [y x]) y); 46436)
(~let* ([($ (list x y)) (list 1010 2020)] [z (+ x y)]) z); 3030)
(~let* ([(⋈ v1 v2) (values 4040 5050)] [z (+ v1 v2)]) z); 9090)
(~let* ([(⋈ v3 ($ (list xy yx))) (values 202 (list 303 404))] 
                      [z (+ v3 (- yx xy))])
                     z);
;              303)
(~let* ([x 1122] 
                      [(⋈ ($ (cons ab bc)) ($ (list-rest y ys)))
                       (values (cons x (* x 2)) (list x (add1 x) (sub1 x)))])
                     (+ (- bc ab) (- y (+ (car ys) (cadr ys))))); 
;              0)
(~let* ([x 101] [x 202]) x); 202)
]}
@defform/subs[(~letrec ([b e] ...) body ...)
              ([b generic-bind identifier?])]{
Same as @racket[letrec], but with generic bind support.
@examples[#:eval the-eval
(require math/number-theory) ; factorial
(~letrec ([x x]) x); (letrec ([x x]) x))
(~letrec ([f (λ (x) (if (zero? x) 1 (* x (f (sub1 x)))))]) (f 10)); 
              (factorial 10)
(~letrec ([evn? (λ (x) (if (zero? x) #t (od? (sub1 x))))]
           [od? (λ (x) (if (zero? x) #f (evn? (sub1 x))))])
   (evn? 10))
(~letrec ([(⋈ evn? od?)
            (values (λ (x) (if (zero? x) #t (od? (sub1 x))))
                    (λ (x) (if (zero? x) #f (evn? (sub1 x)))))])
   (evn? 10));)
(~letrec ([($ (list evn? od?))
            (list (λ (x) (if (zero? x) #t (od? (sub1 x))))
                  (λ (x) (if (zero? x) #f (evn? (sub1 x)))))])
          (and (od? 101) (evn? 10)));)
(~letrec ([(⋈ evn? ($ (list od?)))
            (values (λ (x) (if (zero? x) #t (od? (sub1 x))))
                    (list (λ (x) (if (zero? x) #f (evn? (sub1 x))))))])
          (and (od? 101) (evn? 10)));)
]}                                             



@; Comprehension --------------------------------------------------------------

@; Generic Comprehension Forms ------------------------------------------------------
@section{Generic Comprehension Forms}

All the forms in this section are the same as their Racket counterparts (see @racket[for]), but with added generic bind support.

@defform[(~for ...)]{}
@defform[(~for/list ...)]{}
@defform[(~for/lists ...)]{}
@defform[(~for/vector ...)]{}
@defform[(~for/fold ...)]{Allows generic binds in sequence clauses, not accumulator clauses.}
@defform[(~for/first ...)]{}
@defform[(~for/last ...)]{}
@defform[(~for/or ...)]{}
@defform[(~for/and ...)]{}
@defform[(~for/sum ...)]{}
@defform[(~for/product ...)]{}
@defform[(~for/hash ...)]{}
@defform[(~for/hasheq ...)]{}
@defform[(~for/hasheqv ...)]{}

@defform[(~for* ...)]{}
@defform[(~for*/list ...)]{}
@defform[(~for*/lists ...)]{}
@defform[(~for*/vector ...)]{}
@defform[(~for*/fold ...)]{Allows generic binds in sequence clauses, not accumulator clauses.}
@defform[(~for*/first ...)]{}
@defform[(~for*/last ...)]{}
@defform[(~for*/or ...)]{}
@defform[(~for*/and ...)]{}
@defform[(~for*/sum ...)]{}
@defform[(~for*/product ...)]{}
@defform[(~for*/hash ...)]{}
@defform[(~for*/hasheq ...)]{}
@defform[(~for*/hasheqv ...)]{}

@examples[#:eval the-eval
(~for/list ([($ (list x y)) '((1 2) (3 4) (5 6))]) (list y x));
;              '((2 1) (4 3) (6 5)))

 (~for/list 
  ([($ (list x y)) '((1 2) (3 4))]
   [($ (list a b)) '((5 6) (7 8))])
  (list y x b a));
; '((2 1 6 5) (4 3 8 7)))

 (~for/list 
  ([($ (list x y)) '((1 2) (3 4))]
   [($ (list a b)) '((5 6) (7 8))]
   #:when (= x 1)) 
  (list y x b a));
; '((2 1 6 5)))

 (~for/list 
  ([($ (list x y)) '((1 2) (3 4))]
   [($ (list a b)) '((5 6) (7 8))]
   #:unless (= x 1)) 
  (list y x b a));
; '((4 3 8 7)))

 (~for/list 
  ([($ (list x y)) '((2 2) (1 4) (1 4))]
   [($ (list a b)) '((5 6) (6 7) (7 8))]
   #:when (= x 1)
   #:unless (= a 6))
  (list y x b a));
; '((4 1 8 7)))
(~for/list ([x '(1 2 3)] #:break (= x 2)) x); '(1))
(~for/list ([x '(1 2 3)] #:final (= x 2)) x); '(1 2))
(~for*/list ([x '(1 2 3)]) #:break (= x 2) x); '(1))
(~for*/list ([x '(1 2 3)]) #:final (= x 2) x); '(1 2))

 (~for/list ([x '(1 2 3)] #:when #t [y '(4 5 6)] #:when #t) (list x y));
; '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)))
 (~for*/list ([x '(1 2 3)] [y '(4 5 6)]) (list x y));
; '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)))

(require racket/generator)
 (~for/list ([($ (list x y)) (in-generator (let loop ([n 3])
                                             (unless (zero? n)
                                               (yield (list n (add1 n)))
                                               (loop (sub1 n)))))])
            (list x y));
; '((3 4) (2 3) (1 2)))
;; sequence-generate is broken?
;(check-equal?
; (~for/list ([(~vs x y) (in-generator (let loop ([n 3])
;                                        (unless (zero? n)
;                                          (yield n (add1 n))
;                                          (loop (sub1 n)))))])
;            (list x y))
; '((3 4) (2 3) (1 2)))

;; for/fold
 ;(~for/fold ([sum 0]) ([x '(1 2 3 4 5 6)]) (+ x sum));
; 21)

 (~for/fold ([sum 0]) ([x '(1 2 3 4 5 6)]) (+ x sum));
 (for/fold ([sum 0]) ([x '(1 2 3 4 5 6)]) (+ x sum))

(~for/and ([x (list #t #f #t)]) (displayln x) x)
;; should print
;#t
;#f
;#f

 #;(~for*/list ([($ (list x y)) (list (list (list 1 2 3) (list 4 5 6))
                                    (list (list 10 11 12) (list 13 14 15)))]
               [a x] [b y])
             (list a b));
; '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6) (10 13)
;   (10 14) (10 15) (11 13) (11 14) (11 15) (12 13) (12 14) (12 15)))

  (~for*/list ([($ (list x y)) (list (list (list 1 2 3) (list 4 5 6))
                                    (list (list 10 11 12) (list 13 14 15)))]
               [a x] [b y])
             (list a b));
  (for*/list ([lst (list (list (list 1 2 3) (list 4 5 6))
                         (list (list 10 11 12) (list 13 14 15)))]
              [a (car lst)] [b (cadr lst)])
    (list a b))
 (~for*/list ([(~vs v1 v2) (in-hash (hash 1 2 3 4))]
              [x v1] [y v2])
             (cons x y));
; '((0 . 0) (0 . 1) (0 . 0) (0 . 1) (0 . 2) (0 . 3)
;   (1 . 0) (1 . 1) (1 . 2) (1 . 3) (2 . 0) (2 . 1) (2 . 2) (2 . 3)))
  #;(~for*/list ([(~vs v1 v2) (in-hash (hash 1 2 3 4))]
              [x v1] [y v2])
             (cons x y));
   (for*/list ([(v1 v2) (in-hash (hash 1 2 3 4))]
              [x v1] [y v2])
             (cons x y))
  (~for*/list ([(v1 v2) (in-hash (hash 1 2 3 4))]
              [x v1] [y v2])
             (cons x y));
  #;(~for*/list ([(~vs v1 v2) (in-hash (hash 1 2 3 4))]
               [x v1] [y v2])
              (cons x y))

 (~for*/list ([(~vs ($ (list x y)) ($ (list a b))) 
               (in-hash (hash (list 1 2) (list 3 4) (list 5 6) (list 7 8)))]
              [c x])
             (list x y a b c));
 (for*/list ([(lst1 lst2) (in-hash (hash (list 1 2) (list 3 4) (list 5 6) (list 7 8)))]
             [c (car lst1)])
   (list (car lst1) (cadr lst1) (car lst2) (cadr lst2) c))]


@; Implementing New Generic Binding Instances ---------------------------------
@section[#:tag "new-instances"]{Implementing New Generic Binding Instances}

Only defining @racket[match]-specific new binding instances are currently possible.

@defform*[((define-match-bind (name x ...))
           (define-match-bind name))]{
Defines a new binding instance @racket[$name] that binds using the match pattern @racket[(name x ...)].

@interaction[#:eval the-eval
(struct B (x y z))
(define-match-bind (B x y z))

(~define (bf ($B x y z)) (+ x y z))
(bf (B 20 40 60)) ; 120

(define-match-bind hash-table)
(~define ($hash-table [keys vals] ...) (hash 'a 1 'b 2 'c 3))
keys
vals
]}

@defform*[
 [(~struct struct-id (field ...) struct-option ...)
  (~struct struct-id super-struct-id (field ...) struct-option ...)]
 #:grammar ([field field-id
                   [field-id field-option ...]]
            [struct-option #:mutable
                           (code:line #:super super-expr)
                           (code:line #:inspector inspector-expr)
                           (code:line #:auto-value auto-expr)
                           (code:line #:guard guard-expr)
                           (code:line #:property prop-expr val-expr)
                           (code:line #:transparent)
                           (code:line #:prefab)
                           (code:line #:sealed)
                           (code:line #:authentic)
                           (code:line #:name name-id)
                           (code:line #:extra-name name-id)
                           (code:line #:constructor-name constructor-id)
                           (code:line #:extra-constructor-name constructor-id)
                           (code:line #:reflection-name symbol-expr)
                           (code:line #:methods gen:name-id method-defs)
                           #:omit-define-syntaxes
                           #:omit-define-values]
            [field-option #:mutable
                          #:auto]
            [method-defs (definition ...)])]{
Exactly like @racket[struct] except a new generic binding
instance is also defined.
Equivalent to using @racket[struct] and @racket[define-match-bind].


@interaction[#:eval the-eval
(~struct C (a b c [d #:mutable]))
]
Behaves like a @racket[struct]-defined struct:
@interaction[#:eval the-eval
(define c (C 9 8 7 6))
(C? c)
(C-a c); 9)
(C-b c); 8)
(C-c c); 7)
(C-d c); 6)
(set-C-d! c 20)
(C-d c); 20)
]
Generic binding instance:
@interaction[#:eval the-eval
(~define (cf ($C e f g h)) (+ e f g h))
(cf c); 44)
]}

@do-if-struct/contract-available[
@defform*[
 [(~struct/contract struct-id
                    ([field contract-expr] ...)
                    struct-option ...)
  (~struct/contract struct-id super-struct-id
                    ([field contract-expr] ...)
                    struct-option ...)]
 #:grammar ([field field-id
                   [field-id field-option ...]]
            [struct-option #:mutable
                           (code:line #:auto-value auto-expr)
                           (code:line #:property prop-expr val-expr)
                           (code:line #:transparent)
                           #:omit-define-syntaxes]
            [field-option #:mutable
                          #:auto])]{
Exactly like @racket[struct/contract] except a new generic
binding instance is also defined.
Equivalent to using @racket[struct/contract] and
@racket[define-match-bind].

@interaction[#:eval the-eval
(~struct/contract D ([a number?] [b string?] [[c #:mutable] list?]))
]
Behaves like a @racket[struct/contract]-defined struct:
@interaction[#:eval the-eval
(define d (D 200 "abcdefghij" '(k l m n o p)))
(D? d)
(D-a d)
(D-b d)
(D-c d)
(set-D-c! d '(q r s))
(D-c d)
]
Generic binding instance:
@interaction[#:eval the-eval
(~define (df ($D n s l)) (+ n (string-length s) (length l)))
(df d)
]}]

@defform*[
 [(~define-struct/contract struct-id
                           ([field contract-expr] ...)
                           struct-option ...)
  (~define-struct/contract (struct-id super-struct-id)
                           ([field contract-expr] ...)
                           struct-option ...)]
 #:grammar ([field field-id
                   [field-id field-option ...]]
            [struct-option #:mutable
                           (code:line #:auto-value auto-expr)
                           (code:line #:property prop-expr val-expr)
                           (code:line #:transparent)
                           #:omit-define-syntaxes]
            [field-option #:mutable
                          #:auto])]{
Exactly like @racket[define-struct/contract] except a new
generic binding instance is also defined.
Equivalent to using @racket[define-struct/contract] and
@racket[define-match-bind].

@interaction[#:eval the-eval
(~define-struct/contract E ([a number?] [b string?] [[c #:mutable] list?]))
]
Behaves like a @racket[define-struct/contract]-defined struct:
@interaction[#:eval the-eval
(define e (make-E 200 "abcdefghij" '(k l m n o p)))
(E? e)
(E-a e)
(E-b e)
(E-c e)
(set-E-c! e '(q r s))
(E-c e)
]
Generic binding instance:
@interaction[#:eval the-eval
(~define (ef ($E n s l)) (+ n (string-length s) (length l)))
(ef e)
]}
