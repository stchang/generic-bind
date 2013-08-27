#lang racket
(require (for-syntax syntax/parse
                     racket/syntax
                     racket/list)) ; append-map
(require (for-syntax syntax/parse/experimental/template))
(require (for-syntax "stx-utils.rkt"))

;; TODO:
;; [x] 2013-08-26: ~let doesn't support ~vs
;;                 DONE: 2013-08-26
;; [o] 2013-08-24: bug: creating "bound-ids" stx prop breaks ~define
;;                 (as in it doesn't recognize :bind classes anymore)
;; [o] 2013-08-22: get rid of suprious defines for generic binds nested in
;;                 generic ~v binding -- use splicing
;; [o] 2013-08-21: syntax object for ~m and ~vs is void?
;;                 can I put anything useful here?
;; [o] 2013-08-21: create generic form for syntax to 
;;                 automatically define accessors?
;; [o] 2013-08-21: fix error msgs
;;                 - named ~let dup id references define
(provide ~m ~vs $: $list $null
         ~define ~lambda (rename-out [~lambda ~λ]) ~case-lambda ~case-define
         ~let ~let* ~letrec
         ~for ~for/list ~for/fold ~for/lists ~for/last ~for/and ~for/or 
         ~for/first ~for/sum ~for/product ~for/vector
         ~for* ~for*/list ~for*/vector)

(require racket/splicing)

;; (define-generic-stx bind (definer letter ids let-only 
;;                           nested-definers nested-idss))

;; ----------------------------------------------------------------------------
;; syntax classes

(begin-for-syntax
  ;; need this to catch errors when expanding header of function defines
  (define (try-local-expand stx) 
    (with-handlers ([exn:fail:syntax:unbound? (λ _ #f)] ;; unbound var
                    [exn:fail:syntax? (λ _ #f)] ;; ie keyword as datum
                    [exn:fail? (λ _ (printf "uh oh\n"))])
      (local-expand stx 'expression null)))

  ;; accessors
  (define (bind-definer stx) (syntax-property stx 'definer))
  (define (bind-letter stx) (syntax-property stx 'letter))
  (define (bind-ids stx) (syntax-property stx 'ids))
  (define (bind-nested-definers stx) (syntax-property stx 'nested-definers))
  (define (bind-nested-idss stx) (syntax-property stx 'nested-idss))

  (define (bind? stx) (and (bind-definer stx) (bind-ids stx)))
  (define (bind-let-only? stx) (syntax-property stx 'let-only))
  
  (define-syntax-class bind
    #:description "a generic bind instance"
    (pattern b 
      #:with expanded-b (try-local-expand #'b)
      #:when (bind? #'expanded-b)
      #:attr definer (bind-definer #'expanded-b)
      #:attr letter (bind-letter #'expanded-b)
      #:attr ids (datum->syntax #'b (bind-ids #'expanded-b))
;      #:attr nested-definers (bind-nested-definers #'expanded-b)
;      #:attr nested-idss (datum->syntax #'b (bind-nested-idss #'expanded-b))))
      #:attr nested-defs
      (let ([ids-lst (syntax->list #'ids)])
        (if ids-lst ; for non-list patterns
            (datum->syntax #'b
              (for/list 
                ([nested-definer 
                  (syntax->list (bind-nested-definers #'expanded-b))]
                 [nested-ids (bind-nested-idss #'expanded-b)]
                 [id (syntax->list #'ids)])
                (list nested-definer (datum->syntax #'b nested-ids) id)))
            #'()))))
  ;; - need this because some instances (ie anything involving values)
  ;; needs the target of the bind to work
  ;; - for example, values can't be used in function defs
  ;; - so this is a subset of :bind, which is used in things like fn defs
  (define-syntax-class bind/non-let
    #:description "a generic bind instance that supports non-let contexts"
    #:auto-nested-attributes
    (pattern :bind 
      #:fail-when (bind-let-only? #'expanded-b)
                  (format "can't use ~a pattern in non-let binding context"
                          (syntax->datum #'b))))
  (define-syntax-class id-or-bind/non-let
    #:auto-nested-attributes
    (pattern :bind/non-let #:attr name (generate-temporary))
    (pattern x:id #:attr name (generate-temporary)
                  #:attr definer #'define
                  #:attr letter #'let
                  #:attr ids #'x))
  ) ;; end begin-for-syntax


;; ----------------------------------------------------------------------------
;; generic bind "instances"

;; match generic bind instance 
;; TODO: currently does not support nested generic binds
(define-syntax (~m stx)
  (syntax-parse stx
    [(_ pat) (add-syntax-properties
              `([definer ,#'match-define]
                [letter ,#'match-let]
                ;; need to capture ids in unexpanded ctx so just store datums
                [ids ,(syntax->datum #'pat)]
                [let-only #f]
                [nested-definers ,#'()]
                [nested-idss ,null])
              #'(void))]))

;; match bindings where the outer form is a list or cons
(define-syntax-rule (match-listrest-define (x ... rst) e)
  (match-define (list-rest x ... rst) e))
(define-syntax-rule (match-listrest-let ([(x ... rst) e] ...) body ...) 
  (match-let ([(list-rest x ... rst) e] ...) body ...))
(define-syntax-rule (match-list-define (x ...) e)
  (match-define (list x ...) e))
(define-syntax-rule (match-list-let ([(x ...) e] ...) body ...) 
  (match-let ([(list x ...) e] ...) body ...))
(define-syntax ($list stx)
  (syntax-parse stx #:datum-literals (:)
    [(_ x ... : rst) (add-syntax-properties
                      `([definer ,#'match-listrest-define]
                        [letter ,#'match-listrest-let]
                        [ids ,(syntax->datum #'(x ... rst))]
                        [let-only #f]
                        [nested-definers ,#'()]
                        [nested-idss ,null])
                      #'(void))]
    [(_ x ...) (add-syntax-properties
                `([definer ,#'match-list-define]
                  [letter ,#'match-list-let]
                  [ids ,(syntax->datum #'(x ...))]
                  [let-only #f]
                  [nested-definers ,#'()]
                  [nested-idss ,null])
                #'(void))]))
(define-syntax-rule (match-cons-define (x xs) e) (match-define (cons x xs) e))
(define-syntax-rule (match-cons-let ([(x xs) e] ...) body ...) 
  (match-let ([(cons x xs) e] ...) body ...))
(define-syntax ($: stx)
  (syntax-parse stx 
    [(_ x xs) (add-syntax-properties
               `([definer ,#'match-cons-define]
                 [letter ,#'match-cons-let]
                 [ids ,(syntax->datum #'(x xs))]
                 [let-only #f]
                 [nested-definers ,#'()]
                 [nested-idss ,null])
               #'(void))]))
(define-syntax-rule (match-null-define (x ...) e) ; ignore args
  (match-define '() e))
(define-syntax-rule (match-null-let ([(x ...) e] ...) body ...) 
  (match-let (['() e] ...) body ...))
(define-syntax ($null stx)
  (syntax-case stx ()
    [_ (add-syntax-properties
        `([definer ,#'match-null-define]
          [letter ,#'match-null-let]
          [ids ,null]
          [let-only #f]
          [nested-definers ,#'()]
          [nested-idss ,null])
        #'(void))]))
  

;; values generic bind instance
;; - supports (one-level only) nested (non-let-restricted) generic binds
;;   (currently this is only the generic bind instance for match)
;;   (to support artitrary nesting, need to get the nested-defs of each x:v-bind)
(define-syntax (~vs stx)
  (syntax-parse stx
    [(_ x:id-or-bind/non-let ...)
    (add-syntax-properties
      `([definer ,#'define-values] 
        [letter ,#'let-values]
        ;; need to capture ids in unexpanded ctx so just store datums
        [ids ,(syntax->datum #'(x.name ...))]
        [let-only #t]
        [nested-definers ,#'(x.definer ...)]
        ;; need to capture ids in unexpanded ctx so just store datums
        [nested-idss ,(syntax->datum #'(x.ids ...))])
      #'(void))]))

;; ----------------------------------------------------------------------------
;; ~define

;; syntax-classes for ~define
(begin-for-syntax
  (define-syntax-class def-function-header
    (pattern ((~or header:def-function-header name:id) . args:def-fn-args)
             #:attr new-header 
                    (template ((?? header.new-header name) . args.new-args))
             #:attr defs #'args.defs))

  ;; new-arg needs to be spliced bc of keywords
  ;; def needs to be spliced bc some args don't require defs
  (define-syntax-class def-fn-args
    (pattern (arg:fn-arg ...)
             #:attr new-args (template ((?@ . arg.new-arg) ...))
             #:attr defs (template ((?@ . arg.def) ...)))
    (pattern (arg:fn-arg ... . rest:id)
             #:attr new-args (template ((?@ . arg.new-arg) ... . rest))
             #:attr defs (template ((?@ . arg.def) ...))))
    
  ;; new-arg has to be list because keywords need to be spliced
  ;; def must be list to accomodate args not requiring extra defs
  (define-splicing-syntax-class fn-arg
    #:auto-nested-attributes
    ;; need this here to avoid conflicting with arg-with-default
    ;; actually, this needs to be first, if I want to allow
    ;; generic binding identifiers
    (pattern :bind/non-let
             #:attr new-arg #`(#,(generate-temporary))
             #:attr def #`((definer ids #,(car (syntax->list #'new-arg)))))
    (pattern name:id 
             #:attr new-arg #'(name)
             #:attr def #'())
    (pattern [name:id default] 
             #:attr new-arg #'([name default])
             #:attr def #'())
    (pattern (~seq kw:keyword name:id) 
             #:attr new-arg #'(kw name)
             #:attr def #'())
    (pattern (~seq kw:keyword [name:id default]) 
             #:attr new-arg #'(kw [name default])
             #:attr def #'()))
    ) ;; end begin-for-syntax

;; ~define --------------------------------------------------------------------
(define-syntax (~define stx)
  (syntax-parse stx
    [(_ b:bind body:expr)
     (quasisyntax/loc stx 
       (begin (b.definer b.ids body)
              #,@#'b.nested-defs))]
    [(_ x:id body:expr) (syntax/loc stx (define x body))]
    [(_ ?header:def-function-header ?body ...)
     (template 
      (define ?header.new-header 
        (?@ . ?header.defs)
        ?body ...))]))
         

;; ----------------------------------------------------------------------------
;; ~lambda

(begin-for-syntax
  (define-syntax-class lam-function-header
    (pattern args:lam-fn-args
             #:attr new-header #'args.new-args
             #:attr defs #'args.defs))

  ;; new-arg needs to be spliced bc of keywords
  ;; def needs to be spliced bc some args don't require defs
  (define-syntax-class lam-fn-args
    (pattern (arg:fn-arg ...)
             #:attr new-args (template ((?@ . arg.new-arg) ...))
             #:attr defs (template ((?@ . arg.def) ...)))
    (pattern (arg0:fn-arg arg:fn-arg ... . rest:id)
             #:attr new-args 
                    (template 
                     ((?@ . arg0.new-arg) (?@ . arg.new-arg) ... . rest))
             #:attr defs (template ((?@ . arg0.def) (?@ . arg.def) ...))))
    (define-syntax-class id-or-bind
    #:auto-nested-attributes
    (pattern :bind #:attr name (generate-temporary))
    (pattern x:id #:attr name (generate-temporary)
                  #:attr definer #'define
                  #:attr letter #'let
                  #:attr ids #'x
                  #:attr nested-defs #'()))

    ;; ~case-lambda syntax-classes
    (define-syntax-class case-lam-function-header
      (pattern args:case-lam-fn-args
               #:attr new-header #'args.new-args
               #:attr defs #'args.defs))
    ;; new-arg needs to be spliced bc of keywords
    ;; def needs to be spliced bc some args don't require defs
    (define-syntax-class case-lam-fn-args
      (pattern (arg:case-lam-fn-arg ...)
               #:attr new-args (template ((?@ . arg.new-arg) ...))
               #:attr defs (template ((?@ . arg.def) ...)))
      (pattern (arg0:case-lam-fn-arg arg:case-lam-fn-arg ... . rest:id)
               #:attr new-args 
               (template 
                ((?@ . arg0.new-arg) (?@ . arg.new-arg) ... . rest))
               #:attr defs (template ((?@ . arg0.def) (?@ . arg.def) ...))))
    ;; new-arg has to be list because keywords need to be spliced
    ;; def must be list to accomodate args not requiring extra defs
    (define-splicing-syntax-class case-lam-fn-arg
      #:auto-nested-attributes
      ;; need this here to avoid conflicting with arg-with-default
      (pattern :bind/non-let
               #:attr new-arg #`(#,(generate-temporary))
               #:attr def #`((definer ids #,(car (syntax->list #'new-arg)))))
      (pattern name:id 
               #:attr new-arg #'(name)
               #:attr def #'())
      (pattern [name:id default] 
               #:attr new-arg #'([name default])
               #:attr def #'()))
  ) ; end define-for-syntax

(define-syntax (~lambda stx)
  (syntax-parse stx
    [(_ b:bind/non-let body:expr ...)
     #:with x (generate-temporary)
     (syntax/loc stx (lambda (x) (b.definer b.ids x) body ...))]
    [(_ rst:id body:expr ...) (syntax/loc stx (lambda rst body ...))]
    [(_ ?header:lam-function-header ?body ...)
     (template 
      (lambda ?header.new-header 
        (?@ . ?header.defs)
        ?body ...))]))

(define-syntax (~case-lambda stx)
  (syntax-parse stx
    [(_ [?header:case-lam-function-header ?body ...] ...)
     #:with (fn ...) (generate-temporaries #'(?header ...))
     #:with args (generate-temporary)
     #:with new-body 
     (let loop ([fns (syntax->list #'(fn ...))])
       (if (null? (cdr fns))
           #`(apply #,(car fns) args)
           #`(with-handlers ([exn:misc:match? (λ _ #,(loop (cdr fns)))])
               (apply #,(car fns) args))))
     #'(let ([fn (~lambda ?header ?body ...)] ...)
         (λ args new-body))]))
      ;(case-lambda [?header.new-header (?@ . ?header.defs) ?body ...] ...))]))
(define-syntax (~case-define stx)
  (syntax-parse stx
    [(_ f clause ...)
     #'(define f (~case-lambda clause ...))]))
  
;; ~let -----------------------------------------------------------------------
(define-syntax (~let stx)
  (syntax-parse stx
    ;; only non-let generic binds are allowed in named let
    ;; (same as other fn def forms)
    [(_ loop:id ([x:id-or-bind/non-let e] ...) body ...)
     #`(let ()
         (~define (loop x ...) body ...)
         (loop e ...))]
    [(_ ([x:id-or-bind e] ...) body ...)
;     #'((~lambda (x ...) body ...) e ...)]))
     (with-syntax ([(new-e ...) (map syntax-local-introduce (syntax->list #'(e ...)))])
       #`(let ()
           (x.definer x.ids new-e) ...
           #,@(append-map syntax->list (syntax->list #'(x.nested-defs ...)))
           body ...))]))

(define-syntax (~let* stx)
  (syntax-parse stx
    [(_ ([x:id-or-bind e]) body ...) 
     #`(x.letter ([x.ids e]) 
         #,@#'x.nested-defs
         body ...)]
    [(_ ([x:id-or-bind e] rst ...) body ...)
     #`(x.letter ([x.ids e])
         #,@#'x.nested-defs
         (~let* (rst ...) body ...))]))

(define-syntax (~letrec stx)
  (syntax-parse stx
    [(_ ([x:id-or-bind e] ...) body ...)
     #`(let ()
         (x.definer x.ids e) ...
         #,@(append-map syntax->list (syntax->list #'(x.nested-defs ...)))
         body ...)]))

;; ~for -----------------------------------------------------------------------
;(define (seq-fst s) (sequence-ref s 0))
;(define (seq-rst s) (sequence-tail s 1))
;(define (seq-empty? s) (zero? (sequence-length s)))
(begin-for-syntax
  (define-splicing-syntax-class for-clause
    (pattern (b:for-binder seq:expr))
    (pattern w:when-clause)
    (pattern br:break-clause))
  (define-splicing-syntax-class body-or-break
    (pattern bb:break-clause)
    (pattern e:expr))
  (define-syntax-class for-binder
    (pattern b:bind #:attr new-b #'b)
    (pattern x:id #:attr new-b #'x
                  #:attr nested-defs #'())
    (pattern (x:id ...) #:attr new-b #'(~vs x ...)
                        #:attr nested-defs #'()))
  (define-splicing-syntax-class when-or-break
    (pattern :when-clause)
    (pattern :break-clause))
  (define-splicing-syntax-class when-clause
    (pattern (~seq #:when guard:expr) #:attr test #'guard)
    (pattern (~seq #:unless guard:expr) #:attr test #'(not guard)))
  (define-splicing-syntax-class break-clause
    (pattern (~seq #:break guard:expr))
    (pattern (~seq #:final guard:expr)))
  ) ; begin-for-syntax
(define-syntax (~for/common stx)
  (syntax-parse stx
    [(_ flatten combiner base (c:for-clause ...) bb:break-clause ... body:expr ...)
     #:with expanded-for
     (let ([depth 0])
       (define res
     (let stxloop ([cs #'(c ... bb ...)])
       (syntax-parse cs
         [() #'(begin body ...)]
         [(([b:for-binder seq:expr]) ... (w:when-or-break) ... rst ...)
;          #:with (s ...) (generate-temporaries #'(b ...))
          #:with (seq-not-empty? ...) (generate-temporaries #'(b ...))
          #:with (seq-next ...) (generate-temporaries #'(b ...))
          #:with new-loop (generate-temporary)
          #:with skip-it #'(new-loop) ;;#'(new-loop (seq-rst s) ...)
          #:with do-it 
;            #`(call-with-values 
;               (λ () #,(stxloop #'(rst ...)))
;               (λ this-iter (call-with-values 
;                        (λ () skip-it)
;                        (λ other-iters (combiner this-iter other-iters)))))
          #`(let ([result #,(stxloop #'(rst ...))]) (combiner result skip-it))
          #:with its-done #'base
          #:with one-more-time 
;            #`(call-with-values 
;               (λ () #,(stxloop #'(rst ...)))
;               (λ this-iter (call-with-values 
;                             (λ () base)
;                             (λ other-iters (combiner this-iter other-iters)))))
            #`(let ([result #,(stxloop #'(rst ...))]) (combiner result base))
          #:with conditional-body
            (let whenloop ([ws (syntax->list #'((w) ...))])
              (if (null? ws)
                  #'do-it
                  (syntax-parse (car ws)
                    [((#:when guard)) #`(if guard #,(whenloop (cdr ws)) skip-it)]
                    [((#:unless guard)) #`(if guard skip-it #,(whenloop (cdr ws)))]
                    [((#:break guard)) #`(if guard its-done #,(whenloop (cdr ws)))]
                    [((#:final guard)) #`(if guard one-more-time #,(whenloop (cdr ws)))])))
            (set! depth (add1 depth))
          #`(let-values ([(seq-not-empty? seq-next) (sequence-generate seq)] ...)
               (let new-loop ()
                 (if (and (seq-not-empty?) ...)
                     (~let ([b.new-b (seq-next)] ...)
;                       #,@(append-map syntax->list (syntax->list #'(b.nested-defs ...)))
                           conditional-body)
                     base)))])))
       (let flatten-loop ([n (- depth 2)] [res res])
         (if (<= n 0) res
           (flatten-loop (sub1 n) #`(flatten #,res)))))
     #'expanded-for]))

(define-syntax (~for/common/L stx) ; accum = foldl
  (syntax-parse stx
    [(_ final combiner base break? (c:for-clause ...) bb:break-clause ... body:expr ...)
     #:with init-accum (generate-temporary)
     #:with expanded-for
     #`(let ([init-accum base])
     #,(let stxloop ([cs #'(c ... bb ...)] [accum #'init-accum])
       (syntax-parse cs
         [() #`(combiner (begin body ...) #,accum)]
         [(([b:for-binder seq:expr]) ... (w:when-or-break) ... rst ...)
          #:with (seq-not-empty? ...) (generate-temporaries #'(b ...))
          #:with (seq-next ...) (generate-temporaries #'(b ...))
          #:with new-loop (generate-temporary)
          #:with new-accum (generate-temporary)
          #:with skip-it #'(new-loop new-accum)
          #:with do-it #`(new-loop #,(stxloop #'(rst ...) #'new-accum))
          #:with its-done #'new-accum
          #:with one-more-time (stxloop #'(rst ...) #'new-accum)
          #:with conditional-body
            (let whenloop ([ws (syntax->list #'((w) ...))])
              (if (null? ws)
                  #'do-it
                  (syntax-parse (car ws)
                    [((#:when guard)) #`(if guard #,(whenloop (cdr ws)) skip-it)]
                    [((#:unless guard)) #`(if guard skip-it #,(whenloop (cdr ws)))]
                    [((#:break guard)) #`(if guard its-done #,(whenloop (cdr ws)))]
                    [((#:final guard)) #`(if guard one-more-time #,(whenloop (cdr ws)))])))
            #`(let-values ([(seq-not-empty? seq-next) (sequence-generate seq)] ...)
                (let new-loop ([new-accum #,accum])
                 (if (and (seq-not-empty?) ... (not (break? new-accum)))
                     (~let ([b.new-b (seq-next)] ...)
;                       #,@(append-map syntax->list (syntax->list #'(b.nested-defs ...)))
                           conditional-body)
                     new-accum)))])))
;       (let flatten-loop ([n (- depth 2)] [res res])
;         (if (<= n 0) res
;           (flatten-loop (sub1 n) #`(flatten #,res)))))
     #'(final expanded-for)]))

(define-syntax (~for*/common stx)
  (syntax-case stx ()
    [(_ g f base (clause ...) body ...)
     (with-syntax 
       ([(new-clause ...)
         (append-map (λ (s) (list s #'#:when #'#t)) (syntax->list #'(clause ...)))])
     #'(~for/common g f base (new-clause ...) body ...))]))
(define-syntax (~for*/common/L stx)
  (syntax-case stx ()
    [(_ fi comb base b? (clause ...) body ...)
     (with-syntax 
       ([(new-clause ...)
         (append-map (λ (s) (list s #'#:when #'#t)) (syntax->list #'(clause ...)))])
     #'(~for/common/L fi comb base b? (new-clause ...) body ...))]))

(define-syntax-rule (~for x ...) (~for/common void void (void) x ...))
(define-syntax-rule (~for/list x ...) 
  (~for/common/L reverse cons null (λ _ #f) x ...))
(define-syntax-rule (~for/last x ...) 
  (~for/common/L identity (λ (y acc) y) #f (λ _ #f) x ...))
(define-syntax-rule (~for/and x ...)
  (~for/common/L identity (λ (y acc) (and acc y)) #t (λ (acc) (not acc)) x ...))
(define-syntax-rule (~for/or x ...)
  (~for/common/L identity (λ (y acc) (or acc y)) #f (λ (acc) acc) x ...))
(define-syntax-rule (~for/first x ...)
  (~for/common/L identity (λ (y acc) y) #f (λ (acc) acc) x ...))
(define-syntax-rule (~for/sum x ...) 
  (~for/common/L identity + 0 (λ _ #f) x ...))
(define-syntax-rule (~for/product x ...) 
  (~for/common/L identity * 1 (λ _ #f) x ...))
(define-syntax (~for/vector stx) 
  (syntax-parse stx
    [(_ (~optional (~seq (~seq #:length len) 
                         (~optional (~seq #:fill fill) #:defaults ([fill #'0]))))
        x ...)
     #`(let ()
         #,(cond [(attribute len)
                  #'(begin
                      (define vec (make-vector len fill))
                      (define vec-len len)
                      (define vec-expandable? #f))]
                  [else
                   #'(begin
                       (define vec (make-vector 16))
                       (define vec-len 16)
                       (define vec-expandable? #t))])
         (define i 0)
         (~for/common/L 
          identity
          ;; need the unless to handle when nested loops return
          (λ (y acc) (vector-set! vec i y) (set! i (add1 i)))
          (void)
          (λ _ (cond 
                 [(>= i vec-len)
                  (cond 
                    [(not vec-expandable?) #t]
                    [else
                     (define new-vec (make-vector (* vec-len 2)))
                     (vector-copy! new-vec 0 vec 0 vec-len)
                     (set! vec-len (* 2 vec-len))
                     (set! vec new-vec)
                     #f])]
                 [else #f]))
          x ...)
         (cond [vec-expandable?
                (define new-vec (make-vector i))
                (vector-copy! new-vec 0 vec 0 i)
                new-vec]
               [else vec]))]))
(define-syntax (~for*/vector stx) 
  (syntax-parse stx
    [(_ (~optional (~seq (~seq #:length len) 
                         (~optional (~seq #:fill fill) #:defaults ([fill #'0]))))
        x ...)
     #`(let ()
         #,(cond [(attribute len)
                  #'(begin
                      (define vec (make-vector len fill))
                      (define vec-len len)
                      (define vec-expandable? #f))]
                  [else
                   #'(begin
                       (define vec (make-vector 16))
                       (define vec-len 16)
                       (define vec-expandable? #t))])
         (define i 0)
         (~for*/common/L 
          identity
          (λ (y acc) (vector-set! vec i y) (set! i (add1 i)))
          (void)
          (λ _ 
            (cond 
                 [(>= i vec-len)
                  (cond 
                    [(not vec-expandable?) #t]
                    [else
                     (define new-vec (make-vector (* vec-len 2)))
                     (vector-copy! new-vec 0 vec 0 vec-len)
                     (set! vec-len (* 2 vec-len))
                     (set! vec new-vec)
                     #f])]
                 [else #f]))
          x ...)
         (cond [vec-expandable?
                (define new-vec (make-vector i))
                (vector-copy! new-vec 0 vec 0 i)
                new-vec]
               [else vec]))]))
(define-syntax (~for/fold stx) ; foldl
  (syntax-parse stx
    [(_ ([accum init] ...) (c:for-clause ...) bb:break-clause ... body:expr ...)
     #:with expanded-for
     (let stxloop ([cs #'(c ... bb ...)])
       (syntax-parse cs
         [() #'(begin body ...)]
         [(([b:for-binder seq:expr]) ... (w:when-or-break) ... rst ...)
          #:with (seq-not-empty? ...) (generate-temporaries #'(b ...))
          #:with (seq-next ...) (generate-temporaries #'(b ...))
          #:with new-loop (generate-temporary)
          #:with skip-it #'(new-loop accum ...)
          #:with do-it 
            #`(call-with-values (λ () #,(stxloop #'(rst ...))) new-loop)
          #:with its-done #'(values accum ...)
          #:with one-more-time 
            #`(call-with-values (λ () #,(stxloop #'(rst ...))) values)
          #:with conditional-body
            (let whenloop ([ws (syntax->list #'((w) ...))])
              (if (null? ws)
                  #'do-it
                  (syntax-parse (car ws)
                    [((#:when guard)) #`(if guard #,(whenloop (cdr ws)) skip-it)]
                    [((#:unless guard)) #`(if guard skip-it #,(whenloop (cdr ws)))]
                    [((#:break guard)) #`(if guard its-done #,(whenloop (cdr ws)))]
                    [((#:final guard)) #`(if guard one-more-time #,(whenloop (cdr ws)))])))
            #`(let-values ([(seq-not-empty? seq-next) (sequence-generate seq)] ...)
                (let new-loop ([accum init] ...)
                  (if (and (seq-not-empty?) ...)
                      (~let ([b.new-b (seq-next)] ...)
                        conditional-body)
                      (values accum ...))))]))
     #'expanded-for]))
(define-syntax (~for/lists stx)
  (syntax-parse stx
    [(_ (accum ...) (c:for-clause ...) bb:break-clause ... body:expr ...)
     #:with expanded-for
     (let stxloop ([cs #'(c ... bb ...)])
       (syntax-parse cs
         [() #'(begin body ...)]
         [(([b:for-binder seq:expr]) ... (w:when-or-break) ... rst ...)
          #:with (seq-not-empty? ...) (generate-temporaries #'(b ...))
          #:with (seq-next ...) (generate-temporaries #'(b ...))
          #:with (x ...) (generate-temporaries #'(accum ...))
          #:with (y ...) (generate-temporaries #'(accum ...))
          #:with (z ...) (generate-temporaries #'(accum ...))
          #:with new-loop (generate-temporary)
          #:with skip-it #'(new-loop)
          #:with do-it 
            #`(call-with-values 
               (λ () #,(stxloop #'(rst ...))) 
               (λ (x ...) 
                 (call-with-values 
                  (λ () (new-loop))
                  (λ (y ...) (values (cons x y) ...)))))
          #:with its-done #'(values z ...)
          #:with one-more-time 
            #`(call-with-values
               (λ () #,(stxloop #'(rst ...)))
               (λ (x ...) (values (cons x y) ...)))
          #:with conditional-body
            (let whenloop ([ws (syntax->list #'((w) ...))])
              (if (null? ws)
                  #'do-it
                  (syntax-parse (car ws)
                    [((#:when guard)) #`(if guard #,(whenloop (cdr ws)) skip-it)]
                    [((#:unless guard)) #`(if guard skip-it #,(whenloop (cdr ws)))]
                    [((#:break guard)) #`(if guard its-done #,(whenloop (cdr ws)))]
                    [((#:final guard)) #`(if guard one-more-time #,(whenloop (cdr ws)))])))
            #`(let-values ([(seq-not-empty? seq-next) (sequence-generate seq)] ...)
                (let ([z null] ...)
                  (let new-loop ()
                    (if (and (seq-not-empty?) ...)
                        (~let ([b.new-b (seq-next)] ...)
                              conditional-body)
                        (values z ...)))))]))
     #'expanded-for]))
(define-syntax-rule (~for* x ...) (~for*/common void void (void) x ...))
(define-syntax-rule (~for*/list x ...) 
  (~for*/common/L reverse
                cons ;  (λ (this others) (append this (car others)))
                null
                (λ _ #f)
                x ...))
     
;#;(define-syntax (~for/list stx)
;  (syntax-parse stx
;    [(_ ([x:id-or-bind seq] ... 
;         (~optional (~seq #:when tst1) #:defaults ([tst1 #'#t]))
;         (~optional (~seq #:unless tst2) #:defaults ([tst2 #'#f])))
;        body ...)
;     #:with (s ...) (generate-temporaries #'(x ...))
;     #:with skip-body #'(loop (seq-rst s) ...)
;     #:with do-body #'(let ([result (begin body ...)]) (cons result skip-body))
;     #`(let loop ([s seq] ...)
;         (if (or (seq-empty? s) ...) 
;             null
;             (~let ([x (seq-fst s)] ...)
;               (if (and tst1 (not tst2)) do-body skip-body))))]))
     
