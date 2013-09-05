#lang racket
(require (for-syntax syntax/parse
                     racket/syntax
                     racket/list)) ; append-map
(require (for-syntax syntax/parse/experimental/template))
(require (for-syntax "stx-utils.rkt"))
(require racket/unsafe/ops)
(require racket/private/for)

;; TODO:
;; [x] 2013-08-26: ~let doesn't support ~vs DONE: 2013-08-26
;; [o] 2013-08-24: bug: creating "bound-ids" stx prop breaks ~define
;;                 (as in it doesn't recognize :bind classes anymore)
;; [o] 2013-08-22: get rid of suprious defines for generic binds nested in
;;                 generic ~v binding -- use splicing
;; [o] 2013-08-21: syntax object for ~m and ~vs is void? 
;;                 can I put anything useful here?
;; [o] 2013-08-21: create generic form for syntax to 
;;                 automatically define struct accessors?
;; [o] 2013-08-21: fix error msgs
;;                 - named ~let dup id references define

(provide ~vs $: $list $null ; dont export ~m
         ~define ~lambda ~case-lambda ~case-define
         (rename-out [~lambda ~λ] [~lambda ~lam] [~lambda ~l] 
                     [~define ~def] [~define ~d]
                     [~m $] [~vs ⋈]
                     [~case-lambda ~case-lam] [~case-define ~case-def])
         ~let ~let* ~letrec
         ~for ~for/list ~for/vector ~for/fold ~for/lists ~for/first ~for/last 
         ~for/and ~for/or ~for/sum ~for/product 
         ~for/hash ~for/hasheq ~for/hasheqv
         ~for* ~for*/list ~for*/fold ~for*/vector ~for*/lists 
         ~for*/first ~for*/last ~for*/and ~for*/or ~for*/sum ~for*/product  
         ~for*/hash ~for*/hasheq ~for*/hasheqv
         define-match-bind ~struct)

;; (define-generic-stx bind 
;;   (definer letter ids let-only nested-definers nested-idss))

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
  ;; - need the following separate class because some instances 
  ;;   (ie anything involving values) needs the target of the bind to work
  ;; - for example, values can't be used in function defs
  ;; - so this is a subset of :bind, which is used in things like fn defs
  (define-syntax-class bind/non-let
    #:description "a generic bind instance for define contexts"
    #:auto-nested-attributes
    (pattern :bind 
             #:fail-when (bind-let-only? #'expanded-b)
                         (format "can't use ~a pattern in non-let binding ctxt"
                                 (syntax->datum #'b))))
  (define-syntax-class bind/let-only
    #:description "a generic bind instance for let contexts only"
    #:auto-nested-attributes
    (pattern :bind 
             #:fail-when (not (bind-let-only? #'expanded-b))
             (format "can't use ~a pattern in let-only binding ctxt"
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

;; generic match bindings where the outer form is a list or cons
;; ie, just match list or match cons
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

(define-syntax (define-match-bind stx)
  (syntax-parse stx
    [(_ (name x ...))
     #:with $name (format-id #'name "$~a" #'name)
     #:with match-$name-define (format-id #'here "match-~a-define" #'name)
     #:with match-$name-let (format-id #'here "match-~a-let" #'name)
     #'(begin
         (define-syntax-rule (match-$name-define (x ...) e) (match-define (name x ...) e))
         (define-syntax-rule (match-$name-let ([(x ...) e] (... ...)) body (... ...)) 
           (match-let ([(name x ...) e] (... ...)) body (... ...)))
         (define-syntax ($name stx)
           (syntax-parse stx 
             [(_ x ...) (add-syntax-properties
                         `([definer ,#'match-$name-define]
                           [letter ,#'match-$name-let]
                           [ids ,(syntax->datum #'(x ...))]
                           [let-only #f]
                           [nested-definers ,#'()]
                           [nested-idss ,null])
                         #'(void))])))]))


(begin-for-syntax ;; ~struct syntax classes
  (define-syntax-class struct-field
    (pattern field:id #:attr name #'field)
    (pattern [field:id opt ...] #:attr name #'field))
  ) ; end begin-for-syntax
(define-syntax (~struct stx)
  (syntax-parse stx 
    [(_ id super ... (field:struct-field ...) opt ...)
     #'(begin
         (struct id super ... (field ...) opt ...)
         (define-match-bind (id field.name ...)))]))
  

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
             #:attr tmp (generate-temporary)
             #:attr new-arg #'(tmp)
             #:attr def #`((definer ids tmp)))
    (pattern name:id 
             #:attr new-arg #'(name)
             #:attr def #'())
    (pattern [:bind/non-let default] 
             #:attr tmp (generate-temporary)
             #:attr new-arg #'([tmp default])
             #:attr def #'((definer ids tmp)))
    (pattern [name:id default] 
             #:attr new-arg #'([name default])
             #:attr def #'())
    (pattern (~seq kw:keyword :bind/non-let) 
             #:attr tmp (generate-temporary)
             #:attr new-arg #'(kw tmp)
             #:attr def #'((definer ids tmp)))
    (pattern (~seq kw:keyword name:id) 
             #:attr new-arg #'(kw name)
             #:attr def #'())
    (pattern (~seq kw:keyword [:bind/non-let default]) 
             #:attr tmp (generate-temporary)
             #:attr new-arg #'(kw [tmp default])
             #:attr def #'((definer ids tmp)))
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
                  #:attr nested-defs #'())
     ;; match values, like in for forms
    (pattern (x:id ...) #:attr name (generate-temporary) ; shouldnt get used
                        #:attr definer #'define-values
                        #:attr letter #'let-values
                        #:attr ids #'(x ...)
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
     #'(let ([fn (~lambda ?header ?body ...)] ...) (λ args new-body))]))

(define-syntax (~case-define stx)
  (syntax-parse stx #:datum-literals (→)
    [(_ f (x ... → body ...) ...)
     #'(define f (~case-lambda [(x ...) body ...] ...))]
    [(_ f clause ...) #'(define f (~case-lambda clause ...))]))
  
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

;; ~for forms -----------------------------------------------------------------
(begin-for-syntax
  (define-splicing-syntax-class seq-binding (pattern (b:for-binder seq:expr)))
  (define-splicing-syntax-class for-clause 
    (pattern :seq-binding) (pattern :when-or-break))
  (define-syntax-class for-binder
    (pattern :bind/let-only #:with xs (generate-temporaries (attribute ids)))
    (pattern :bind #:with xs (list (generate-temporary)))
    (pattern x:id #:attr xs #'(x))
    (pattern (x:id ...) #:attr xs #'(x ...)))
  (define-splicing-syntax-class when-or-break 
    (pattern :when-clause) (pattern :break-clause))
  (define-splicing-syntax-class when-clause
    (pattern (~seq #:when guard:expr) #:attr test #'guard)
    (pattern (~seq #:unless guard:expr) #:attr test #'(not guard)))
  (define-splicing-syntax-class break-clause
    (pattern (~seq #:break guard:expr)) (pattern (~seq #:final guard:expr)))
  ) ; begin-for-syntax for ~for forms

(define id-fn identity)

(define-syntax (~for/common stx) ; essentially a foldl (ie uses an accum(s))
  (syntax-parse stx
    ;; this clause allows naming of the accums so the body can reference them
    ;; -- used by for/fold and for/lists
    [(_ (~optional (~seq #:final final))
        combiner
        (~optional (~seq #:break? break?))
        ([accum base] ...)
        (c:for-clause ...) bb:break-clause ... body:expr ...)
     #:with expanded-for
     (let ([one-accum? (= 1 (length (syntax->list #'(accum ...))))])
       #`(let ([accum base] ...)
           #,(let stxloop ([cs #'(c ... bb ...)])
               (syntax-parse cs
                 [() #`(call-with-values (λ () body ...)
                                         (λ res (apply combiner accum ... res)))]
                 [(([b:for-binder seq:expr]) ... (w:when-or-break) ... rst ...)
                  (with-syntax* 
                   ([one-more-time (stxloop #'(rst ...))]
                    [((ys ...) ...) #'(b.xs ...)]
                    [(([outer-binding ...]
                       outer-check
                       [loop-binding ...]
                       pos-guard
                       ;[inner-binding ...]
                       [[xs next] ...]
                       pre-guard
                       post-guard
                       [loop-arg ...]) ...)
                     (map (λ (x) (expand-clause x x)) (syntax->list #'([b.xs seq] ...)))]
                    [new-loop (generate-temporary)]
                    [its-done (if one-accum? 
                                  (car (syntax->list #'(accum ...)))
                                  #'(values accum ...))]
                    [skip-it #`(if (and post-guard ...) 
                                   (new-loop accum ... loop-arg ... ...)
                                   its-done)]
                    [do-it 
                     (if one-accum?
                         #`(if (and post-guard ...) 
                               (new-loop one-more-time loop-arg ... ...)
                               one-more-time)
                         #`(if (and post-guard ...)
                               (call-with-values 
                                (λ () one-more-time) 
                                (λ res (apply new-loop 
                                              (append res (list loop-arg ... ...)))))
                               one-more-time))]
                    [conditional-body
                     (let whenloop ([ws (syntax->list #'((w) ...))])
                       (if (null? ws)
                           #'do-it
                           (syntax-parse (car ws)
                             [((#:when guard)) 
                              (if (eq? (syntax-e #'guard) #t)
                                  (whenloop (cdr ws))
                                  #`(if guard #,(whenloop (cdr ws)) skip-it))]
                             [((#:unless guard)) #`(if guard skip-it #,(whenloop (cdr ws)))]
                             [((#:break guard)) #`(if guard its-done #,(whenloop (cdr ws)))]
                             [((#:final guard)) #`(if guard one-more-time #,(whenloop (cdr ws)))])))])
                   #`(let-values #;([(more? next) (sequence-generate seq)] ...) (outer-binding ... ...)
                       ;; must shadow accum in new loop, in case body references it
                       (let new-loop ([accum accum] ... loop-binding ... ...)
                         ;; must check break? first, bc more? pulls an item
                         (if (and #,@(if (attribute break?) #'((not (break? accum ...))) #'())
                                  pos-guard ...)
                             (let-values ([xs next] ... ...)
                               (if (and pre-guard ...)
                                   (~let ([b (values ys ...)] ...) conditional-body)
                                   its-done))
                             its-done))))]))))
     (if (attribute final)
         (if (= 1 (length (syntax->list #'(accum ...)))) ; one accum
             #'(final expanded-for)
             #'(call-with-values (λ () expanded-for) final))
         #'expanded-for)]
    ;; this clause has unnamed accums, name them and then call the first clause
    [(_ (~optional (~seq #:final final))
        combiner
        (~optional (~seq #:break? break?))
        (base ...) 
        (c:for-clause ...) bb:break-clause ... body:expr ...)
     #:with (accum ...) (generate-temporaries #'(base ...))
     #`(~for/common #,@(if (attribute final) #'(#:final final) #'())
                    combiner 
                    #,@(if (attribute break?) #'(#:break? break?) #'())
                    ([accum base] ...) 
                    #,@(template (((?@ . c) ...) (?@ . bb) ... body ...)))]))

;; inserts #:when #t between each (non-#:when/unless) clause
(define-syntax (~for*/common stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:final fin))
        comb
        (~optional (~seq #:break? b?))
        (base ...) 
        ((~seq sb:seq-binding ... wb:when-or-break ...) ...) body ...)
     #:with ((new-sb ...) ...)
     ;; append accounts for list added by splicing-syntax-class
       (map (λ (ss) (append-map 
                     (λ (s) (append (syntax->list s) (list #'#:when #'#t)))
                     (syntax->list ss)))
            (syntax->list #'((sb ...) ...)))
     ;; wb is also a list (due to splicing-stx-class)
     #:with (new-clause ...) 
       (template ((?@ . (new-sb ... (?@ . ((?@ . wb) ...)))) ...))
     #`(~for/common #,@(if (attribute fin) #'(#:final fin) #'())
                    comb 
                    #,@(if (attribute b?) #'(#:break? b?) #'())
                    (base ...) (new-clause ...) body ...)]))

(define-syntax (mk~for/ stx)
  (syntax-parse stx 
    [(_ name combiner (base ...) (~optional (~seq #:final fin))
                                 (~optional (~seq #:break? b?)))
     #:with new-name (format-id #'name "~~for/~a" #'name)
     #:with new-name* (format-id #'name "~~for*/~a" #'name)
     #`(begin 
         (define-syntax-rule (new-name x (... ...)) 
           (~for/common #,@(if (attribute fin) #'(#:final fin) #'())
                        combiner 
                        #,@(if (attribute b?) #'(#:break? b?) #'())
                        (base ...) x (... ...)))
         (define-syntax-rule (new-name* x (... ...)) 
           (~for*/common #,@(if (attribute fin) #'(#:final fin) #'()) 
                         combiner 
                         #,@(if (attribute b?) #'(#:break? b?) #'())
                         (base ...) x (... ...))))]))

(define-syntax-rule (~for x ...) (~for/common #:final void void ((void)) x ...))
(define-syntax-rule (~for* x ...) (~for*/common #:final void void ((void)) x ...))

;; ~for/list ~for*/list and ~for/lists probably need a "right folding" for/common
;;   but for now, just reverse the output
;; - have to reverse args in cons bc acc is first
(mk~for/ list (λ (acc y) (unsafe-cons-list y acc)) (null) #:final reverse)
;; break as soon as we find something
(mk~for/ first (λ (acc y) y) (#f) #:break? id-fn)
(mk~for/ last (λ (acc y) y) (#f))
(mk~for/ and (λ (acc y) (and acc y)) (#t) #:break? not)
(mk~for/ or (λ (acc y) (or acc y)) (#f) #:break? id-fn)
(mk~for/ sum + (0))
(mk~for/ product * (1))
(mk~for/ hash hash-set ((hash)))
(mk~for/ hasheq hash-set ((hasheq)))
(mk~for/ hasheqv hash-set ((hasheqv)))

(define-syntax (~for/fold stx) ; foldl
  (syntax-parse stx
    [(_ ([accum init] ...) (c:for-clause ...) bb:break-clause ... body:expr ...)
     #:with (res ...) (generate-temporaries #'(accum ...))
     ;; combiner drops old accums and uses result(s) of body as current accums
     (template (~for/common
                #:final values (λ (accum ... res ...) (values res ...))
                ([accum init] ...) ((?@ . c) ...) (?@ . bb) ... body ...))]))
(define-syntax (~for*/fold stx)
  (syntax-parse stx
    [(_ ([accum init] ...) (c:for-clause ...) bb:break-clause ... body:expr ...)
     #:with (res ...) (generate-temporaries #'(accum ...))
     ;; combiner drops old accums and uses result(s) of body as current accums
     (template (~for*/common 
                #:final values (λ (accum ... res ...) (values res ...))
                ([accum init] ...) ((?@ . c) ...) (?@ . bb) ... body ...))]))

(define-syntax (~for/lists stx)
  (syntax-parse stx
    [(_ (accum ...) (c:for-clause ...) bb:break-clause ... body:expr ...)
     #:with (res ...) (generate-temporaries #'(accum ...))
     ;; combiner drops old accums and uses result(s) of body as current accums
     (template (~for/common 
                #:final (λ (accum ...) (values (reverse accum) ...))
                (λ (accum ... res ...) (values (unsafe-cons-list res accum) ...))
                ([accum null] ...) ((?@ . c) ...) (?@ . bb) ... body ...))]))
(define-syntax (~for*/lists stx)
  (syntax-parse stx
    [(_ (accum ...) (c:for-clause ...) bb:break-clause ... body:expr ...)
     #:with (res ...) (generate-temporaries #'(accum ...))
     ;; combiner drops old accums and uses result(s) of body as current accums
     (template (~for*/common 
                #:final (λ (accum ...) (values (reverse accum) ...))
                (λ (accum ... res ...) (values (unsafe-cons-list res accum) ...))
                ([accum null] ...) ((?@ . c) ...) (?@ . bb) ... body ...))]))

;; ~for/vector is an imperative mess
(define-syntax (~for/vector stx) 
  (syntax-parse stx
    [(_ (~optional (~seq (~seq #:length len) 
                         (~optional (~seq #:fill fill) #:defaults ([fill #'0]))))
        x ...)
     (if (attribute len)
         ;; if #:length is specified, then we need a break? because there may
         ;; be more iterations than #:length
         #'(let ([vec (make-vector len fill)]
                 [vec-len len])
             (define i 0)
             (~for/common 
              #:final (λ _ vec)
              (λ (acc y) (unsafe-vector-set! vec i y) (set! i (add1 i))) ; combiner
              #:break? (λ _ (>= i vec-len))
              ((void)) ; base ...
              x ...))
         ;; repeatedly checking break? is slow so if no #:length is given,
         ;; first build list and then copy into a result vector
         #'(~for/common
            #:final 
            (λ (lst) 
              (let* ([vec-len (length lst)]
                     [vec (make-vector vec-len)])
                (let loop ([n vec-len] [lst lst])
                  (if (zero? n) vec
                      (let ([n-1 (sub1 n)])
                        (unsafe-vector-set! vec n-1 (unsafe-car lst))
                        (loop n-1 (unsafe-cdr lst)))))))
            (λ (acc y) (unsafe-cons-list y acc))
            (null)
            x ...))]))
(define-syntax (~for*/vector stx) 
  (syntax-parse stx
    [(_ (~optional (~seq (~seq #:length len) 
                         (~optional (~seq #:fill fill) #:defaults ([fill #'0]))))
        x ...)
     (if (attribute len)
         ;; if #:length is specified, then we need a break? because there may
         ;; be more iterations than #:length
         #'(let ([vec (make-vector len fill)]
                 [vec-len len])
             (define i 0)
             (~for*/common 
              #:final (λ _ vec)
              (λ (acc y) (unsafe-vector-set! vec i y) (set! i (add1 i))) ; combiner
              #:break? (λ _ (>= i vec-len))
              ((void)) ; base ...
              x ...))
         ;; repeatedly checking break? is slow so if no #:length is given,
         ;; first build list and then copy into a result vector
         #'(~for*/common
            #:final 
            (λ (lst) 
              (let* ([vec-len (length lst)]
                     [vec (make-vector vec-len)])
                (let loop ([n vec-len] [lst lst])
                  (if (zero? n) vec
                      (let ([n-1 (sub1 n)])
                        (unsafe-vector-set! vec n-1 (unsafe-car lst))
                        (loop n-1 (unsafe-cdr lst)))))))
            (λ (acc y) (unsafe-cons-list y acc))
            (null)
            x ...))]))



;; this is a right-folding ~for/common
;;  it's currently unused, but would probably be faster for ~for/list
;#;(define-syntax (~for/common/foldr stx)
;  (syntax-parse stx
;    [(_ flatten combiner base (c:for-clause ...) bb:break-clause ... body:expr ...)
;     #:with expanded-for
;     (let ([depth 0])
;       (define res
;     (let stxloop ([cs #'(c ... bb ...)])
;       (syntax-parse cs
;         [() #'(begin body ...)]
;         [(([b:for-binder seq:expr]) ... (w:when-or-break) ... rst ...)
;;          #:with (s ...) (generate-temporaries #'(b ...))
;          #:with (seq-not-empty? ...) (generate-temporaries #'(b ...))
;          #:with (seq-next ...) (generate-temporaries #'(b ...))
;          #:with new-loop (generate-temporary)
;          #:with skip-it #'(new-loop) ;;#'(new-loop (seq-rst s) ...)
;          #:with do-it 
;;            #`(call-with-values 
;;               (λ () #,(stxloop #'(rst ...)))
;;               (λ this-iter (call-with-values 
;;                        (λ () skip-it)
;;                        (λ other-iters (combiner this-iter other-iters)))))
;          #`(let ([result #,(stxloop #'(rst ...))]) (combiner result skip-it))
;          #:with its-done #'base
;          #:with one-more-time 
;;            #`(call-with-values 
;;               (λ () #,(stxloop #'(rst ...)))
;;               (λ this-iter (call-with-values 
;;                             (λ () base)
;;                             (λ other-iters (combiner this-iter other-iters)))))
;            #`(let ([result #,(stxloop #'(rst ...))]) (combiner result base))
;          #:with conditional-body
;            (let whenloop ([ws (syntax->list #'((w) ...))])
;              (if (null? ws)
;                  #'do-it
;                  (syntax-parse (car ws)
;                    [((#:when guard)) #`(if guard #,(whenloop (cdr ws)) skip-it)]
;                    [((#:unless guard)) #`(if guard skip-it #,(whenloop (cdr ws)))]
;                    [((#:break guard)) #`(if guard its-done #,(whenloop (cdr ws)))]
;                    [((#:final guard)) #`(if guard one-more-time #,(whenloop (cdr ws)))])))
;            (set! depth (add1 depth))
;          #`(let-values ([(seq-not-empty? seq-next) (sequence-generate seq)] ...)
;               (let new-loop ()
;                 (if (and (seq-not-empty?) ...)
;                     (~let ([b.new-b (seq-next)] ...)
;;                       #,@(append-map syntax->list (syntax->list #'(b.nested-defs ...)))
;                           conditional-body)
;                     base)))])))
;       (let flatten-loop ([n (- depth 2)] [res res])
;         (if (<= n 0) res
;           (flatten-loop (sub1 n) #`(flatten #,res)))))
;     #'expanded-for]))
