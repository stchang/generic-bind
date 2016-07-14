#lang racket
(require syntax/parse
         syntax/parse/define
         racket/unsafe/ops
         "syntax-parse-utils.rkt"
         "nested-binds-helper.rkt"
         (for-syntax syntax/parse
                     racket/syntax
                     racket/list ; append-map
                     racket/format
                     syntax/parse/experimental/template
                     "stx-utils.rkt"
                     (only-in syntax/unsafe/for-transform expand-for-clause)
                     syntax/for-body))

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

(provide ~vs $: $list $null $stx $and $c ; dont export ~m
         ~define ~lambda ~case-lambda ~case-define ~define/contract
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
  (define (bind-names stx) ; used only for let-only bindings
    (and (bind-let-only? stx)
         (syntax-property stx 'names)))
  
  (define (bind? stx) (bind-definer stx))
  (define (bind-let-only? stx) (syntax-property stx 'let-only))
  
  (define-syntax-class bind
    #:description "a generic bind instance"
    (pattern b 
      #:with expanded-b (try-local-expand #'b)
      #:when (bind? #'expanded-b)
      #:attr definer (bind-definer #'expanded-b)
      #:attr letter (bind-letter #'expanded-b)
      ))
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
                                 (syntax->datum #'b))
             #:attr name (generate-temporary)
             ))
  (define-syntax-class bind/let-only
    #:description "a generic bind instance for let contexts only"
    #:auto-nested-attributes
    (pattern :bind 
             #:fail-when (not (bind-let-only? #'expanded-b))
             (format "can't use ~a pattern in let-only binding ctxt"
                     (syntax->datum #'b))
             #:attr names (bind-names #'expanded-b)
             ))
  (define-syntax-class id-or-bind/non-let
    #:auto-nested-attributes
    (pattern :bind/non-let)
    (pattern x:id #:attr name (generate-temporary)
                  #:attr definer #'define
                  #:attr letter #'let
                  ))
  (define-syntax-class id-or-bind/let
    #:auto-nested-attributes
    (pattern :bind/let-only)
    (pattern :id-or-bind/non-let #:attr names #'(name))
    (pattern (x:id ...)
             #:with names (generate-temporaries #'(x ...))
             #:attr definer #'define-values
             #:attr letter #'let-values
             ))
  ) ;; end begin-for-syntax

;; ----------------------------------------------------------------------------
;; define-syntax/parse/gen-bind

(define-for-syntax make-gen-bind-match-expander-proc
  (make-gen-bind-match-expander-proc-maker #'~define))

(define-syntax/parse define-syntax/gen-bind
  [(define-syntax/gen-bind id:id parser-proc-expr:expr)
   #'(define-match-expander id
       (make-gen-bind-match-expander-proc)
       parser-proc-expr)])

(define-syntax/parse define-syntax/parse/gen-bind
  [(define-syntax/parse/gen-bind id:id #:stx stx:id option-or-clause ...)
   #'(define-syntax/gen-bind id
       (lambda (stx)
         (syntax-parse stx option-or-clause ...)))]
  [(define-synatx/parse/gen-bind id:id option-or-clause ...)
   #'(define-syntax/parse/gen-bind id #:stx stx option-or-clause ...)])

;; ----------------------------------------------------------------------------
;; generic bind "instances"

;; match generic bind instance 
(define-syntax/parse/gen-bind ~m
  [(_ pat) (add-bind-properties #'$-definer #'$-letter #'(void))])

(define-simple-macro ($-definer ($ pat) expr)
   (~match-define pat expr))

(define-simple-macro ($-letter ([($ pat) expr]) body ...+)
   (~match-let ([pat expr]) body ...))

(define-syntax/parse ~match-define #:stx stx
  [(match-def pat:expr expr:expr)
   (quasisyntax/loc stx
     (splicing-with-new-match-pat-def-set
      #,(syntax/loc stx (match-define pat expr))
      (define-current-match-pat-def-set)))])

(define-syntax/parse ~match-let #:stx stx
  [(_ stuff body ...+)
   #'(with-new-match-pat-def-set
      (match-let stuff
        (define-current-match-pat-def-set)
        body ...))])

(define-syntax/parse/gen-bind $stx
  [(_ pat pat-dir ...)
   (add-bind-properties #'$stx-definer #'$stx-letter #'(void))])

(define-simple-macro ($stx-definer ($stx pat pat-dir ...) stx)
   (define/syntax-parse pat pat-dir ... stx))

(define-simple-macro ($stx-letter ([($stx pat pat-dir ...) stx] ...) body ...)
  (syntax-parse (stx ...)
    [(pat ...) pat-dir ... ... body ...]))


;; generic match bindings where the outer form is a list or cons
;; ie, just match list or match cons
(define-simple-macro (match-listrest-define (_ x ... (~datum :) rst) e)
  (~match-define (list-rest x ... rst) e))
(define-simple-macro (match-listrest-let ([(_ x ... (~datum :) rst) e] ...) body ...) 
  (~match-let ([(list-rest x ... rst) e] ...) body ...))
(define-syntax-rule (match-list-define (_ x ...) e)
  (~match-define (list x ...) e))
(define-syntax-rule (match-list-let ([(_ x ...) e] ...) body ...) 
  (~match-let ([(list x ...) e] ...) body ...))
(define-syntax/parse/gen-bind $list #:datum-literals (:)
  [(_ x ... : rst) (add-bind-properties #'match-listrest-define #'match-listrest-let #'(void))]
  [(_ x ...) (add-bind-properties #'match-list-define #'match-list-let #'(void))])
(define-syntax-rule (match-cons-define (_ x xs) e) (~match-define (cons x xs) e))
(define-syntax-rule (match-cons-let ([(_ x xs) e] ...) body ...) 
  (~match-let ([(cons x xs) e] ...) body ...))
(define-syntax/parse/gen-bind $:
  [(_ x xs) (add-bind-properties #'match-cons-define #'match-cons-let #'(void))])
(define-syntax-rule (match-null-define _ e) ; ignore args
  (~match-define '() e))
(define-syntax-rule (match-null-let ([_ e] ...) body ...) 
  (~match-let (['() e] ...) body ...))
(define-syntax/parse/gen-bind $null
  [_ (add-bind-properties #'match-null-define #'match-null-let #'(void))])

(define-syntax/parse define-match-bind
  [(_ (name x ...))
   #:with $name (format-id #'name "$~a" #'name)
   #:with match-$name-define (format-id #'here "match-~a-define" #'name)
   #:with match-$name-let (format-id #'here "match-~a-let" #'name)
   #'(begin
       (define-syntax-rule (match-$name-define (_ x ...) e) (~match-define (name x ...) e))
       (define-syntax-rule (match-$name-let ([(_ x ...) e] (... ...)) body (... ...)) 
         (~match-let ([(name x ...) e] (... ...)) body (... ...)))
       (define-syntax/parse/gen-bind $name
         [(_ x ...) (add-bind-properties #'match-$name-define #'match-$name-let #'(void))]))]
  [(_ name)
   #:with $name (format-id #'name "$~a" #'name)
   #:with match-$name-define (format-id #'here "match-~a-define" #'name)
   #:with match-$name-let (format-id #'here "match-~a-let" #'name)
   #:with ooo (quote-syntax ...)
   #'(begin
       (define-syntax-rule (match-$name-define (_ x ooo) e) (~match-define (name x ooo) e))
       (define-syntax-rule (match-$name-let ([(_ x ooo) e] (... ...)) body (... ...)) 
         (~match-let ([(name x ooo) e] (... ...)) body (... ...)))
       (define-syntax/parse/gen-bind $name
         [(_ x ooo) (add-bind-properties #'match-$name-define #'match-$name-let #'(void))]))]
  )


(begin-for-syntax ;; ~struct syntax classes
  (define-syntax-class struct-field
    (pattern field:id #:attr name #'field)
    (pattern [field:id opt ...] #:attr name #'field))
  ) ; end begin-for-syntax
(define-syntax/parse ~struct
  [(_ id:id super:id ... (field:struct-field ...) opt ...)
   #'(begin
       (struct id super ... (field ...) opt ...)
       (define-match-bind (id field.name ...)))])


;; values generic bind instance
;; - supports (one-level only) nested (non-let-restricted) generic binds
;;   (currently this is only the generic bind instance for match)
;;   (to support artitrary nesting, need to get the nested-defs of each x:v-bind)
(define-syntax/parse/gen-bind ~vs
  [(_ x:id-or-bind/non-let ...)
   (add-bind-properties #'vs-definer #'vs-letter #t #'(x.name ...) #'(void))])
(define-simple-macro (vs-definer (_ x:id-or-bind/non-let ...) expr)
  (begin
    (define-values (x.name ...) expr)
    (~define x x.name) ...))
(define-simple-macro (vs-letter ([(_ x:id-or-bind/non-let ...) expr]) body ...)
  (let-values ([(x.name ...) expr])
    (~define x x.name) ...
    body ...))

;; $and
(define-syntax/parse/gen-bind $and
  [($and x:id-or-bind/non-let ...)
   (add-bind-properties #'$and-definer #'$and-letter #'(void))]
  [($and x0:id-or-bind/let x:id-or-bind/let ...)
   (add-bind-properties #'$and-definer #'$and-letter #t #'x0.names #'(void))])
(define-syntax/parse $and-definer
  [(def (_) expr)
   #'(define-values () (begin expr (values)))]
  [(def (_ x0:id-or-bind/let x:id-or-bind/let ...) expr)
   #'(begin
       (define-values x0.names expr)
       (x0.definer x0 (values . x0.names))
       (x.definer x (values . x0.names)) ...)])
(define-syntax/parse $and-letter
  [(letter ([(_) expr]) body ...+)
   #'(let () expr body ...)]
  [(letter ([(_ x0:id-or-bind/let x:id-or-bind/let ...) expr]) body ...+)
   #'(let-values ([x0.names expr])
       (x0.definer x0 (values . x0.names))
       (x.definer x (values . x0.names)) ...
       body ...)])

;; $c for contracts
(define-syntax/parse/gen-bind $c #:literals (values)
  [($c x:id-or-bind/non-let c:expr)
   (add-bind-properties #'$c-definer #'$c-letter #'(void))]
  [($c x:bind/let-only (values c:expr ...))
   (add-bind-properties #'$c-definer #'$c-letter #t #'x.names #'(void))])
(define-syntax/parse $c-definer #:literals (values)
  [(def (_ x:id c:expr) body:expr)
   #'(define/contract x c body)]
  [(def (_ x:id-or-bind/non-let c:expr) body:expr)
   (with-syntax ([blame-id (syntax->identifier #'x)])
     #'(x.definer x (with-contract blame-id #:result c body)))]
  [(def (_ x:bind/let-only (values c:expr ...)) body:expr)
   (with-syntax ([blame-id (syntax->identifier #'x)])
     #'(x.definer x (with-contract blame-id #:results (c ...) body)))])
(define-syntax/parse $c-letter #:literals (values)
  [(letter ([(_ x:id-or-bind/non-let c:expr) expr:expr]) body ...+)
   #'(let ([x.name (with-contract x #:result c expr)]) body ...)]
  [(letter ([(_ x:bind/let-only (values c:expr ...)) expr:expr]) body ...+)
   #'(let-values ([x.names (with-contract x #:results (c ...) expr)]) body ...)])
(define-for-syntax (syntax->identifier stx)
  (define sym (string->symbol (~s (syntax->datum stx))))
  (datum->syntax stx sym stx stx))


;; ----------------------------------------------------------------------------
;; ~define

(define-syntax/parse ~define #:stx stx
  [(_ b:bind body:expr)
   (syntax/loc stx 
     (b.definer b body))]
  [(_ x:id body:expr) (syntax/loc stx (define x body))]
  [(_ (f:expr . args:expr) body:expr ...+)
   (quasisyntax/loc stx
     (~define f
       #,(syntax/loc stx
           (~lambda args
             body ...))))])

(define-syntax/parse ~define/contract #:stx stx
  [(_ b:bind c:expr body:expr)
   (syntax/loc stx 
     (~define ($c b c) body))]
  [(_ x:id c:expr body:expr)
   (syntax/loc stx
     (define/contract x c body))]
  [(_ (f:expr . args:expr) c:expr body:expr ...+)
   (quasisyntax/loc stx
     (~define/contract f c
       #,(syntax/loc stx
           (~lambda args
             body ...))))])


;; ----------------------------------------------------------------------------
;; ~lambda

(begin-for-syntax
  ;; new-arg has to be list because keywords need to be spliced
  ;; def must be list to accomodate args not requiring extra defs
  (define-splicing-syntax-class fn-arg
    #:auto-nested-attributes
    ;; need this here to avoid conflicting with arg-with-default
    ;; actually, this needs to be first, if I want to allow
    ;; generic binding identifiers
    (pattern (~and arg :bind/non-let)
             #:attr tmp (generate-temporary)
             #:attr new-arg #'(tmp)
             #:attr def #`((definer arg tmp)))
    (pattern name:id 
             #:attr new-arg #'(name)
             #:attr def #'())
    (pattern [(~and arg :bind/non-let) default] 
             #:attr tmp (generate-temporary)
             #:attr new-arg #'([tmp default])
             #:attr def #'((definer arg tmp)))
    (pattern [name:id default] 
             #:attr new-arg #'([name default])
             #:attr def #'())
    (pattern (~seq kw:keyword (~and arg :bind/non-let))
             #:attr tmp (generate-temporary)
             #:attr new-arg #'(kw tmp)
             #:attr def #'((definer arg tmp)))
    (pattern (~seq kw:keyword name:id) 
             #:attr new-arg #'(kw name)
             #:attr def #'())
    (pattern (~seq kw:keyword [(~and arg :bind/non-let) default]) 
             #:attr tmp (generate-temporary)
             #:attr new-arg #'(kw [tmp default])
             #:attr def #'((definer arg tmp)))
    (pattern (~seq kw:keyword [name:id default]) 
             #:attr new-arg #'(kw [name default])
             #:attr def #'()))

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
      (pattern (~and arg :bind/non-let)
               #:attr new-arg #`(#,(generate-temporary))
               #:attr def #`((definer arg #,(car (syntax->list #'new-arg)))))
      (pattern name:id 
               #:attr new-arg #'(name)
               #:attr def #'())
      (pattern [name:id default] 
               #:attr new-arg #'([name default])
               #:attr def #'()))
  ) ; end define-for-syntax

(define-syntax/parse ~lambda #:stx stx
  [(_ b:bind/non-let body:expr ...)
   (syntax/loc stx (lambda (x) (b.definer b x) body ...))]
  [(_ rst:id body:expr ...) (syntax/loc stx (lambda rst body ...))]
  [(_ ?header:lam-function-header ?body ...)
   (template 
    (lambda ?header.new-header 
      (?@ . ?header.defs)
      ?body ...))])

(define-syntax/parse ~case-lambda
  [(_ [?header:case-lam-function-header ?body ...] ...)
   #:with (fn ...) (generate-temporaries #'(?header ...))
   #:with new-body 
   (let loop ([fns (syntax->list #'(fn ...))])
     (if (null? (cdr fns))
         #`(apply #,(car fns) args)
         #`(with-handlers ([exn:misc:match? (λ _ #,(loop (cdr fns)))])
             (apply #,(car fns) args))))
   #'(let ([fn (~lambda ?header ?body ...)] ...) (λ args new-body))])

(define-syntax/parse ~case-define #:datum-literals (→)
  [(_ f (x ... → body ...) ...)
   #'(define f (~case-lambda [(x ...) body ...] ...))]
  [(_ f clause ...) #'(define f (~case-lambda clause ...))])

;; ~let -----------------------------------------------------------------------
(define-syntax/parse ~let
  ;; only non-let generic binds are allowed in named let
  ;; (same as other fn def forms)
  [(_ loop:id ([x:id-or-bind/non-let e] ...) body ...)
   #`(let ()
       (~define (loop x ...) body ...)
       (loop e ...))]
  [(_ ([x:id-or-bind/let e] ...) body ...)
   #`(let-values ([x.names e] ...)
       (x.definer x (values . x.names)) ...
       body ...)])

(define-syntax/parse ~let*
  [(_ ([x:id-or-bind/let e]) body ...) 
   #`(x.letter ([x e]) 
               body ...)]
  [(_ ([x:id-or-bind/let e] rst ...) body ...)
   #`(x.letter ([x e])
               (~let* (rst ...) body ...))])

(define-syntax/parse ~letrec
  [(_ ([x:id-or-bind/let e] ...) body ...)
   #`(let ()
       (x.definer x e) ...
       body ...)])

;; ~for forms -----------------------------------------------------------------
(begin-for-syntax
  (define-splicing-syntax-class seq-binding (pattern (b:for-binder seq:expr)))
  (define-splicing-syntax-class for-clause 
    (pattern :seq-binding) (pattern :when-or-break))
  (define-syntax-class for-binder
    (pattern :bind/let-only #:with xs #'names)
    (pattern :bind #:with xs (list (generate-temporary)))
    (pattern x:id #:attr xs #'(x))
    (pattern (x:id ...) #:attr xs #'(x ...)))
  (define-splicing-syntax-class when-or-break 
    (pattern :when-clause) (pattern :break-or-final))
  (define-splicing-syntax-class when-clause
    #:attributes (test)
    (pattern (~seq #:when guard:expr) #:attr test #'guard)
    (pattern (~seq #:unless unless-guard:expr) #:attr test #'(not unless-guard)))
  (define-splicing-syntax-class break-clause (pattern (~seq #:break guard:expr)))
  (define-splicing-syntax-class final-clause (pattern (~seq #:final guard:expr)))
  (define-splicing-syntax-class break-or-final
    (pattern :break-clause) (pattern :final-clause))
  (define-splicing-syntax-class break/final-or-body 
    (pattern :break-or-final) (pattern body:expr))
  ) ; begin-for-syntax for ~for forms

(define-syntax/parse ~for/common ; essentially a foldl (ie uses an accum(s))
  ;; this clause allows naming of the accums so the body can reference them
  ;; -- used by for/fold and for/lists
  [(_ (~optional (~seq #:final final))
      combiner
      (~optional (~seq #:break? break?))
      ([accum base] ...)
      (c:for-clause ...) bb:break/final-or-body ...)
   #:with (body-res ...) (generate-temporaries #'(accum ...))
   #:with (b ...) (template ((?@ . bb) ...))
   #:with ((pre ...) (body ...)) (split-for-body #'(b ...) #'(b ...))
   #:with (pre-body:break/final-or-body ...) #'(pre ...)
   #:with expanded-for
   #`(let ([abort? #f] [accum base] ...)
       #,(let clauseloop ([cs #'(c ...)])
           (syntax-parse cs
             [() 
              #:with do-body
              ;; this must be a call-with-values because the number of results
              ;; is unpredictable (may be 0 values or multiple values)
              #`(call-with-values 
                 (lambda () body ...)
                 (lambda results (apply combiner accum ... results)))
              #`(let ()
                  ;; finalloop handles #:break and #:final in the body
                  #,(let finalloop ([pbs (syntax->list #'(pre-body ...))])
                      (if (null? pbs) #'do-body
                          (syntax-parse (car pbs)
                            [(#:break guard)
                             #`(if guard 
                                   (begin (set! abort? #t) (values accum ...))
                                   (let () #,(finalloop (cdr pbs))))]
                            [(#:final guard) 
                             #`(if guard 
                                   (begin (set! abort? #t) (let () #,(finalloop (cdr pbs))))
                                   (let () #,(finalloop (cdr pbs))))]
                            [(e:expr) #`(begin e #,(finalloop (cdr pbs)))]))))]
             [(([b:for-binder seq:expr]) ... (w:when-or-break) ... rst ...)
              (with-syntax* 
                  ([one-more-time (clauseloop #'(rst ...))]
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
                    (map (λ (x) (expand-for-clause x x)) (syntax->list #'([b.xs seq] ...)))]
                   [new-loop (generate-temporary)]
                   [its-done #'(values accum ...)]
                   [skip-it #`(if (and post-guard ...) 
                                  (if abort? 
                                      (values accum ...) 
                                      (new-loop accum ... loop-arg ... ...))
                                  its-done)]
                   [do-it 
                    #`(if (and post-guard ...) 
                          (let-values ([(accum ...) one-more-time])
                            (if abort?
                                (values accum ...)
                                (new-loop accum ... loop-arg ... ...)))
                          one-more-time)]
                   [conditional-body
                    (let whenloop ([ws (syntax->list #'(w ...))])
                      (if (null? ws)
                          #'do-it
                          (syntax-parse (car ws)
                            [(#:when guard) 
                             (if (eq? (syntax-e #'guard) #t)
                                 (whenloop (cdr ws))
                                 #`(if guard #,(whenloop (cdr ws)) skip-it))]
                            [(#:unless guard) #`(if guard skip-it #,(whenloop (cdr ws)))]
                            [(#:break guard) 
                             #`(if guard 
                                   (begin (set! abort? #t) its-done)
                                   #,(whenloop (cdr ws)))]
                            [(#:final guard) 
                             #`(if guard 
                                   (begin (set! abort? #t) #,(whenloop (cdr ws)))
                                   #,(whenloop (cdr ws)))])))])
                #`(let-values (outer-binding ... ...)
                    ;; must shadow accum in new loop, in case body references it
                    (let new-loop ([accum accum] ... loop-binding ... ...)
                      ;; must check break? first, bc more? pulls an item
                      (if (and #,@(if (attribute break?) #'((not (break? accum ...))) #'())
                               pos-guard ...)
                          (let-values ([xs next] ... ...)
                            (if (and pre-guard ...)
                                (~let ([b (values ys ...)] ...) conditional-body)
                                its-done))
                          its-done))))])))
   (if (attribute final)
       #'(let-values ([(accum ...) expanded-for]) (final accum ...))
       #'expanded-for)]
  ;; this clause has unnamed accums; name them and then call the first clause
  [(_ (~optional (~seq #:final final))
      combiner
      (~optional (~seq #:break? break?))
      (base ...) 
      (c:for-clause ...) bb:break/final-or-body ...)
   #:with (accum ...) (generate-temporaries #'(base ...))
   #`(~for/common #,@(if (attribute final) #'(#:final final) #'())
                  combiner 
                  #,@(if (attribute break?) #'(#:break? break?) #'())
                  ([accum base] ...) 
                  #,@(template (((?@ . c) ...) (?@ . bb) ...)))])

;; inserts #:when #t between each (non-#:when/unless) clause
(define-syntax/parse ~for*/common
  [(_ (~optional (~seq #:final fin))
      comb
      (~optional (~seq #:break? b?))
      (base ...) 
      ((~and (~seq sb:seq-binding ... wb:when-or-break ...) (~seq _ ...+)) ...) body ...)
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
                  (base ...) (new-clause ...) body ...)])

(define-syntax/parse mk~for/
  [(_ name combiner (base ...)
      (~optional (~seq #:final fin))
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
                       (base ...) x (... ...))))])

(define-syntax-rule (~for x ...) (~for/common void ((void)) x ...))
(define-syntax-rule (~for* x ...) (~for*/common void ((void)) x ...))

;; ~for/list ~for*/list and ~for/lists probably need a "right folding" for/common
;;   but for now, just reverse the output
;; - have to reverse args in cons bc acc is first
(mk~for/ list (λ (acc y) (unsafe-cons-list y acc)) (null) #:final reverse)
;; break as soon as we find something
(mk~for/ first (λ (acc y) y) (#f) #:break? identity)
(mk~for/ last (λ (acc y) y) (#f))
(mk~for/ and (λ (acc y) (and acc y)) (#t) #:break? not)
(mk~for/ or (λ (acc y) (or acc y)) (#f) #:break? identity)
(mk~for/ sum + (0))
(mk~for/ product * (1))
(mk~for/ hash hash-set ((hash)))
(mk~for/ hasheq hash-set ((hasheq)))
(mk~for/ hasheqv hash-set ((hasheqv)))

(define-syntax/parse ~for/fold ; foldl
  [(_ ([accum init] ...) (c:for-clause ...) bb:break/final-or-body ...)
   #:with (res ...) (generate-temporaries #'(accum ...))
   ;; combiner drops old accums and uses result(s) of body as current accums
   (template (~for/common
              #:final values (λ (accum ... res ...) (values res ...))
              ([accum init] ...) ((?@ . c) ...) (?@ . bb) ...))])
(define-syntax/parse ~for*/fold
  [(_ ([accum init] ...) (c:for-clause ...) bb:break/final-or-body ...)
   #:with (res ...) (generate-temporaries #'(accum ...))
   ;; combiner drops old accums and uses result(s) of body as current accums
   (template (~for*/common 
              #:final values (λ (accum ... res ...) (values res ...))
              ([accum init] ...) ((?@ . c) ...) (?@ . bb) ...))])

(define-syntax/parse ~for/lists
  [(_ (accum ...) (c:for-clause ...) bb:break/final-or-body ...)
   #:with (res ...) (generate-temporaries #'(accum ...))
   ;; combiner drops old accums and uses result(s) of body as current accums
   (template (~for/common 
              #:final (λ (accum ...) (values (reverse accum) ...))
              (λ (accum ... res ...) (values (unsafe-cons-list res accum) ...))
              ([accum null] ...) ((?@ . c) ...) (?@ . bb) ...))])

(define-syntax/parse ~for*/lists
  [(_ (accum ...) (c:for-clause ...) bb:break/final-or-body ...)
   #:with (res ...) (generate-temporaries #'(accum ...))
   ;; combiner drops old accums and uses result(s) of body as current accums
   (template (~for*/common 
              #:final (λ (accum ...) (values (reverse accum) ...))
              (λ (accum ... res ...) (values (unsafe-cons-list res accum) ...))
              ([accum null] ...) ((?@ . c) ...) (?@ . bb) ...))])

;; ~for/vector is an imperative mess
(define-syntax/parse ~for/vector
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
          x ...))])
(define-syntax/parse ~for*/vector
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
          x ...))])


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
