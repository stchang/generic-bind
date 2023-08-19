#lang racket
(require syntax/parse
         syntax/parse/define
         racket/unsafe/ops
         "syntax-parse-utils.rkt"
         "nested-binds-helper.rkt"
         "version-utils.rkt"
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
         ~for ~for/list ~for/vector ~for/fold ~for/foldr ~for/lists
         ~for/first ~for/last
         ~for/and ~for/or ~for/sum ~for/product 
         ~for/hash ~for/hasheq ~for/hasheqv
         ~for* ~for*/list ~for*/fold ~for*/foldr ~for*/vector ~for*/lists 
         ~for*/first ~for*/last ~for*/and ~for*/or ~for*/sum ~for*/product  
         ~for*/hash ~for*/hasheq ~for*/hasheqv
         define-match-bind ~struct ~define-struct/contract
         (if-struct/contract-available-out ~struct/contract))

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

(begin-for-syntax ;; ~struct/contract syntax classes
  (define-syntax-class struct/contract-field
    (pattern [field:struct-field contract:expr] #:attr name #'field.name))
  ) ; end begin-for-syntax
(do-if-struct/contract-available
 (define-syntax/parse ~struct/contract
   [(_ id:id super:id ... (field:struct/contract-field ...) opt ...)
    #'(begin
        (struct/contract id super ... (field ...) opt ...)
        (define-match-bind (id field.name ...)))]))
(define-syntax/parse ~define-struct/contract
   [(_ {~and head {~or id:id (id:id . _)}} (field:struct/contract-field ...) opt ...)
    #'(begin
        (define-struct/contract head (field ...) opt ...)
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
    #:attributes (b xs)
    (pattern :bind/let-only #:with xs #'names)
    (pattern :bind #:with xs (list (generate-temporary)))
    (pattern x:id #:with b #'x #:with xs (generate-temporaries #'(x)))
    (pattern (x:id ...) #:with b #'(~vs x ...) #:with xs (generate-temporaries #'(x ...))))
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

  (define-syntax-class for-clauses
    #:attributes (do)
    (pattern (c:for-clause ...)
      #:with do (for-clauses-do #'(c ...))))
  (define-syntax-class for*-clauses
    #:attributes (do)
    (pattern (c:for-clause ...)
      #:with do
      (append-map (λ (c) (syntax->list (for-clauses-do (list c))))
                  (attribute c))))
  (define (for-clauses-do cs)
    (syntax-parse cs
      [() #'()]
      [(([b:for-binder seq:expr]) ... (w:when-or-break) ... rst ...)
       #:with ((y ...) ...) #'(b.xs ...)
       #`([(y ...) seq] ... #:do [(~define b.b (values y ...)) ...] (~@ . w) ...
          #,@(for-clauses-do #'(rst ...)))]))
  ) ; begin-for-syntax for ~for forms

(define-syntax/parse mk~for
  [(_ name
      (~optional hpat-before-clauses #:defaults ([hpat-before-clauses #'(~seq)])))
   #:with old-name (format-id #'name "for~a" #'name)
   #:with old-name* (format-id #'name "for*~a" #'name)
   #:with new-name (format-id #'name "~~for~a" #'name)
   #:with new-name* (format-id #'name "~~for*~a" #'name)
   #`(begin
       (define-syntax-parse-rule
         (new-name (~and (~seq hpat-before-clauses) (~seq bcs (... ...)))
                   cs:for-clauses
                   x
                   (... ...))
         (old-name bcs (... ...) cs.do x (... ...)))
       (define-syntax-parse-rule
         (new-name* (~and (~seq hpat-before-clauses) (~seq bcs (... ...)))
                    cs:for*-clauses
                    x
                    (... ...))
         (old-name* bcs (... ...) cs.do x (... ...))))])

(mk~for ||)
(mk~for /list)
(mk~for /first)
(mk~for /last)
(mk~for /and)
(mk~for /or)
(mk~for /sum)
(mk~for /product)
(mk~for /hash)
(mk~for /hasheq)
(mk~for /hasheqv)

(mk~for /fold accums)

(mk~for /foldr accums)

(mk~for /lists accums)

(mk~for /vector
        (~optional (~seq (~seq #:length len) (~optional (~seq #:fill fill)))))
