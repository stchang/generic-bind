#lang racket

(require (for-syntax racket/syntax 
                     syntax/parse
                     racket)) ; for append-map

(provide (rename-out [:=define-values define-values]
                     [:=let-values let-values]
                     [:=define define]
                     [:=for for])
         :=m :=v)

(define-for-syntax (bind-prop stx)
  (syntax-property (local-expand stx 'expression null) 'bind))
(define-for-syntax (has-bind-prop? stx)
  (with-handlers ([exn:fail? (λ _ #f)]) (bind-prop stx)))
;    (syntax-property (local-expand stx 'expression null) 'bind)))
(define-for-syntax (let-only-prop? stx)
  (with-handlers ([exn:fail? (λ _ #f)])
    (syntax-property (local-expand stx 'expression null) 'let-only)))
(define-for-syntax (get-nested-defs stx)
  (syntax-property (local-expand stx 'expression null) 'nested-defs))

(define-syntax (:=define-values stx)
  (syntax-parse stx
    [(_ (b:id ...) body) #'(define-values (b ...) body)]
    [(_ (b ...) body)
     (andmap has-bind-prop? (syntax->list #'(b ...)))
;     (let ([m (printf "~a\n" (map (λ (s) (syntax-property (local-expand s 'expression null) 'bind)) (syntax->list #'(b ...))))])
     (with-syntax*
      ([((m v) ...) 
        (map (λ (s) 
               (syntax-property 
                (local-expand s 'expression null)
                'bind))
             (syntax->list #'(b ...)))]
        [(w ...) (datum->syntax stx (syntax->datum #'(v ...)))]
        [(z ...) (generate-temporaries #'(b ...))])
     #'(begin
         (define-values (z ...) body)
         (m w z) ...))]))

(define-syntax (:=let-values stx)
  (syntax-case stx ()
    [(_ ([(b ...) e]) body ...)
     (with-syntax*
      ([((m v) ...) 
        (map (λ (s)
               (syntax-property
                (local-expand s 'expression null)
                'bind))
             (syntax->list #'(b ...)))]
        [(w ...) (datum->syntax stx (syntax->datum #'(v ...)))]
        [(z ...) (generate-temporaries #'(b ...))])
      #'(let-values ([(z ...) e])
          (m w z) ...
          body ...))]))
    

(define-syntax (:=define stx)
  (syntax-parse stx
    [(_ x:id body) #'(define x body)]
    [(_ x body)
     #:when (has-bind-prop? #'x)
     (with-syntax* ([(def ids) (bind-prop #'x)]
                    [new-ids (datum->syntax stx (syntax->datum #'ids))])
     #'(begin
         (def new-ids body)))]
    [(_ (f x ...) body ...) #'(define (f x ...) body ...)]))

(define-syntax (:=for stx)
  (syntax-parse stx
    [(_ ((~and (x:id seq) clause) ...) body ...) 
     #'(for (clause ...) body ...)]
    [(_ ((x seq) ...) body ...)
     #:when (andmap has-bind-prop? #'(x ...))
     #:with ((def ids) ...) (map bind-prop (syntax->list #'(x ...)))
     #:with (new-ids ...) (datum->syntax stx (syntax->datum #'(ids ...)))
     #'for]))



(require (for-syntax syntax/parse/experimental/template))


;; syntax classes for `define/match`
(begin-for-syntax
  (define-syntax-class function-header
    (pattern ((~or header:function-header name:id) . args:fn-args)
;             #:attr params
;             (template ((?@ . (?? header.params ()))
;                        . args.params))
             #:attr new-header
             (template ((?? header.new-header name)
                        . args.new-args))
             #:attr defs #'args.defs
             ))

  (define-syntax-class fn-args
    (pattern (arg:fn-arg ...)
;             #:attr params #'(arg.name ...)
;             #:attr new-args #'(arg.new-arg ...))
             #:attr new-args (template ((?@ . arg.new-arg) ...))
             #:attr defs #'(arg.def ...))
    (pattern (arg:fn-arg ... . rest:id)
;             #:attr params #'(arg.name ... rest)
             #:attr new-args #'((?@ . arg.new-arg) ... rest)
             #:attr defs #'(arg.def ...)))

  (define-syntax-class gen-bind
    (pattern b #:when (has-bind-prop? #'b)
               #:with e (local-expand #'b 'expression null)
               #:with (df ids) (bind-prop-expanded #'e)
               #:with new-e (generate-temporary)
               #:attr definer #'df
               #:attr ids-to-bind (datum->syntax #'b (syntax->datum #'ids))
               #:attr name #'new-e
               #:attr new-arg #'(name)
               #:attr def #`(df #,(datum->syntax #'b (syntax->datum #'ids)) name)
               #:attr nested-defs (get-nested-defs-expanded #'e)))
  
  (define-syntax-class gen-bind-no-let
    (pattern e:gen-bind 
             #:fail-when (let-only-prop? #'e)
                         (format "can't use ~a pattern in non-let-style binding position"
                                 (syntax->datum #'e))
             #:attr name #'e.name
             #:attr new-arg #'e.new-arg
             #:attr def #'e.def
             #:attr nested-defs #'e.nested-defs))
    
  
  (define-splicing-syntax-class fn-arg
;    #:attributes (name)
    (pattern name:id 
             #:attr new-arg #'(name)
             #:attr def #'(void))
    ;; need this here to avoid conflicting with arg-with-default
    (pattern e:gen-bind-no-let
             #:attr name #'e.name
             #:attr new-arg #'e.new-arg
             #:attr def #'e.def)
    #;(pattern e #:fail-when (let-only-prop? #'e) 
                           (format "can't use ~a pattern in non-let-style binding position"
                                   (syntax->datum #'e))
               #:when (has-bind-prop? #'e) 
               #:with (df ids) (bind-prop #'e)
               #:attr name (generate-temporary) 
               #:attr new-arg #'(name)
               #:attr def #`(df #,(datum->syntax #'e (syntax->datum #'ids)) name))
    (pattern [name:id default] 
             #:attr new-arg #'([name default])
             #:attr def #'(void))
    (pattern (~seq kw:keyword name:id) 
             #:attr new-arg #'(kw name)
             #:attr def #'(void))
    (pattern (~seq kw:keyword [name:id default]) 
             #:attr new-arg #'(kw (name default))
             #:attr def #'(void))
    ))


(define-syntax (:=m stx)
  (syntax-case stx ()
    [(_ pat) 
     (syntax-property
      (syntax-property 
       #'(void)
       'bind (list #'match-define #'pat))
      'nested-defs #'())]))

(define-for-syntax (bind-prop-expanded stx)
  (syntax-property stx 'bind))
(define-for-syntax (has-bind-prop-expanded? stx)
  (with-handlers ([exn:fail? (λ _ #f)]) (bind-prop-expanded stx)))
;    (syntax-property (local-expand stx 'expression null) 'bind)))
(define-for-syntax (let-only-prop-expanded? stx)
  (with-handlers ([exn:fail? (λ _ #f)]) (syntax-property 'let-only)))
(define-for-syntax (get-nested-defs-expanded stx)
  (syntax-property stx 'nested-defs))

(begin-for-syntax
;  (define-syntax-class gen-bind-expanded
;    (pattern e #:when (has-bind-prop-expanded? #'e)
;               #:with (df ids) (bind-prop-expanded #'e)
;               #:with new-e (generate-temporary)
;               #:attr definer #'df
;               #:attr ids-to-bind (datum->syntax #'e (syntax->datum #'ids))
;               #:attr name #'new-e
;               #:attr new-arg #'(name)
;               #:attr def #`(df #,(datum->syntax #'e (syntax->datum #'ids)) name)))
;  
;  (define-syntax-class gen-bind-no-let-expanded
;    (pattern e:gen-bind-expanded
;             #:fail-when (let-only-prop-expanded? #'e)
;                         (format "can't use ~a pattern in non-let-style binding position"
;                                 (syntax->datum #'e))
;             #:attr name #'e.name
;             #:attr new-arg #'e.new-arg
;             #:attr def #'e.def))
    
  (define-syntax-class non-let-bind
;    #:attributes (name)
    (pattern name:id 
             #:attr new-arg #'(name)
             #:attr def #'(void))
    ;; need this here to avoid conflicting with arg-with-default
    (pattern e:gen-bind-no-let
             #:attr name #'e.name
             #:attr new-arg #'e.new-arg
             #:attr def #'e.def))
  #;(define-syntax-class let-bind
;    #:attributes (name)
    (pattern name:id 
             #:attr new-arg #'(name)
             #:attr def #'(void))
    ;; need this here to avoid conflicting with arg-with-default
    (pattern e:gen-bind-no-let
             #:attr name #'e.name
             #:attr new-arg #'e.new-arg
             #:attr def #'e.def)))

(provide new-define)
(define-syntax (new-define stx)
  (syntax-parse stx
    [(_ x:id body) #'(define x body)]
    [(_ b:gen-bind body)
;     (printf "~a\n"
     (datum->syntax stx
     (syntax->datum
     #`(begin
        (b.definer b.ids-to-bind body)
        #,@#'b.nested-defs)))]
;#'(b.definer b.ids-to-bind body)]
;    [(_ ?header:function-header ?clause ...)
    [(_ ?header:function-header ?body ...)
     ;(printf "~a\n" #'?header)
     (template
;      (define ?header body ...))]))
      (define ?header.new-header 
        (?@ . ?header.defs)
        ?body ...))]))
;        (match* (?? ?header.params)
;                ?clause ...)))]))
;(define-syntax (new-let stx)
;  (syntax-parse stx
;    [(_ (~optional loop:id) ([b:let-bind e] ...) body ...)
;     #'(let #,@(if (attribute loop) #'(loop) #'())
;         ([b.name e] ...) body ...)]))

(define-syntax (:=v stx)
  (syntax-parse stx
    [(_ x:non-let-bind ...)
     (syntax-property
      (syntax-property
       (syntax-property
        #'(void)
        'bind (list #'define-values #'(x.name ...)))
       'let-only #t)
      'nested-defs #'(x.def ...) #;(append (syntax->list #'(x.def ...))
                                           (append-map get-nested-defs (syntax->list #'(x ...)))))]))
