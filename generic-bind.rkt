#lang racket

(require (for-syntax racket/syntax syntax/parse))

(provide (rename-out [:=define-values define-values]
                     [:=let-values let-values]
                     [:=define define]
                     [:=for for])
         :=m :=v)

(define-for-syntax (has-bind-prop? stx)
  (with-handlers ([exn:fail? (λ _ #f)])
    (syntax-property (local-expand stx 'expression null) 'bind)))
(define-for-syntax (let-only-prop? stx)
  (with-handlers ([exn:fail? (λ _ #f)])
    (syntax-property (local-expand stx 'expression null) 'let-only)))
(define-for-syntax bind-prop has-bind-prop?)

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

(define-syntax (:=m stx)
  (syntax-case stx ()
    [(_ x ...) (syntax-property 
                #'null
                'bind (list #'match-define #'(x ...)))]))


(define-syntax (:=v stx)
  (syntax-case stx ()
    [(_ x ...) (syntax-property
                (syntax-property
                 #'null
                 'bind (list #'define-values #'(x ...)))
                'let-only #t)]))



(require (for-syntax syntax/parse/experimental/template))


;; syntax classes for `define/match`
(begin-for-syntax
  (define-syntax-class function-header
    (pattern ((~or header:function-header name:id) . args:args)
;             #:attr params
;             (template ((?@ . (?? header.params ()))
;                        . args.params))
             #:attr new-header
             (template ((?? header.new-header name)
                        . args.new-args))
             #:attr defs #'args.defs
             ))

  (define-syntax-class args
    (pattern (arg:arg ...)
;             #:attr params #'(arg.name ...)
;             #:attr new-args #'(arg.new-arg ...))
             #:attr new-args (template ((?@ . arg.new-arg) ...))
             #:attr defs #'(arg.def ...))
    (pattern (arg:arg ... . rest:id)
;             #:attr params #'(arg.name ... rest)
             #:attr new-args #'(arg.new-arg ... rest)
             #:attr defs #'(arg.def ...)))

  (define-splicing-syntax-class arg
;    #:attributes (name)
    (pattern name:id 
             #:attr new-arg #'(name)
             #:attr def #'(void))
    (pattern [name:id default] 
             #:attr new-arg #'([name default])
             #:attr def #'(void))
    (pattern (~seq kw:keyword name:id) 
             #:attr new-arg #'(kw name)
             #:attr def #'(void))
    (pattern (~seq kw:keyword [name:id default]) 
             #:attr new-arg #'(kw (name default))
             #:attr def #'(void))
    (pattern e #:fail-when (let-only-prop? #'e) 
                           (format "can't use ~a pattern in non-let-style binding position"
                                   (syntax->datum #'e))
               #:when (has-bind-prop? #'e) 
               #:with (df ids) (bind-prop #'e)
               #:attr name (generate-temporary) 
               #:attr new-arg #'(name)
               #:attr def #`(df #,(datum->syntax #'e (syntax->datum #'ids)) name))
    ))

(provide new-define)
(define-syntax (new-define stx)
  (syntax-parse stx
;    [(_ ?header:function-header ?clause ...)
    [(_ ?header:function-header ?body ...)
     (template
;      (define ?header body ...))]))
      (define ?header.new-header 
        (?@ . ?header.defs)
        ?body ...))]))
;        (match* (?? ?header.params)
;                ?clause ...)))]))
