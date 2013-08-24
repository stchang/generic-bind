#lang racket
(require (for-syntax syntax/parse
                     racket/syntax))
(require (for-syntax syntax/parse/experimental/template))
(require (for-syntax "stx-utils.rkt"))


(provide ~m ~v ~define ~lambda (rename-out [~lambda ~λ]))

;; (define-generic-stx bind (definer ids let-only))

(begin-for-syntax
  
  ;; need this otherwise will try to expand the header of function defines,
  ;; resulting in unbound variable error
  (define (try-local-expand stx) 
    (with-handlers ([exn:fail:syntax:unbound? (λ _ #f)] ;; unbound var
                    [exn:fail:syntax? (λ _ #f)]) ;; ie keyword as datum
      (local-expand stx 'expression null)))
  (define (bind-definer stx) (syntax-property stx 'definer))
  (define (bind-ids stx) (syntax-property stx 'ids))
  (define (bind-nested-definers stx) (syntax-property stx 'nested-definers))
  (define (bind-nested-idss stx) (syntax-property stx 'nested-idss))
  (define (bind? stx) (and (bind-definer stx) (bind-ids stx)))
  (define (bind-let-only? stx) (syntax-property stx 'let-only))
  
  (define-syntax-class bind
    #:description "a generic bind instance"
    (pattern b #:with expanded-b (try-local-expand #'b)
               #:when (bind? #'expanded-b)
               #:attr definer (bind-definer #'expanded-b)
               #:attr ids (datum->syntax #'b (bind-ids #'expanded-b))
;               #:attr nested-definers (bind-nested-definers #'expanded-b)
;               #:attr nested-idss (datum->syntax #'b (bind-nested-idss #'expanded-b))))
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
  (define-syntax-class bind/non-let
    #:description "a generic bind instance that supports non-let contexts"
    #:auto-nested-attributes
    (pattern :bind #:fail-when 
                   (bind-let-only? #'expanded-b)
                   (format "can't use ~a pattern in non-let binding context"
                           (syntax->datum #'b))))
                   
  #;(define-syntax-class bind/non-let
    #:description "a generic bind instance that supports non-let contexts"
    (pattern b #:with expanded-b (local-expand #'b 'expression null)
               #:fail-when 
               (bind-let-only? #'expanded-b)
               (format "can't use ~a pattern in non-let binding context"
                       (syntax->datum #'b))            
               #:when (bind? #'expanded-b)
               #:attr definer (bind-definer #'expanded-b)
               #:attr ids (datum->syntax #'b (bind-ids #'expanded-b))
               #:attr name (generate-temporary)
;               #:attr nested-definers (bind-nested-definers #'expanded-b)
;               #:attr nested-idss (datum->syntax #'b (bind-nested-idss #'expanded-b))
               #:attr nested-defs 
               (datum->syntax #'b
                 (for/list ([nested-definer (syntax->list (bind-nested-definers #'expanded-b))]
                            [nested-ids (bind-nested-idss #'expanded-b)]
                            [id (syntax->list #'ids)])
                   (list nested-definer (datum->syntax #'b nested-ids) id)))))
;                 (for/list ([nested-definer (syntax->list #'nested-definers)]
;                            [nested-ids (syntax->list #'nested-idss)]
;                            [id (syntax->list #'ids)])
;                   (list nested-definer nested-ids id)))))
  
  ) ;; end begin-for-syntax



;; match generic bind instance 
;; TODO: currently does not support nested generic binds
(define-syntax (~m stx)
  (syntax-parse stx
    [(_ pat) (add-syntax-properties
              `([definer ,#'match-define]
                ;; need to capture ids in outer ctx so just store datums
                [ids ,(syntax->datum #'pat)]
                [let-only #f]
                [nested-definers ,#'()]
                [nested-idss ,null])
              #'(void))]))

(begin-for-syntax
  (define-syntax-class v-bind
    #:auto-nested-attributes
    (pattern x:id #:attr name (generate-temporary)
                  #:attr definer #'define
                  #:attr ids #'x)
    (pattern :bind/non-let #:attr name (generate-temporary))))

;; values generic bind instance
;; - supports (one-level only) nested (non-let-restricted) generic binds
;;   (to support artitrary nesting, need to get the nested-defs of each x:v-bind)
(define-syntax (~v stx)
  (syntax-parse stx
    [(_ x:v-bind ...)
    (add-syntax-properties
      `([definer ,#'define-values] 
        ;; need to capture ids in outer ctx so just store datums
        [ids ,(syntax->datum #'(x.name ...))]
        [let-only #t]
        [nested-definers ,#'(x.definer ...)]
        ;; need to capture ids in outer ctx so just store datums
        [nested-idss ,(syntax->datum #'(x.ids ...))])
      #'(void))]))


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
;
;  (define-syntax-class gen-bind
;    (pattern b #:when (has-bind-prop? #'b)
;               #:with e (local-expand #'b 'expression null)
;               #:with (df ids) (bind-prop-expanded #'e)
;               #:with new-e (generate-temporary)
;               #:attr definer #'df
;               #:attr ids-to-bind (datum->syntax #'b (syntax->datum #'ids))
;               #:attr name #'new-e
;               #:attr new-arg #'(name)
;               #:attr def #`(df #,(datum->syntax #'b (syntax->datum #'ids)) name)
;               #:attr nested-defs (get-nested-defs-expanded #'e)))
;  
;  (define-syntax-class gen-bind-no-let
;    (pattern e:gen-bind 
;             #:fail-when (let-only-prop? #'e)
;                         (format "can't use ~a pattern in non-let-style binding position"
;                                 (syntax->datum #'e))
;             #:attr name #'e.name
;             #:attr new-arg #'e.new-arg
;             #:attr def #'e.def
;             #:attr nested-defs #'e.nested-defs))
    
  ;; new-arg has to be list because keywords need to be spliced
  ;; def must be list to accomodate args not requiring extra defs
  (define-splicing-syntax-class fn-arg
    #:auto-nested-attributes
    (pattern name:id 
             #:attr new-arg #'(name)
             #:attr def #'())
    ;; need this here to avoid conflicting with arg-with-default
    (pattern :bind/non-let
;             #:attr name #'e.name
             #:attr new-arg #`(#,(generate-temporary))
             #:attr def #`((definer ids #,(car (syntax->list #'new-arg)))))
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
             #:attr def #'())
    (pattern (~seq kw:keyword name:id) 
             #:attr new-arg #'(kw name)
             #:attr def #'())
    (pattern (~seq kw:keyword [name:id default]) 
             #:attr new-arg #'(kw [name default])
             #:attr def #'()))
    ) ;; end begin-for-syntax

(define-syntax (~define stx)
  (syntax-parse stx
    [(_ x:id body:expr) (syntax/loc stx (define x body))]
    [(_ b:bind body:expr)
     (quasisyntax/loc stx 
       (begin (b.definer b.ids body)
              #,@#'b.nested-defs))]
    #;[(_ (f x ...) body ...) #'(define (f x ...) body ...)]
    [(_ ?header:def-function-header ?body ...)
     (template (define ?header.new-header 
                 (?@ . ?header.defs)
                 ?body ...))]))
         

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
  ) ; end define-for-syntax

(define-syntax (~lambda stx)
  (syntax-parse stx
    [(_ rst:id body:expr ...) (syntax/loc stx (lambda rst body ...))]
    [(_ b:bind/non-let body:expr ...)
     #:with x (generate-temporary)
     (syntax/loc stx (lambda (x) (b.definer b.ids x) body ...))]
    [(_ ?header:lam-function-header ?body ...)
     (template (lambda ?header.new-header 
                 (?@ . ?header.defs)
                 ?body ...))]))

