#lang racket/base

(provide with-new-match-pat-def-set
         splicing-with-new-match-pat-def-set
         define-current-match-pat-def-set
         (for-syntax make-gen-bind-match-expander-proc-maker
                     ))

(require racket/stxparam
         racket/splicing
         "syntax-parse-utils.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/set
                     "stx-utils.rkt"
                     ))

(define-syntax-parameter current-match-pat-def-set #f)

(begin-for-syntax
  (define (get-current-match-pat-def-set)
    (syntax-parameter-value #'current-match-pat-def-set))

  (define (nested-gen-bind-not-supported-error stx)
    (raise-syntax-error
     #f
     (string-append
      "nested generic-binding instances not supported for Racket version "(version)"\n"
      "  because syntax-local-match-introduce is needed")
     stx))
  
  (define ((make-gen-bind-match-expander-proc-maker ~define-id))
    ;; hash : (Hash-Table Any Symbol)
    (define hash (make-hash))
    (lambda (stx)
      (define def-set (get-current-match-pat-def-set))
      (cond [(set-mutable? def-set)
             (unless syntax-local-match-introduce-available?
               (nested-gen-bind-not-supported-error stx))
             (define/syntax-parse name
               (datum->stx stx (hash-ref! hash (syntax->datum+srcloc+props stx)
                                 gensym)))
             (define/syntax-parse -define ~define-id)
             (define/syntax-parse stx* stx)
             (define def
               (syntax-local-match-introduce-2
                (syntax/loc stx (-define stx* name))))
             (set-add! def-set def)
             #'name]
            [else
             (raise-syntax-error #f "not within a generic binding context" stx)]))))

(define-syntax/parse with-new-match-pat-def-set #:stx stx
  [(wnmpds expr:expr ...+)
   (syntax/loc stx
     (syntax-parameterize ([current-match-pat-def-set (mutable-set)])
       expr ...))])

(define-syntax/parse splicing-with-new-match-pat-def-set #:stx stx
  [(swnmpds expr:expr ...+)
   (syntax/loc stx
     (splicing-syntax-parameterize ([current-match-pat-def-set (mutable-set)])
       expr ...))])

(define-syntax define-current-match-pat-def-set
  (lambda (stx)
    (define def-set (syntax-parameter-value #'current-match-pat-def-set))
    (cond [(set-mutable? def-set)
           (define/syntax-parse (def ...) (map syntax-local-introduce (set->list def-set)))
           (begin0 #'(begin def ...)
                   (set-clear! def-set))]
          [else
           (raise-syntax-error #f "no current-match-pat-def-set" stx)])))

