#lang racket

(require (for-syntax syntax/parse))

(provide define-syntax/parse)

(define-syntax define-syntax/parse
  (syntax-parser
    [(_ name:id #:stx stx-arg:id option-or-clause ...)
     #'(define-syntax (name stx-arg)
         (syntax-parse stx-arg option-or-clause ...))]
    [(_ name:id option-or-clause ...)
     #'(define-syntax name
         (syntax-parser option-or-clause ...))]))

