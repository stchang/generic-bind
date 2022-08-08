#lang racket/base

(provide do-if-struct/contract-available
         do-if-syntax-local-match-introduce-available
         if-struct/contract-available-out)

(require racket/provide-syntax
         (for-syntax racket/base
                     "stx-utils.rkt"))

(define-syntax do-if-struct/contract-available
  (lambda (stx)
    (if struct/contract-available?
        (syntax-case stx ()
          [(_ stuff ...) #'(begin stuff ...)])
        #'(begin))))

(define-syntax do-if-syntax-local-match-introduce-available
  (lambda (stx)
    (if syntax-local-match-introduce-available?
        (syntax-case stx ()
          [(_ stuff ...) #'(begin stuff ...)])
        #'(begin))))

(define-provide-syntax if-struct/contract-available-out
  (lambda (stx)
    (if struct/contract-available?
        (syntax-case stx ()
          [(_ stuff ...) #'(combine-out stuff ...)])
        #'(combine-out))))
