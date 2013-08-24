#lang racket
;(require (for-syntax syntax/parse))
(provide (all-defined-out))

(define (add-syntax-properties props body)
  (if (null? props) body
      (add-syntax-properties 
       (cdr props)
       (apply syntax-property body (car props)))))
