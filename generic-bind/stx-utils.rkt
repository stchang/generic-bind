#lang racket

(provide (all-defined-out))

(define (add-syntax-properties props body)
  (if (null? props) body
      (add-syntax-properties 
       (cdr props)
       (apply syntax-property body (car props)))))

(define add-bind-properties
  (case-lambda
    [(definer-prop letter-prop body)
     (add-syntax-properties
      `([definer ,definer-prop]
        [letter ,letter-prop])
      body)]
    [(definer-prop letter-prop let-only-prop names-prop body)
     (add-syntax-properties
      `([definer ,definer-prop]
        [letter ,letter-prop]
        [let-only ,let-only-prop]
        [names ,names-prop])
      body)]))