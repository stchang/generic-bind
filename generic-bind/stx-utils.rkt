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

(define (syntax-srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))

(define (syntax-props stx)
  (for/hash ([key (in-list (syntax-property-symbol-keys stx))])
    (values key (syntax-property stx key))))

(define (syntax->datum+srcloc+props stx)
  (let ([stx.e  (syntax-e stx)]
        [srcloc (syntax-srcloc stx)]
        [keys   (syntax-property-symbol-keys stx)])
    (list (stx->datum+srcloc+props stx.e)
          (syntax-srcloc stx)
          (syntax-props stx))))

(define (stx->datum+srcloc+props x)
  (match x
    [(? syntax? stx) (syntax->datum+srcloc+props stx)]
    [(? symbol? sym) sym]
    [(? number? n) n]
    [(? boolean? b) b]
    [(? char? c) c]
    [(? string? str) str]
    [(? bytes? bs) bs]
    [(? regexp? rx) rx]
    [(box val) (box-immutable (stx->datum+srcloc+props val))]
    ['() '()]
    [(cons x.car x.cdr) (cons (stx->datum+srcloc+props x.car)
                              (stx->datum+srcloc+props x.cdr))]
    [(vector lst ...) (apply vector-immutable (map stx->datum+srcloc+props lst))]
    [(? hash? hash)
     (for/hash ([(key val) (in-hash hash)])
       (values (stx->datum+srcloc+props key)
               (stx->datum+srcloc+props val)))]
    [(app prefab-struct-key (and key (not #f)))
     (apply make-prefab-struct key
            (map stx->datum+srcloc+props (rest (vector->list (struct->vector x)))))]
    [_ x]))

(define (datum->stx stx datum)
  (datum->syntax stx datum stx stx))

(define (syntax-local-match-introduce-fallback stx)
  (error 'syntax-local-match-introduce
         "not available in Racket version ~a"
         (version)))

(define syntax-local-match-introduce-2
  (with-handlers ([exn:fail:filesystem:missing-module? (λ (e) syntax-local-match-introduce-fallback)])
    (dynamic-require
     'racket/match/syntax-local-match-introduce
     'syntax-local-match-introduce
     (λ () syntax-local-match-introduce-fallback))))

(define syntax-local-match-introduce-available?
  (not (eq? syntax-local-match-introduce-2 syntax-local-match-introduce-fallback)))

