#lang racket
(require rackunit)
(require "generic-bind.rkt")

(define-syntax-rule (⋈ x ...) (~v x ...))
(define-syntax-rule ($ x ...) (~m x ...))

;; define non-fns
(~define x1 (+ 1 2))
(check-equal? x1 3)

(~define x2 (list 1 2))
(check-equal? x2 (list 1 2))

(~define ($ (list y1 y2)) (list 10 20))
(check-equal? y1 10) (check-equal? y2 20)
(~define ($ (list (cons y3 y4) y5)) (list (cons 20 30) 40))
(check-equal? y3 20) (check-equal? y4 30) (check-equal? y5 40)
(~define (⋈ v1 v2) (values 101 202))
(check-equal? v1 101) (check-equal? v2 202)
(~define (⋈ ($ (list x4 x5)) x6) (values (list 99 999) 9999))
(check-equal? x4 99)(check-equal? x5 999)(check-equal? x6 9999)

(struct A (x y))
(~define ($ (A x7 y7)) (A 101 202))
(check-equal? x7 101) (check-equal? y7 202)

;; define fns
(~define (f1 x [y 10] #:z [z 0]) (+ x y z))
(check-equal? (f1 100) 110)
(check-equal? (f1 100 200) 300)
(check-equal? (f1 100 200 #:z 300) 600)

(~define (f3 . rst) rst)
(check-equal? (f3 1 2 3) (list 1 2 3))
(check-equal? (f3) null)
(~define (f4 x y . rst) (cons x (cons y rst)))
(check-equal? (f4 1 2 3 4 5 6) (list 1 2 3 4 5 6))
(check-equal? (f4 1 2) (list 1 2))

(~define (f2 ($ (list x y))) (- x y))
(check-equal? (f2 (list 145 45)) 100)
(~define (g1 ($ (list (list a b) y ...))) (apply + a b y))
(check-equal? (g1 (list (list 101 202) 303)) 606)

(~define (f5 ($ (list (list x y) z)) . rst) (cons x (cons y (cons z rst))))
(check-equal? (f5 (list (list 1 2) 3)) (list 1 2 3))
(check-equal? (f5 (list (list 1 2) 3) 4 5) (list 1 2 3 4 5))

;; test non-list pats
(~define (gg ($ xxx)) (add1 xxx))
(check-equal? (gg 10001) 10002)
(~define (ggg ($ _)) 12345)
(check-equal? (ggg 5432) 12345)
(~define (gggg ($ 11111)) 22222)
(check-exn exn:misc:match? (λ () (gggg 111))) ; match fail
(check-equal? (gggg 11111) 22222)
