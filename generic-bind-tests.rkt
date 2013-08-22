#lang racket
(require rackunit)
(require "generic-bind.rkt")

(define-syntax-rule (≫ x ...) (:=v x ...))
(define-syntax-rule (◇ x ...) (:=m x ...))

(define-values 
  ((:=m (list x y)) (:=m (cons a b)))
  (values (list 1 2) (cons 10 20)))

(check-equal? (+ x y) 3)
(check-equal? (* a b) 200)

(define-values (x1 x2 x3) (values 9 10 11))
(check-equal? (+ x1 x2 x3) 30)

(let-values ([((:=m (list c d)) (:=m (cons e f)))
              (values (list 100 200) (cons 3 4))])
  (check-equal? (+ c d) 300)
  (check-equal? (* e f) 12))


(new-define (f x [y 10] #:z [z 0]) (+ x y z))
(check-equal? (f 100) 110)
(check-equal? (f 100 200) 300)
(check-equal? (f 100 200 #:z 300) 600)

(new-define x4 10000) (check-equal? x4 10000)

(struct A (x y))
(new-define (◇ (A x5 y5)) (A 101 202))
(check-equal? x5 101) (check-equal? y5 202)
(new-define (≫ v1 v2) (values 987 654))
(check-equal? v1 987) (check-equal? v2 654)

(for ([x 10])
  (check-equal? x x))


(new-define (g (◇ (list (list a b) y ...))) (apply + a b y))
(check-equal? (g (list (list 101 202) 303)) 606)

; (new-define (gv (:=v x y)) (+ x y))) ; expand fail because of invalid :=v bind

;; test non-list pats
(new-define (gg (◇ xxx)) (add1 xxx))
(check-equal? (gg 10001) 10002)
(new-define (ggg (◇ _)) 12345)
(check-equal? (ggg 5432) 12345)
(new-define (gggg (◇ 11111)) 22222)
(check-exn exn:misc:match? (λ () (gggg 111))) ; match fail
(check-equal? (gggg 11111) 22222)

;; nested bind patterns
(new-define (≫ (◇ (list xxxx yyyy)) zzzz) (values (list 11 22) 33))
(check-equal? xxxx 11)
(check-equal? yyyy 22)
(check-equal? zzzz 33)

(check-equal? (new-let ([x 10]) x) 10)
(check-equal? (new-let ([x 10] [y 20]) (+ x y)) 30)
(check-equal? (new-let ([(◇ (list x y)) (list 10 20)]) (+ x y)) 30)
(check-equal? (new-let ([(◇ (list x y)) (list 10 20)] [(◇ (cons a b)) (cons 40 50)]) (+ x y a b)) 120)
(check-equal? (new-let ([(≫ v1 v2) (values 10 20)]) (+ v1 v2)) 30)
(check-equal? (new-let ([(≫ (◇ (list ab cd))) (values (list 99 88))]) (- cd ab)) -11)
(check-equal? (new-let ([(≫ (◇ (list ab cd)) (◇ (cons ef fg))) (values (list 99 88) (cons 77 66))]) (+ (- cd ab) (- fg ef))) -22)
(check-equal? (new-let loop ([(◇ (list x y ...)) (list 1 2 3 4 5)]) (if (null? y) x (+ x (loop y)))) 15)