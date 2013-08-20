#lang racket
(require rackunit)
(require "generic-bind.rkt")

(define-values 
  ((:=m list x y) (:=m cons a b))
  (values (list 1 2) (cons 10 20)))

(check-equal? (+ x y) 3)
(check-equal? (* a b) 200)

(define-values (x1 x2 x3) (values 9 10 11))
(check-equal? (+ x1 x2 x3) 30)

(let-values ([((:=m list c d) (:=m cons e f))
              (values (list 100 200) (cons 3 4))])
  (check-equal? (+ c d) 300)
  (check-equal? (* e f) 12))


(define (f x [y 10] #:z [z 0]) (+ x y z))
(check-equal? (f 100) 110)
(check-equal? (f 100 200) 300)
(check-equal? (f 100 200 #:z 300) 600)

(define x4 10000) (check-equal? x4 10000)

(struct A (x y))
(define (:=m A x5 y5) (A 101 202))
(check-equal? x5 101) (check-equal? y5 202)
(define (:=v v1 v2) (values 987 654))
(check-equal? v1 987) (check-equal? v2 654)

(for ([x 10])
  (check-equal? x x))