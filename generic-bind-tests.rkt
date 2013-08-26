#lang racket
(require rackunit)
(require "generic-bind.rkt")

(define-syntax-rule (⋈ x ...) (~vs x ...))
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

;; lambda tests

(check-equal? ((~λ (x) x) 111) 111)
(check-equal? ((~λ rst rst) 1 2 3) (list 1 2 3))
(check-equal? ((~λ (x [y 0] #:z z #:a [a 10]) (+ x y z a)) 1 #:z 10) 21)
(check-equal? ((~λ (x [y 0] #:z z #:a [a 10]) (+ x y z a)) 1 22 #:z 10) 43)
(check-equal? ((~λ (x [y 0] #:z z #:a [a 10]) (+ x y z a)) 1 #:z 10 #:a 111) 122)
(check-equal? ((~lambda (x [y 0] #:z z #:a [a 10]) (+ x y z a)) 1 #:z 10 #:a 111) 122)

;; single arg
(check-equal? ((~λ ($ (list x y)) (+ x y)) (list 14 56)) 70)
(check-equal? ((~λ (~vs v1 v2) (+ ~vs v1 v2)) 1 2 3) 6) ;; shadow ~vs
; (~λ ((~vs v1 v2)) (+ v1 v2)) ; error
(check-equal? ((~λ (($ (list x y)) ($ (cons a b))) (+ a b x y)) 
               (list 1 2) (cons 3 4))
              10)
(check-equal? ((~λ (x y ($ (A a b))) (+ x y a b)) 10 20 (A 30 40)) 100)

;; example for spurious defines in nested generic binds
(~define (⋈ ($ (cons x20 x21)) y22) (values (cons 56 67) 78))
(check-equal? x20 56) (check-equal? x21 67) (check-equal? y22 78)

;; ~let (2013-08-25: doesn't support values)
;; some corner cases
(define x55 155)
(check-equal? (~let ([x55 (add1 x55)]) x55) 156)
(define x56 156)
(check-equal? (~let ([x56 157] [x57 x56]) x57) 156)

(check-equal? (~let ([($ (list x58 x59)) (list 158 159)]) (list x59 x58))
              (list 159 158))



;; named let
(check-equal? 20
              (~let L ([($ (list x y ...)) (list 1 2 3 4 5)] [n 4])
                    (if (zero? n) 0
                        (+ n x (L y (sub1 n))))))

;; check named let corner cases
;(let loop ([x 1] [x 2]) x) ; duplicate id error
;(~let loop ([x 1] [x 2]) x) ; duplicate id error (but references define)
(define x8 109)
(check-equal? 109 (let loop ([x8 1] [y x8]) y))
(check-equal? 109 (~let loop ([x8 1] [y x8]) y))

;; ~let*
(check-equal? (~let* ([x 46436] [y x]) y) 46436)
;(check-equal? 
(check-equal? (~let* ([($ (list x y)) (list 1010 2020)] [z (+ x y)]) z) 3030)
(check-equal? (~let* ([(⋈ v1 v2) (values 4040 5050)] [z (+ v1 v2)]) z) 9090)
(check-equal? (~let* ([(⋈ v3 ($ (list xy yx))) (values 202 (list 303 404))] 
                      [z (+ v3 (- yx xy))])
                     z)
              303)
(check-equal? (~let* ([x 1122] 
                      [(⋈ ($ (cons ab bc)) ($ (list-rest y ys)))
                       (values (cons x (* x 2)) (list x (add1 x) (sub1 x)))])
                     (+ (- bc ab) (- y (+ (car ys) (cadr ys))))) 
              0)
(check-equal? (~let* ([x 101] [x 202]) x) 202)

;; ~letrec*
(require math/number-theory) ; factorial
(check-equal? (~letrec ([x x]) x) (letrec ([x x]) x))
(check-equal? (~letrec ([f (λ (x) (if (zero? x) 1 (* x (f (sub1 x)))))]) (f 10)) 
              (factorial 10))
(check-true
 (~letrec ([evn? (λ (x) (if (zero? x) true (od? (sub1 x))))]
           [od? (λ (x) (if (zero? x) false (evn? (sub1 x))))])
   (evn? 10)))
(check-true
 (~letrec ([(⋈ evn? od?)
            (values (λ (x) (if (zero? x) true (od? (sub1 x))))
                    (λ (x) (if (zero? x) false (evn? (sub1 x)))))])
   (evn? 10)))
(check-true
 (~letrec ([($ (list evn? od?))
            (list (λ (x) (if (zero? x) true (od? (sub1 x))))
                  (λ (x) (if (zero? x) false (evn? (sub1 x)))))])
          (and (od? 101) (evn? 10))))
(check-true
 (~letrec ([(⋈ evn? ($ (list od?)))
            (values (λ (x) (if (zero? x) true (od? (sub1 x))))
                    (list (λ (x) (if (zero? x) false (evn? (sub1 x))))))])
          (and (od? 101) (evn? 10))))
