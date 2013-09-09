#lang racket
(require "generic-bind.rkt")

#;(let loop ([s (stream-filter identity (in-range 1000000))] [sum 0]) 
    (if (stream-empty? s) sum 
        (loop 
         (stream-rest s) 
         (let loop2 ([t (stream-filter identity (in-range 10))] [sum sum]) 
           (if (stream-empty? t) sum 
               (loop2 
                (stream-rest t) 
                (+ sum (stream-first s) (stream-first t))))))))

(let ([ss (stream-filter identity (in-range 10000000))])
  (let loop ([s ss] [sum 0])
    (if (stream-empty? s) sum 
        (loop 
         (stream-rest s) 
         (+ sum (stream-first s))
         #;(let loop2 ([t (stream-filter identity (in-range 10))] [sum sum]) 
           (if (stream-empty? t) sum 
               (loop2 
                (stream-rest t) 
                (+ sum (stream-first s) (stream-first t)))))))))

#;(~for/sum ([s (stream-filter identity (in-range 10000000))]
          #:when #t
          #;[t (stream-filter identity (in-range 1))])
  s)