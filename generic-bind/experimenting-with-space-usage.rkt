#lang racket
(let loop ([s (stream-filter identity (in-range 10000000))] [sum 0]) 
    (if (stream-empty? s) sum 
        (loop 
         (stream-rest s) 
         (let loop2 ([t (stream-filter identity (in-range 1))] [sum sum]) 
           (if (stream-empty? t) sum 
               (loop2 
                (stream-rest t) 
                (+ sum (stream-first s) (stream-first t))))))))