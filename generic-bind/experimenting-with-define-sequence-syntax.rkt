#lang racket
(require syntax/unsafe/for-transform)
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ [(x ...) (form y ...)])
; #;(let (#;[xformer (sequence-transformer-ref (syntax-local-value #'form) 1)]
; [intro (make-syntax-introducer)])
; (printf "~a\n" (syntax->datum
; (expand-clause #'[(x) (form y ...)] #'[(x) (form y ...)])
; #;(xformer #'[(x) (form y ...)])
; #;(xformer (intro (syntax-local-introduce #'[x (form y ...)]))))))
     (with-syntax ([((outer-binding ...)
                     outer-check
                     (loop-binding ...)
                     pos-guard
                     (inner-binding ...)
                     pre-guard
                     post-guard
                     (loop-arg ...))
                    (expand-for-clause #'[(x ...) (form y ...)] #'[(x ...) (form y ...)])])
       (printf "~a\n" (syntax->datum #'((outer-binding ...)
                     outer-check
                     (loop-binding ...)
                     pos-guard
                     (inner-binding ...)
                     pre-guard
                     post-guard
                     (loop-arg ...))))
       #'1)]))
       
;(test [(x) (in-range 10)])
#;((((start) 0) ((end) 10) ((inc) 1))
   (unless (and (real? start) (real? end) (real? inc)) (in-range start end inc))
   ((pos start))
   (unsafe-fx< pos end)
   (((x) pos))
   #t
   #t
   ((unsafe-fx+ pos inc)))

;(test [(x) (list 1 2 3 4 5)])
#;((((pos->vals pos-next init pos-cont? val-cont? all-cont?) (make-sequence '(x) (list 1 2 3 4 5))))
   (void)
   ((pos init))
   (if pos-cont? (pos-cont? pos) #t)
   (((x) (pos->vals pos)))
   (if val-cont? (val-cont? x) #t)
   (if all-cont? (all-cont? pos x) #t)
   ((pos-next pos)))

;(test [(x) (vector 1 2 3 4 5)])
#;((((pos->vals pos-next init pos-cont? val-cont? all-cont?) (make-sequence '(x) (vector 1 2 3 4 5))))
   (void)
   ((pos init))
   (if pos-cont? (pos-cont? pos) #t)
   (((x) (pos->vals pos)))
   (if val-cont? (val-cont? x) #t)
   (if all-cont? (all-cont? pos x) #t)
   ((pos-next pos)))

;(test [(x) (in-hash-keys (hash 1 2 3 4 5 6))])
#;((((pos->vals pos-next init pos-cont? val-cont? all-cont?) (make-sequence '(x) (in-hash-keys (hash 1 2 3 4 5 6)))))
   (void)
   ((pos init))
   (if pos-cont? (pos-cont? pos) #t)
   (((x) (pos->vals pos)))
   (if val-cont? (val-cont? x) #t)
   (if all-cont? (all-cont? pos x) #t)
   ((pos-next pos)))

;(test [(x y) (in-hash (hash 1 2 3 4 5 6))])
#;((((pos->vals pos-next init pos-cont? val-cont? all-cont?) (make-sequence '(x y) (in-hash (hash 1 2 3 4 5 6)))))
   (void)
   ((pos init))
   (if pos-cont? (pos-cont? pos) #t)
   (((x y) (pos->vals pos)))
   (if val-cont? (val-cont? x y) #t)
   (if all-cont? (all-cont? pos x y) #t)
   ((pos-next pos)))

;(test [(x) 10])
;(test [(v) (in-list (list 1))])
(test [(v) (stream-filter identity (in-range 10))])
#;((((pos->vals pos-next init pos-cont? val-cont? all-cont?) ; outer binding
   (make-sequence '(v) (stream-filter identity (in-range 10)))))
 (void) ; outer check
 ((pos init)) ; loop binding
 (if pos-cont? (pos-cont? pos) #t) ; pos guard
 (((v) (pos->vals pos))) ; inner binding
 (if val-cont? (val-cont? v) #t) ; pre guard
 (if all-cont? (all-cont? pos v) #t) ; post guard
 ((pos-next pos))) ; loop arg