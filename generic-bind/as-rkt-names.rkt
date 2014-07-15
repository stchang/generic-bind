#lang racket
(require racket/provide)
(require "main.rkt")
(provide (filtered-out
          (λ (name)
            (or (and (regexp-match? #rx"^~.*" name) ; drop ~ prefix
                     (not (string=? name "~vs")) ; except ~vs
                     (substring name 1))
                name)) ;; else provide as is
          (all-from-out "main.rkt")))