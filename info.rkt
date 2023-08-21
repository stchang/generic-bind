#lang info
(define collection 'multi)
;; technically, I should revert to #lang setup/infotab
;; and declare the dependency on racket, not base
;; see: https://www.mail-archive.com/users@racket-lang.org/msg23164.html
(define deps '(("base" #:version "8.4.0.2")))
(define build-deps '("rackunit-lib" "racket-doc" "scribble-lib" "math-lib" "compatibility-lib"))
