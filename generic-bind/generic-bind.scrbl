#lang scribble/manual
@(require scribble/eval
          (for-syntax "main.rkt")
          "main.rkt"
          (for-label "main.rkt"
                     racket))

@title{Racket Generic Binding Forms}

@defmodule[generic-bind #:use-sources ("main.rkt")]

This library implements Racket binding forms (ie @racket[~define], @racket[~lambda], @racket[~let], etc.) that support generic binding instances. A generic binding instance is a syntax object that implements an "interface" (currently just some syntax properties) that the binding forms then use to determine how to proceed with the binding. 

This moves the binding "logic" to the binding site itself and enables one binding form to support many different binding modes (ie, @racket[match] or @racket[values]), rather than have to manually implement a version of each binding form for all the different binding combinations (ie, @racket[define-values], @racket[match-define], @racket[match-define-values], etc.). 

The hope is that the forms in this library will eventually replace their analogous counterparts in Racket.

@(define the-eval (make-base-eval))
@(the-eval '(require "main.rkt"))

@author[@author+email["Stephen Chang" "stchang@racket-lang.org"]]

@; Generic Binding Instances --------------------------------------------------
@section{Generic Binding Instances}

The forms described in this section may be used only in binding positions of binding forms that support generic bindings. Any other use is invalid. For examples, see the next section, @secref{binding-forms}.

Generic binding instances are further separated into two categories:
@itemize[@item{bindings that may be used anywhere, and}
         @item{bindings that may only be used in "define" contexts.}]

The second category is a subset of the first and is needed only to properly handle Racket's multiple return @racket[values]. Since Racket functions cannot receive @racket[values], we must know what we are binding at the time of the binding. This means that @racket[~let] may support @racket[values] binding, but @racket[~define] or @racket[~lambda] may not. Thus, the second category essentially excludes @racket[values]-bindings.

A few generic binding instances are currently supported. Defining new generic binding instances is possible but not well supported. To learn how to do so anyways, see @secref{new-instances}.

@defform[($ match-pattern)]{
  The @racket[match] binding instance. The required pattern is a @racket[match] pattern. May be used in any context.
      
  Currently does not support nested generic binding instances. In other words, the binding sites in the given pattern may only be identifiers and not other generic binding instances.}

@defform/subs[(~vs b ...) ([b define-allowable-generic-binding])]{
  Binding instance for @racket[values]. May only be used in "let" contexts (which means not in @racket[~define] or @racket[~lambda] for example).
      
  Currently supports one-level of nested generic binding instances. In other words, each binding site in the @racket[~vs] form may be either an identifier, or another generic binding instances. However, any nested binding positions are considered "define" contexts. This means that one cannot nest @racket[~vs] bindings.}

@defform[(⋈ b ...)]{ Same as @racket[~vs]. }

@defform[($: x xs)]{
  A @racket[match] binding instance where the outer @racket[match] pattern is @racket[cons]. Equivalent to @racket[($ (cons x xs))]. 
    
    No nested generic binding instances are allowed.}

@defform*[(($list x ...)
           ($list x ... : rst))]{
  A @racket[match] binding instance where the outer @racket[match] pattern is either @racket[list] or @racket[list-rest], depending whether a rest argument is supplied. Equivalent to either @racket[($ (list x ...))] or @racket[($ (list-rest x ... rst))].
    
    No nested generic binding instances are allowed.}

@defthing[$null _]{ A @racket[match] binding instance for @racket[null].}

                                
@; Core Generic Binding Forms ------------------------------------------------------
@section[#:tag "binding-forms"]{Core Generic Binding Forms}

@defform*/subs[((~define id body)
                (~define b body)
                (~define (f db ...) body ...))
               ([id identifier?]
                [b generic-binding]
                [db define-allowable-generic-binding])]{
  Same as @racket[define] but each binding site (except for the function name) may be either an identifier or a (define-allowable) generic binding instance. (So the @racket[~vs] values-binding form is not allowed.)}

@defform[(~def ...)]{ Same as @racket[~define].}
@defform[(~d ...)]{ Same as @racket[~define].}
                                         
@defform*/subs[((~lambda id body ...)
                (~lambda b body ...)
                (~lambda (b ...) body ...))
               ([b define-allowable-generic-binding])]{
  Same as @racket[lambda] but each binding site may be either an identifier or a (define-allowable) generic binding instance. (So the @racket[~vs] values-binding form is not allowed.)
          
  If a single identifier is given with no parentheses, then the arguments are put in a list, just like @racket[lambda]. A single generic bind instance may also be used without parens. If a list of bindings if given, then each may be either an identifier or a generic bind instance.}

@defform[(~lam ...)]{ Same as @racket[~lambda].}
@defform[(~l ...)]{ Same as @racket[~lambda].}
@defform[(~λ ...)]{ Same as @racket[~lambda].}

@defform/subs[(~case-lambda clause ...)
              ([clause (header body ...)])]{
  A "simulation" of a @racket[case-lambda] that also acccepts generic binds. 
                      
  Since @racket[case-lambda] only distinguishes between cases based on the number of arguments, this does not expand to @racket[case-lambda] but instead defines a series of @racket[lambda]s, trying each one and backtracking on @racket[exn:misc:match?].}

@defform[(~case-lam clause ...)]{ Same as @racket[~case-lambda]}

@defform*/subs[((~case-define f clause1 ...)
                (~case-define f clause2 ...))
               ([clause1 (header body ...)]
                [clase2 (b ... → body ...)])]{
  Syntax for naming a @racket[~case-lambda]}
                                             
@defform*/subs[((~let loop ([db e] ...) body ...)
                (~let ([b e] ...) body ...))
               ([db define-allowable-generic-bind]
                [b generic-bind])]{
  Same as @racket[let], but with generic bind support. If a name is given to the let, then the bindings must be define-allowable, since it's essentially defining a function. If no name is given to the @racket[~let], then any generic bind is allowed.}
                                  
@defform/subs[(~let* ([b e] ...) body ...)
              ([v generic-bind])]{ Same as @racket[let*], but with generic bind support.}
@defform/subs[(~letrec ([b e] ...) body ...)
              ([v generic-bind])]{ Same as @racket[letrec], but with generic bind support.}                                             
@defform[(~case-def clause ...)]{ Same as @racket[~case-define]}


@; Comprehension --------------------------------------------------------------

@; Generic Comprehension Forms ------------------------------------------------------
@section{Generic Comprehension Forms}

All the forms in this section are the same as their Racket counterparts (see @racket[for]), but with added generic bind support.

@defform[(~for ...)]{}
@defform[(~for/list ...)]{}
@defform[(~for/lists ...)]{}
@defform[(~for/vector ...)]{}
@defform[(~for/fold ...)]{}
@defform[(~for/first ...)]{}
@defform[(~for/last ...)]{}
@defform[(~for/or ...)]{}
@defform[(~for/and ...)]{}
@defform[(~for/sum ...)]{}
@defform[(~for/product ...)]{}
@defform[(~for/hash ...)]{}
@defform[(~for/hasheq ...)]{}
@defform[(~for/hasheqv ...)]{}

@defform[(~for* ...)]{}
@defform[(~for*/list ...)]{}
@defform[(~for*/lists ...)]{}
@defform[(~for*/vector ...)]{}
@defform[(~for*/fold ...)]{}
@defform[(~for*/first ...)]{}
@defform[(~for*/last ...)]{}
@defform[(~for*/or ...)]{}
@defform[(~for*/and ...)]{}
@defform[(~for*/sum ...)]{}
@defform[(~for*/product ...)]{}
@defform[(~for*/hash ...)]{}
@defform[(~for*/hasheq ...)]{}
@defform[(~for*/hasheqv ...)]{}

@; Implementing New Generic Binding Instances ---------------------------------
@section[#:tag "new-instances"]{Implementing New Generic Binding Instances}