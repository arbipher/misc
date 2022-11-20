#lang nanopass

(define (foo? x)
  (eq? x 'Foo))

(define fv 'Foo)

(define-language FooLang
  (terminals
   (foo (f)))
  (Expr (e)
   (e* ... e)
   f))

(define program
  '(
    Foo
    (
     Foo
     Foo
     (
      Foo
      Foo
      Foo
     )
    )
   )
)

(define-parser parser-Foo FooLang)

(parser-Foo '(Foo Foo))

(define f
  (with-output-language (FooLang Expr)
    `Foo))

(define ff
  (with-output-language (FooLang Expr)
    `(Foo Foo)))

(display f)

(display ff)

(define (case-foo x)
  (nanopass-case (FooLang Expr) x
    [(,e* ... ,e) "Expressions"]
    [,f "Fooooo"]))

(case-foo f)
(case-foo ff)

(define-pass name : FooLang (e) -> * ()
  (name-Expr : Expr (e) -> * ()
    [(,e* ... ,e) "Expressions"]
    [,f "Fooooo"]))

(name f)
(name ff)
  

