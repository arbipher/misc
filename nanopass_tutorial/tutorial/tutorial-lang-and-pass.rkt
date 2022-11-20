#lang nanopass

; part 1 - languages

(define (foo? x)
  (eq? x 'Foo))

(define (bar? x)
  (eq? x 'Bar))

(define-language Foo
  (terminals
   (foo (f)))
  (Expr (e)
    f))

(define-parser parse-foo Foo)
(parse-foo 'Foo)

(define-language Foo2
  (Expr (e)
    (foo)))

(define-parser parse-foo2 Foo2)
(parse-foo2 '(foo))

(define-language Foo3
  (terminals
   (foo (f)))
  (Expr (e)
    (F f)))

(define-parser parse-foo3 Foo3)
(parse-foo3 '(F Foo))

(define-language Foo4
  (terminals
   (foo (f)))
  (Expr (e)
    (e* ... e)
    (F f)))

(define-parser parse-foo4 Foo4)
(parse-foo4 '(F Foo))
(parse-foo4
 '(
   (F Foo)
   (F Foo)
   (F Foo)
  )
)

(define-language Foo5
  (terminals
   (foo (f)))
  (Expr (e)
    (e* ... e)
    (f)))

(define-parser parse-foo5 Foo5)
#;(parse-foo5
 '(
   (Foo)
   (Foo)
   (Foo)))

(define-language Foo6
  (terminals
   (foo (f)))
  (Expr (e)
    (f)
    (e* ... e)))

(define-parser parse-foo6 Foo6)
(parse-foo6
 '(
   (Foo)
   (Foo)
   (Foo)))

(define-language Foo7
  (terminals
   (foo (f)))
  (Expr (e)
    (e* ... e)
    f))

(define-parser parse-foo7 Foo7)
(parse-foo7
 '(
   Foo
   Foo
   Foo))
(parse-foo7
 '(
   (Foo)
   (Foo)
   (Foo)))

(define-language Foo8
  (terminals
   (foo (f)))
  (Expr (e)
    f
    (e* ... e)))

(define-parser parse-foo8 Foo8)
(parse-foo8
 '(
   Foo
   Foo
   Foo))
(parse-foo8
 '(
   (Foo)
   (Foo)
   (Foo)))

; part 2 - passes

#;(define-language Foo
  (terminals
   (foo (f)))
  (Expr (e)
    f))

(define-language Bar
  (extends Foo)
  (terminals
   (- (foo (f)))
   (+ (bar (b))))
  (Expr (e)
   (- f)
   (+ b)))

(define-pass Foo->Bar : Foo (e) -> Bar ()
  (Expr : Expr (e) -> Expr ()
    [,f `Bar]))

(Foo->Bar
 (parse-foo 'Foo))

#;(define-language Foo2
  (Expr (e)
    (foo)))

(define-language Bar2
  (extends Foo2)
  (Expr (e)
   (- (foo))
   (+ (bar))))

(define-pass Foo2->Bar2 : Foo2 (e) -> Bar2 ()
  (Expr : Expr (e) -> Expr ()
    [(foo) `(bar)]))

(Foo2->Bar2
 (parse-foo2 '(foo)))

#;(define-language Foo3
  (terminals
   (foo (f)))
  (Expr (e)
    (F f)))

(define-language Bar3
  (extends Foo3)
  (terminals
   (- (foo (f)))
   (+ (bar (b))))
  (Expr (e)
   (- (F f))
   (+ (B b)))
  )

(define-pass Foo3->Bar3 : Foo3 (e) -> Bar3 ()
  (definitions
    (define bv 'Bar))
  (Expr : Expr (e) -> Expr ()
    ;[(F ,f) `(B bar)]
    ;[(F ,f) `(B ,'bar)]
    [(F ,f) `(B ,bv)]
))

(Foo3->Bar3
 (parse-foo3 '(F Foo)))

#;(define-language Foo4
  (terminals
   (foo (f)))
  (Expr (e)
    (e* ... e)
    (F f)))

(define-language Bar4
  (extends Foo4)
  (terminals
   (- (foo (f)))
   (+ (bar (b))))
  (Expr (e)
   (- (F f))
   (+ (B b)))
  )

(define-pass Foo4->Bar4 : Foo4 (e) -> Bar4 ()
  (definitions
    (define bv 'Bar))
  (Expr : Expr (e) -> Expr ()
    [(F ,f) `(B ,bv)]
))

(Foo4->Bar4
 (parse-foo4
  '(
    (F Foo)
    (F Foo)
    (F Foo))))

#;(define-language Foo6
  (terminals
   (foo (f)))
  (Expr (e)
    (f)
    (e* ... e)))

(define-language Bar6
  (extends Foo6)
  (terminals
   (- (foo (f)))
   (+ (bar (b))))
  (Expr (e)
   (- (f))
   (+ (b))))

(define-pass Foo6->Bar6 : Foo6 (e) -> Bar6 ()
  (definitions
    (define bv 'Bar))
  (Expr : Expr (e) -> Expr ()
    [(,f) `(,bv)]
))

(Foo6->Bar6
 (parse-foo6
  '(
    (Foo)
    (Foo)
    (Foo))))

#;(define-language Foo7
  (terminals
   (foo (f)))
  (Expr (e)
    (e* ... e)
    f))

(define-language Bar7
  (extends Foo7)
  (terminals
   (- (foo (f)))
   (+ (bar (b))))
  (Expr (e)
   (- f)
   (+ b)))

(define-pass Foo7->Bar7 : Foo7 (e) -> Bar7 ()
  (definitions
    (define bv 'Bar))
  (Expr : Expr (e) -> Expr ()
    [,f `,bv]
))

(Foo7->Bar7
 (parse-foo7
  '(
    Foo
    Foo
    Foo)))
(Foo7->Bar7
 (parse-foo7
  '(
    (Foo)
    (Foo)
    (Foo))))

#;(define-language Foo8
  (terminals
   (foo (f)))
  (Expr (e)
    f
    (e* ... e)))

(define-language Bar8
  (extends Foo8)
  (terminals
   (- (foo (f)))
   (+ (bar (b))))
  (Expr (e)
   (- f)
   (+ b)))

(define-pass Foo8->Bar8 : Foo8 (e) -> Bar8 ()
  (definitions
    (define bv 'Bar))
  (Expr : Expr (e) -> Expr ()
    [,f `,bv]
))

(Foo8->Bar8
 (parse-foo8
  '(
    Foo
    Foo
    Foo)))

(Foo8->Bar8
 (parse-foo8
  '(
    (Foo)
    (Foo)
    (Foo))))