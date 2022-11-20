#lang nanopass

(define (foo? x)
  (eq? x 'Foo))

(define (bar? x)
  (eq? x 'Bar))

(define bv 'Bar)

(define-language FooBar
  (terminals
   (foo (f))
   (bar (b)))
  (Expr (e)
   (e* ... e)
   (F f)
   (B b)))

(define-parser parse-FooBar FooBar)

(define-pass removeFoo : FooBar (e) -> FooBar ()
  (Expr : Expr (e) -> Expr ()
        [(F ,f) `(B ,bv)]))

(removeFoo
 (parse-FooBar
  '(
    (F Foo)
    (F Foo))))

(define-pass removeFoo2 : FooBar (e) -> FooBar ()
  (Expr : Expr (e) -> Expr ()
        [(,[e*] ... ,[e]) `(,e* ... ,e)]
        [(F ,f) `(B ,bv)]))

(removeFoo2
 (parse-FooBar
  '(
    (F Foo)
    (F Foo))))

(define-pass removeFoo3 : FooBar (e) -> FooBar ()
  (Expr : Expr (e) -> Expr ()
        [(,e* ... ,e) `(,e* ... ,(Expr e))]
        [(F ,f) `(B ,bv)]))

(removeFoo3
 (parse-FooBar
  '(
    (F Foo)
    (F Foo))))

(define-pass removeFoo4 : FooBar (e) -> FooBar ()
  (Expr : Expr (e) -> Expr ()
        [(,e* ... ,e) `(,(Expr e*) ... ,(Expr e))]
        [(F ,f) `(B ,bv)]))

;nanopass-record-tag: contract violation
;  expected: nanopass-record?
;  given: '(#<language:FooBar: (F Foo)>)
#;(removeFoo4
 (parse-FooBar
  '(
    (F Foo)
    (F Foo))))

(define-pass removeFoo5 : FooBar (e) -> FooBar ()
  (Expr : Expr (e) -> Expr ()
        [(,e* ... ,e) `(,(map Expr e*) ... ,(Expr e))]
        [(F ,f) `(B ,bv)]))

(removeFoo5
 (parse-FooBar
  '(
    (F Foo)
    (F Foo))))

(define-language FooBarEmpty
  (terminals
   (foo (f))
   (bar (b)))
  (Expr (e)
   (e* ...)
   (F f)
   (B b)))

(define-parser parse-FooBarEmpty FooBarEmpty)

(define-pass removeFoo6 : FooBarEmpty (e) -> FooBarEmpty ()
  (Expr : Expr (e) -> Expr ()
        [(,e* ... ) `(,(map Expr e*) ... )]
        [(F ,f) `(B ,bv)]))

(removeFoo6
 (parse-FooBarEmpty
  '(
    (F Foo)
    (F Foo))))

(define-pass removeFoo7 : FooBar (e) -> FooBar ()
  (Expr : Expr (ex) -> Expr ()
        [(,e* ... ,e)
         (display (list? e*))
         (display (list? (append e* (list e))))
         `(,(map Expr (append e* (list e))) ...)]
        [(F ,f) `(B ,bv)]))
;removeFoo7: expected Expr but received (#<language:FooBar: (B Bar)> #<language:FooBar: (B Bar)>) in field e of (e* ... e) from expression (map Expr (append e* (list e))) /Users/ex/Desktop/workspace/odefa2/nano/nanodefa/tutorial-cata.rkt:106.12
#;(removeFoo7
 (parse-FooBar
  '(
    (F Foo)
    (F Foo))))

(define-pass removeFoo8 : FooBarEmpty (e) -> FooBarEmpty ()
  (Expr : Expr (e) -> Expr ()
        [(,e* ... ) `(,(map Expr (append e* (list `(B ,bv)))) ... )]
        [(F ,f) `(B ,bv)]))

(removeFoo8
 (parse-FooBarEmpty
  '(
    (F Foo)
    (F Foo))))

(define-pass removeFoo9 : FooBar (e) -> FooBar ()
  (Expr : Expr (ex) -> Expr ()
        [(,e* ... ,e)
         (let* ([es (append e* (list e))]
                [nes (map Expr es)])
           ;(match-define `(,ne* ... ,ne) nes)
           ;`(,ne* ... ,ne))
           `(,(drop-right nes 1) ... ,(last nes)))
           ]
        [(F ,f) `(B ,bv)]))

(removeFoo9
 (parse-FooBar
  '(
    (F Foo)
    (F Foo))))