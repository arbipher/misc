#lang nanopass

(define (foo? x)
  (eq? x 'Foo))

(define (bar? x)
  (eq? x 'Bar))

(define bv 'Bar)

(define program
  '(
   Foo
   Foo
   (fe (
        Foo
        Foo))
   (fe (
        Foo
        (fe (
             Foo
             (fe (
                  Foo
                  Foo))))))))

(define-language FooLang
  (terminals
   (foo (f))
   (bar (b)))
  (Expr (e)
   (e* ... e)
   FF)
  (Foo (FF)
   (fe e)
   f
   b))

(define-parser parser-Foo FooLang)

(define-pass removeFoo : FooLang (e) -> FooLang ()
  (Foo->Bar : Foo (e) -> Foo ()
        [,f `,bv]))

(removeFoo
 (parser-Foo program))

(define-pass removeFoo2 : FooLang (e) -> FooLang ()
  (Foo->Bar : Foo (e) -> Foo ()
        [,f `,bv]
        [(fe ,[e]) `(fe ,e)]))

(removeFoo2
 (parser-Foo program))

;n: unbound identifier in module in: n
#;(define-pass countFooFail : FooLang (e) -> FooLang ()
  (Expand-Expr : Expr (e) -> Expr (n)
        [(,[e*] ... ,[e])
         (values `(,e* ... ,e) 0)])
  (Foo->Bar : Foo (e) -> Foo (n)
        [,f (values `,bv 0)]))
#;(countFooFail
 (parser-Foo program))

#;(define-pass countFooPass : FooLang (e) -> FooLang ()
  (Expand-Expr : Expr (e) -> Expr (n)
        [(,[e*] ... ,[e])
         (values `(,e* ... ,e) 0)])
  (Foo->Bar : Foo (e) -> Foo (n)
        [(fe ,e)
         (values `(fe ,e) 0)]
        [,f
         (values `,bv 0)]
        [,b
         (values `,bv 0)]))
#;(countFooPass
 (parser-Foo program))

#;(define-pass countFooNoPrint : FooLang (e) -> FooLang ()
  (Expand-Expr : Expr (e n) -> Expr (n)
        [(,[e*] ... ,[e])
         (display n)
         (values `(,e* ... ,e) 0)])
  (Foo->Bar : Foo (e n) -> Foo (n)
        [(fe ,e)
         (values `(fe ,e) 0)]
        [,f
         (values `,bv 0)]
        [,b
         (values `,bv 0)]))
#;(countFooNoPrint
 (parser-Foo program))

#;(define-pass countFooWithN : FooLang (e n) -> FooLang ()
  (Expand-Expr : Expr (e n) -> Expr (n)
        [(,[e*] ... ,[e])
         (display n)
         (values `(,e* ... ,e) 0)])
  (Foo->Bar : Foo (e n) -> Foo (n)
        [(fe ,e)
         (values `(fe ,e) 0)]
        [,f
         (values `,bv 0)]
        [,b
         (values `,bv 0)]))

#;(countFooWithN
 (parser-Foo program) 0)

(define-pass countFoo0 : FooLang (e) -> FooLang (n)
  (Expand-Expr : Expr (e m) -> Expr (n)
        [(,[e*] ... ,[e])
         (values `(,e* ... ,e) 0)])
  (Foo->Bar : Foo (e m) -> Foo (n)
        [(fe ,e)
         (values `(fe ,e) 0)]
        [,f
         (values `,bv 0)]
        [,b
         (values `,bv 0)])
  (Expand-Expr e 0))

(countFoo0
 (parser-Foo program))

(define-syntax-rule (first-value expr)
  (call-with-values (λ () expr) (λ (a . _) a)))

(define-syntax-rule (second-value expr)
  (call-with-values (λ () expr) (λ (_ . b) (car b))))

(define-syntax-rule (pair-value expr)
  (call-with-values (λ () expr) (λ (a . b) (cons a b))))

(second-value
 (countFoo0
  (parser-Foo program)))

(define-pass countFooN : FooLang (e) -> * ()
  (definitions
    (define (count-es es acc)
      (cond
        [(null? es)
         acc]
        [else
         (let* ([e0 (car es)]
                [e* (cdr es)]
                [nacc (Expand-Expr e0 acc)])
           (count-es e* nacc))])))
  (Expand-Expr : Expr (e m) -> * ()
        ;[(,[e*] ... ,[e])
        [(,e* ... ,e)
               (let* ([es (append e* (list e))]
                [n (count-es es m)])
           n)]
        [,FF
         (Foo->Bar FF m)])
  (Foo->Bar : Foo (e m) -> * ()
        [(fe ,e)
         (Expand-Expr e m)]
        [,f
         (add1 m)]
        [,b
         m])
  (Expand-Expr e 0))

(countFooN
  (parser-Foo program))

(define-pass countFoo : FooLang (e) -> FooLang (n)
  (definitions
    (define (count-es es acc [nes '()])
      (cond
        [(null? es)
         (cons acc nes)]
        [else
         (let* ([e0 (car es)]
                [e* (cdr es)]
                [r0p (pair-value (Expand-Expr e0 acc))]
                [nes (append nes (list (first r0p)))]
                [nacc (second r0p)])
           (count-es e* nacc nes))])))
  (Expand-Expr : Expr (e m) -> Expr (n)
        [(,e* ... ,e)
         (let* ([es (append e* (list e))]
                [rc (count-es es m)]
                [n (car rc)]
                [nes (cdr rc)])
           (values `(,(drop-right nes 1) ... ,(last nes)) n))]
        [,FF
         (let* ([rp (pair-value (Foo->Bar FF m))])
         (values `,(first rp) (second rp)))])
  (Foo->Bar : Foo (e m) -> Foo (n)
        [(fe ,e)
         (let* ([rf (pair-value (Expand-Expr e m))])
           (values `(fe ,(first rf)) (second rf)))]
        [,f
         (values
          `,(if (odd? m) f bv)
          (add1 m))]
        [,b
         (values `,b m)])
  (Expand-Expr e 0))

(countFoo
  (parser-Foo program))

#;(define-pass countFooCon : FooLang (e) -> FooLang (n)
  (definitions
    (define (count-es es acc [nes '()])
      (cond
        [(null? es)
         (cons acc nes)]
        [else
         (let* ([e0 (car es)]
                [e* (cdr es)]
                [r0p (Expand-Expr e0 acc)]
                [nes (append nes (list (car r0p)))]
                [nacc (cdr r0p)])
           (count-es e* nacc nes))])))
  (Expand-Expr : Expr (e m) -> Expr (n)
        [(,e* ... ,e)
         (let* ([es (append e* (list e))]
                [rc (count-es es m)]
                [n (car rc)]
                [nes (cdr rc)])
           (cons `(,(drop-right nes 1) ... ,(last nes)) n))]
        [,FF
         (let* ([rp (Foo->Bar FF m)])
         (cons `,(car rp) (cdr rp)))])
  (Foo->Bar : Foo (e m) -> Foo (n)
        [(fe ,e)
         (let* ([rf (Expand-Expr e m)])
           (cons `(fe ,(car rf)) (cdr rf)))]
        [,f
         (cons
          `,(if (odd? m) f bv)
          (add1 m))]
        [,b
         (cons `,b m)])
  (Expand-Expr e 0))

#;(countFooCon
  (parser-Foo program))