#lang racket

;###############################################
; part 1 - primitives: atom, eq, car, cdr, cons
; Racket provides car, cdr, cons
;###############################################

(define (atom x)
  (or (symbol? x)
      (null? x)))

; eq is slightly different with paper
(define (eq x y)
  (and (atom x)
       (atom y)
       (eq? x y)))

;########################################
; part 2 - recursive S-function examples
;########################################

(define (ff x)
  (cond [(atom x) x]
        [else (ff (car x))]))

;(ff 'a)
;(ff (cons 'c (cons 'b 'a)))

(define (subst x y z)
  (cond [(atom z)
         (cond [(eq z y) x]
                        [else z])]
        [else
         (cons (subst x y (car z))
                    (subst x y (cdr z)))]))          
                                
;(subst (cons 'x 'a) 'b (cons (cons 'a 'b) 'c))

(define (equal x y)
  (cond [(eq x y) #t] ; slight different
        [else
         (and (not (atom x))
              (not (atom y))
              (equal (car x) (car y))
              (equal (cdr x) (cdr y)))]))

;(equal 'x 'x)
;(equal (cons 'x 'y) 'x)
;(equal (cons 'x 'y) (cons 'x 'y))
;(equal (cons 'x 'y) (cons 'x 'z))

; cadr x := (car (cdr x))
; caddr x := (car (cdr (cdr x)))

;#################################################
; part 3 - list functions (M-functions)
; Racket provides append, and we use '() for 'NIL
;#################################################

(define (null x) ; slightly different since the nil in Racket is '()
  (null? x))

;(append (list 'a 'b) (list 'c 'd 'e))

(define (among x y)
  (and (not (null y))
       (or (equal x (car y))
           (among x (cdr y)))))

(define (pair x y) ; the original paper won't consider list with different lengths, neither does this
  (cond [(and (null x) (null y))
         '()]
        [else #;(and (not (atom x)) (not (atom y)))
         (cons (list (car x) (car y)) (pair (cdr x) (cdr y)))]))

;(pair (list 'a 'b 'c) (list 'x (cons 'y 'z) 'u))

(define (assoc x y)
  (cond [(eq (caar y) x) (cadar y)]
        [else (assoc x (cdr y))]))

#;(assoc 'x
         (list
           (list 'w (list 'a 'b))
           (list 'x (list 'c 'd))
           (list 'y (list 'e 'f))))

;(define zip pair)

(define (sub2 x z)                    ; x is the list of key-value pairs
  (cond [(null x) z]                  ; x is the end of list
        [(eq (caar x) z) (cadar x)]   ; (caar x) is the key of the current pair;
                                      ; if the key is z, return the first element of the value
        [else (sub2 (cdr x) z)]))     ; otherwises, check the next pair

;(sub2 (list (list 'kb (list 'vb1 'vb2)) (list 'ka (list 'va1 'va2)) ) 'ka)

(define (sublis x y)
  (cond [(atom y) (sub2 x y)]
        [else (cons
               (sublis x (car y))
               (sublis x (cdr y)))]))

;(sublis (list (list 'x (list 'a 'b)) (list 'y (list 'b 'c))) (list 'a (cons 'x 'y)))

;######################
; part 4 - interpreter
;######################

;eval-cond-expression
(define (evcon e a)
  (cond [(eval (caar e) a) (eval (cadar e) a)]
        [else (evcon (cdr e) a)]))

;eval-list-of-expressions
(define (evlis m a)
  (cond [(null m) '()]
        [else (cons (eval (car m) a)
                    (evlis (cdr m) a))]))

(define (appq m)
  (cond [(null m) '()]
        [else (cons (list 'QUOTE (car m))
                    (appq (cdr m)))]))

; a helper function to build a lambda-with-args expression then evaluate it
(define (apply f args)
  (eval (cons f (appq args)) '()))

(define (eval e a)
  ;(display "\n exp = ")
  ;(display e)
  ;(display "\n env = ")
  ;(display a)
  ;(display "\n")
  (cond [(atom e) (assoc e a)]
        [(atom (car e))
         (cond [(eq (car e) 'QUOTE)                    (cadr e)]
               [(eq (car e) 'ATOM)                     (atom (eval (cadr e) a))]
               [(eq (car e) 'EQ)                       (eq (eval (cadr e) a)
                                                           (eval (caddr e) a))]
               [(eq (car e) 'COND)                     (evcon (cdr e) a)]
               [(eq (car e) 'CONS)                     (cons (eval (cadr e) a)
                                                             (eval (caddr e) a))]
               [(eq (car e) 'CAR)                      (car (eval (cadr e) a))]
               [(eq (car e) 'CDR)                      (cdr (eval (cadr e) a))]
               [else ; this is for (recursive) function call
                     ; the expression is the recursive part inside the fbody
                     ; the form is like
                     ; (fname fargs ((args init-value) (fname fbody)))
                     ; - fname is the recursive function name
                     ; - fargs is arbitrary expressions
                     ; - (in the env) args binds to initial value
                     ; - fname binds to the recursive function
                     ; e.g. in the FF function
                     ; ψ            = (LABEL ff (LAMBDA (x) (COND ((ATOM x) x) ((QUOTE #t) (ff (CAR x))))))
                     ; a            = ((x (a b)) (ff ψ))
                     ; e            = (ff (CAR x))
                     ; car e        = ff
                     ; cdr e        = (CAR x)
                     ; (assoc ff a) = ψ
                                                       (eval (cons (assoc (car e) a)
                                                                   ;(evlis (cdr e) a)
                                                                   (cdr e)
                                                                   )
                                                             a)])]
        [(eq (caar e) 'LABEL)                          (eval (cons (caddar e) (cdr e))
                                                             (cons (list (cadar e) (car e))
                                                                   a))]
        [(eq (caar e) 'LAMBDA) ; e.g.
                               ; e       = ((LAMBDA (x y) f) (1 2))
                               ; car     =  (LAMBDA (x y) f)
                               ; cdr     =                   (1 2)
                               ; cdar    =         ((x y) f)
                               ; cadar   =          (x y)
                               ; cddar   =               (f)
                               ; caddar  =                f
                               ; => (eval f new-env)
                               ; new-env = (append (pair (x y) (evlis (1 2) a) a)
                               ;         => (append (pair (x y) (1 2)) a)
                               ;         => (append (x 1) (y 2) a)
                               ; => (eval f (append (x 1) (y 2) a))
                                                       (eval (caddar e)
                                                             (append (pair (cadar e)
                                                                           (evlis (cdr e) a))
                                                                     a))]
        [(eq (caar e) 'λ)                              (eval (caddar e)
                                                             (append (pair (cadar e)
                                                                           (evlis (cdr e) a))
                                                                     a))]
        [else (raise 'failed #t)]
        #;[else (eval (cons (eval (car e) a)
                          (cdr e))
                    a)]
))

;(eval 'x `((x ,1)))
;(eval `(QUOTE ,#t) `())
;(eval `(ATOM x) `((x x)))
;(eval `(ATOM x) `((x ,1)))
;(eval `(CONS (QUOTE ,#t) (QUOTE ,#f)) `())
;(eval `(EQ x x) '((x x)))
;(eval `(EQ (QUOTE x) (QUOTE x)) `())
;(eval `(EQ x (CAR (CONS x x))) `((x x)))
;(eval `(EQ x (CDR (CONS x x))) `((x x)))
;(eval `(CAR (CONS (QUOTE ,#t) (QUOTE ,#f))) `())
;(eval `(CDR (CONS (QUOTE ,#t) (QUOTE ,#f))) `())
;(eval `(CDR (CDR (CONS (QUOTE ,#t) (CONS (QUOTE ,#t) (QUOTE ,#f))))) `())
;(eval `(CDR (CDR (CONS (QUOTE ,#t) (CONS (QUOTE ,#t) x)))) `((x x)))
;(eval `(COND ((QUOTE ,#f) (QUOTE ,#f)) ((QUOTE ,#t) (QUOTE ,#t))) `())
;(eval `((LAMBDA (x) x) (QUOTE y)) `())

;#########################
; part 5 - paper examples
;#########################

;(apply `(LAMBDA (x y) (CONS (CAR x) y)) `((a b) (c d)))

; maplist with f as a function returning dummy - using Racket list
#;(apply (list 'LABEL 'maplist
             (list 'LAMBDA (list 'x 'f)
                   (list 'COND
                         (list (list 'ATOM 'x) 'x)
                         (list (list 'QUOTE #t) (list 'CONS (list 'f (list 'CAR 'x))
                                                            (list 'maplist (list 'CDR 'x) 'f))))))
       (list (list 'a 'b)
             (list 'LAMBDA (list 'x)
                   (list 'QUOTE 'dummy))))

; maplist with f as a function returning dummy - using Racket quasiquote
#;(apply `(LABEL maplist
               (LAMBDA (x f)
                       (COND
                        ((ATOM x) x)
                        ((QUOTE ,#t) (CONS (f (CAR x))
                                           (maplist (CDR x) f))))))
       '((a b)
         (LAMBDA (x)
                 (QUOTE dummy))))

; maplist with f as a function returning (CAR x) - using Racket list
#;(apply (list 'LABEL 'maplist
             (list 'LAMBDA (list 'x 'f)
                   (list 'COND
                         (list (list 'ATOM 'x) 'x)
                         (list (list 'QUOTE #t) (list 'CONS (list 'f (list 'CAR 'x))
                                                            (list 'maplist (list 'CDR 'x) 'f))))))
       (list (list (list 'ka 'va) (list 'kb 'vb))
             (list 'LAMBDA (list 'x)
                   (list 'CAR 'x))))

; maplist with f as a function returning (CAR x) - using Racket quasiquote
#;(apply `(LABEL maplist
               (LAMBDA (x f)
                       (COND
                        ((ATOM x) x)
                        ((QUOTE ,#t) (CONS (f (CAR x))
                                           (maplist (CDR x) f))))))
       '(((ka vb) (kb vb))
         (LAMBDA (x)
                 (CAR x))))

; maplist with f as a function returning (CAR x) - using Racket list
#;(apply (list 'LABEL 'maplist
             (list 'LAMBDA (list 'x 'f)
                   (list 'COND
                         (list (list 'ATOM 'x) 'x)
                         (list (list 'QUOTE #t) (list 'CONS (list 'f (list 'CAR 'x))
                                                            (list 'maplist (list 'CDR 'x) 'f))))))
       (list (list (list 'ka 'va) (list 'kb 'vb))
             (list 'LAMBDA (list 'x)
                   (list 'CAR (list 'CDR 'x)))))

; maplist with f as a function returning (CAR x) - using Racket quasiquote
#;(apply `(LABEL maplist
               (LAMBDA (x f)
                       (COND
                        ((ATOM x) x)
                        ((QUOTE ,#t) (CONS (CAR x)
                                           (maplist (CDR x) f))))))
       '(((ka vb) (kb vb))
         (LAMBDA (x)
                 (CAR (CDR x)))))

;##################
; part 6 - scoping
;##################

; (\x -> x) y => #t
;(eval '((LAMBDA (x) x) y) '((y #t)))

; (\x -> x) "y" => "y"
;(eval '((LAMBDA (x) x) (QUOTE y)) '())

; an error case
; (\x -> (\z -> x)) "_" =\=>
;;;;(eval '((LAMBDA (x) (LAMBDA (z) x)) (QUOTE _)) '())

; (\x -> "\z -> x") "_" => "\z -> x"
;(eval '((LAMBDA (x) (QUOTE (LAMBDA (z) x))) (QUOTE _)) '())

; ((\x -> "\z -> x") "_") "_"
;  => "\z -> x" "_"
;  => (\z -> x) "_"
;  => x
;  => 
;(eval (list (eval '((LAMBDA (x) (QUOTE (LAMBDA (z) x))) (QUOTE _)) '()) '(QUOTE _)) '((x #t)))

;######################
; part 7 - lambda term?
;######################

; (\x -> ((\y -> cons x y) "right")) "left"
#;(eval '((λ (x)
          ((λ (y)
             (CONS x y))
           (QUOTE right)))
        (QUOTE left))
      '())

; incorrect
; ((\x -> (\y -> cons x y)) "left") "right"
#;(eval '(((λ (x)
          (λ (y)
            (CONS x y)))
         (QUOTE left))
        (QUOTE right))
      '())

; incorrect
; ((\x -> (\y -> cons x y)) "left")
#;(eval '((λ (x)
          (λ (y)
            (CONS x y)))
        (QUOTE left))
      '())

; (\f -> "42") id
#;(eval '((λ (f)
          (QUOTE 42))
        (QUOTE id))
      '())


; incorrect
; (\f -> "42") (\x -> x)
#;(eval '((λ (f)
          (QUOTE 42))
        (λ (x) x))
      '())

; (\f -> "42") "(\x -> x)"
#;(eval '((λ (f)
          (f (QUOTE 42)))
          (QUOTE (λ (x) x)))
      '())

; incorrect
; (\f -> f "42") ((\x -> (\y -> cons x y)) "hello")
#;(eval '((λ (f)
          (f (QUOTE 42)))
        ((λ (x)
          (λ (y)
            (CONS x y)))
         (QUOTE hello)))
      '())


; (\f -> f "42") ((\x -> "(\y -> cons x y)") "hello")
#;(eval '((λ (f)
          (f (QUOTE 42)))
        ((λ (x)
           (QUOTE (λ (y)
                    (CONS x y))))
         (QUOTE hello)))
      '((x (QUOTE fix-x))))