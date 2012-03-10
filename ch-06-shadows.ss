; 
; Code snippets from "The Little Schemer", 4th edition, by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 06) Shadows)
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Define atom?
; 
; True if S-expression is an atom.
; 
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Define numbered?
;
; Determines whether a representation
; of an arithmetic expression contains
; only numbers besides the +, x and ^.
;
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) '+)  
        (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'x)  
        (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '^)  
        (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))
; Define numbered? (simplified)
;
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))
;
(numbered? '(n + 3))             ; #f
(numbered? '(3 + (4 x 5)))       ; #t
; (numbered? '(3 + 4 x 5))       ; Chain of operations not allowed. Paranthesis required.
(numbered? '(3 + (4 ^ 5)))       ; #t
(numbered? '(2 x sausage))       ; #f
(numbered? '((3 + 4) ^ (5 x 3))) ; #t

; Define add1
;
(define add1
  (lambda (n)
    (+ n 1)))

; Define sub1
;
(define sub1
  (lambda (n)
    (- n 1)))

; Define o+
; 
; Adds two non-negative integers.
;
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

; Define x
; 
; Non-negative integer multiplication
;
(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

; Define ^
;
(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (^ n (sub1 m)))))))

; Define value
;
; Evaluates the arithmetic expression (only
; those expressions that would pass numbered?)
;
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+) 
        (o+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) 'x)
        (x (value (car nexp)) (value (car (cdr (cdr nexp))))))
      (else
        (^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))
;
(value '13)
(value '(1 + 3))
(value '(1 + (3 ^ 4)))
(value '(1 + (3 x 4)))
(value '((2 + 1) x (2 ^ 3)))

;
; ###############################################
;
;             The Seventh Commandment
;
;   Recur on the subparts that are of the same
;   nature:
;   * On the sublists of a list.
;   * On the subexpressions of an arithmetic
;     expression.
;
; ###############################################
;

; Define 1st-sub-exp
;
; Returns exp1 in (op exp1 exp2)
;
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

; Define 2nd-sub-exp
;
; Returns exp2 in (op exp1 exp2)
;
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

; Define operator
;
; Returns op in (op exp1 exp2)
;
(define operator
  (lambda (aexp)
    (car aexp)))

; Define value-prefix
;
; Evaluates a prefix arithmetic expression.
;
(define value-prefix
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
        (o+ (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'x)
        (x (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp))))
      (else
        (^ (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp)))))))
;
(value-prefix '(+ 3 4))
(value-prefix '(+ (x 3 6) (^ 8 2)))

; 
; ###############################################
;
;              The Eighth Commandment
;
;        Use help functions to abstract from 
;        representations.
;
; ###############################################
;

; Note:
;
; Truth-values and numbers are representations.
;

; Numbers using different representation.
;
; () - 0
; (()) - 1
; (() ()) - 2
; etc.
;

; Define sero?
;
; Just like zero?
;
(define sero?
  (lambda (n)
    (null? n)))

; Define edd1
;
; Just like add1
;
(define edd1
  (lambda (n)
    (cons '() n)))

; Define zub1
;
; Just like sub1
;
(define zub1
  (lambda (n)
    (cdr n)))

; Define oo+
;
; Just like o+
;
(define oo+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (oo+ n (zub1 m)))))))
;
(oo+ '(() () ()) '(() ()))

; Beware of shadows.
