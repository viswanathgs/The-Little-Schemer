; 
; Code snippets from "The Little Schemer", 4th edition, by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 02) Do It, Do It Again, and Again, and Again...)
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

; Define lat?
;
; True if S-expression is a list of atoms.
;
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
;
(lat? '(Jack Sprat could eat no chicken fat))
(lat? '((Jack) Sprat could eat no chicken fat))
(lat? '(Jack (Sprat could) eat no chicken fat))
(lat? '())
(lat? '(bacon and eggs))
(lat? '(bacon (and eggs)))

; Note:
;
; (cond ...) asks questions;
; (lambda...) creates a function; and
; (define...) gives it a name.
;

; Examples for or
;
(or (null? '()) (atom? '(d e f g)))
(or (null? '(a b c)) (null? '()))
(or (null? '(a b c)) (null? '(atom)))

; Define member?
;
; True if a is a member of lat.
;
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) 
              (member? a (cdr lat)))))))
; 
(member? 'tea '(coffee tea or milk))
(member? 'poached '(fried eggs and scrambled eggs))
(member? 'meat '(mashed potatoes and meat gravy))
(member? 'liver '(bagels and lox))

; Note:
;
; else is a question whose value is always true.
;

;
; ###############################################
;
;             The First Commandment
;
;                 (preliminary)
;
;     Always ask null? as the first question
;     in expressing any function.
;
; ###############################################
;

;
; ###############################################
;
;            This space for doodling
;
; ###############################################
;
