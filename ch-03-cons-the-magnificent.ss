; 
; Code snippets from "The Little Schemer", 4th edition, by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 03) Cons the Magnificent)
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

;
; ###############################################
;
;              The Second Commandment
;
;              Use cons to build lists.
;
; ###############################################
;

; Define rember
; 
; Removes the first occurrence of atom a from 
; the list lat.
;
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
              (rember a (cdr lat)))))))
;
(rember 'mint '(lamb chops and mint jelly))
(rember 'mint '(lamb chops and mint flavored mint jelly))
(rember 'toast '(bacon lettuce and tomato))
(rember 'cup '(coffee cup tea cup and hick cup))
(rember 'and '(bacon lettuce and tomato))
(rember 'sauce '(soy sauce and tomato sauce))

; Define firsts
;
; Takes a list that is either null or contains
; only non-empty lists as an argument.
; Builds a list composed of the first S-expression
; of each internal list.
;
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))
;
(firsts '((apple peach pumpkin) 
          (plum pear cherry) 
          (grape raisin pea) 
          (bean carrot eggplant)))
(firsts '((a b) (c d) (e f)))
(firsts '())
(firsts '((five plums) (four) (eleven green oranges)))
(firsts '(((five plums) four) (eleven green oranges) ((no) more)))

;
; ###############################################
;
;              The Third Commandment
;
;     When building a list, describe the first 
;     typical element, and then cons it onto 
;     the natural recursion.
;
; ###############################################
;

; Define insertR
;
; It takes three arguments: the atoms new
; and old, and a lat. The function insertR
; builds a lat with new inserted to the
; right of the first occurrence of old.
;
(define insertR
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))
;
(insertR 'topping 'fudge '(ice cream with fudge for dessert))
(insertR 'jalapeno 'and '(tacos tamales and salsa))
(insertR 'e 'd '(a b c d f g d h))

; Define insertL
;
; Inserts the atom new to the left of the
; first occurrence of the atom old in lat.
;
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

; Define subst
;
; Replaces the first occurrence of old in
; the lat with new.
;
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))
;
(subst 'topping 'fudge '(ice cream with fudge for dessert))

; Define subst2
;
; Replaces either the first occurrence of 
; o1 or the first occurrence of o2 by new.
;
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))
;
(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

; Define multirember
;
; Removes all occurrences of atom a from 
; list lat.
;
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))
;
(multirember 'cup '(coffee cup tea cup and hick cup))

; Define multiinsertR
;
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

; Define multiinsertL
;
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

;
; ###############################################
;
;              The Fourth Commandment
;
;                   (preliminary)
;
;    Always change at least one argument while
;    recursing. It must be changed to be closer 
;    to termination. The changing argument must
;    be tested in the termination condition: 
;    when using cdr, test termination with null?.
;
; ###############################################
;

; Define multisubst
;
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))
