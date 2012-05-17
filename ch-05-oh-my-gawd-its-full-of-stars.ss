; 
; Code snippets from "The Little Schemer", 4th edition, by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 05) "Oh My Gawd*: It's Full of Stars)
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

; Define rember*
;
; Recursively removes atom a from lat.
;
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l)) 
        (cond
          ((eq? (car l) a) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))
;
(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

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
(lat? '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
(atom? (car '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))))

; Define insertR*
;
; Recursively inserts new to the right of
; every occurrence of old in list l.
;
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) 
        (cond
          ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
          (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))
;
(insertR* 'roast 'chuck '((how much (wood)) 
                         could 
                         ((a (wood) chuck)) 
                         (((chuck))) 
                         (if (a) ((wood chuck))) 
                         could chuck wood))

;
; ###############################################
;
;               The First Commandment
;
;                  (final version)
;
;   When recurring on a list of atoms, lat, ask
;   two questions about it: (null? lat) and else.
;   When recurring on a number, n, ask two 
;   questions about it: (zero? n) and else.
;   When recurring on a list of S-expressions, l,
;   ask three questions about it: (null? l),
;   (atom? (car l)), and else.
;
; ###############################################
;

;
; ###############################################
;
;               The Fourth Commandment
;
;                  (final version)
;
;   Always change at least one argument while
;   recurring. When recurring on a list of atoms,
;   lat, use (cdr lat). When recurring on a 
;   number, n, use (sub1 n). And when recurring
;   on a list of S-expressions, l, use (car l)
;   and (cdr l) if neither (null? l) nor 
;   (atom? (car l)) are true.
;
;   It must be changed to be closer to 
;   termination. The changing argument must be
;   tested in the termination condition:
;
;   when using cdr, test termination with null?
;   and when using sub1, test termination with
;   zero?.
;
; ###############################################
;

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

; Define o-
; 
; Subtracts m from n, where n and
; m are non-negative integers.
; 
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

; Define occur*
;
; Recursively counts the number of
; occurrences of atom a in l.
;
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) 
        (cond
          ((eq? (car l) a) (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))
;
(occur* 'banana '((banana)
                  (split ((((banana ice)))
                          (cream (banana))
                          sherbet))
                  (banana)
                  (bread)
                  (banana brandy)))

; Define subst*
;
; Recursively replaces all occurrences 
; of old by new in l.
;
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) 
        (cond
          ((eq? (car l) old) (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))
;
(subst* 'orange 'banana '((banana)
                          (split ((((banana ice)))
                                  (cream (banana))
                                  sherbet))
                          (banana)
                          (bread)
                          (banana brandy)))

; Define insertL*
;
; Recursively inserts new to the left 
; of all occurrences of old in l.
;
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) 
        (cond
          ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
          (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

; Define member*
;
; Recursively checks if atom a is a 
; member of l.
;
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eq? (car l) a) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))
;
(member* 'chips '((potato) (chips ((with) fish) (chips))))

; Define leftmost
;
; Finds the leftmost atom in a non-empty
; list of S-expressions that does not
; contain the empty list.
;
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
;
(leftmost '((potato) (chips ((with) fish) (chips))))
(leftmost '(((hot) (tuna (and))) cheese))

; and
;
(and (atom? (car '(mozzarella pizza))) (eq? (car '(mozzarella pizza)) 'pizza))
(and (atom? (car '((mozzarella mushroom) pizza))) (eq? (car '((mozzarella mushroom) pizza)) 'pizza))
(and (atom? (car '(pizza (tastes good)))) (eq? (car '(pizza (tastes good))) 'pizza))

; Note:
;
; (and a b) = (cond (a b) (else #f))
; (or a b)  = (cond (a #t) (else b))
;

; Define o=
;
(define o=
  (lambda (n m)
    (cond
      ((zero? n) (zero? m))
      ((zero? m) #f)
      (else (o= (sub1 n) (sub1 m))))))

; Define eqan?
;
; True if a1 and a2 are the same atom.
;
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

; Define eqlist?
;
; Checks if two lists of S-expressions
; are equal.
;
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2))) 
        (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else 
        (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))
;
(eqlist? '(strawberry ice cream) '(strawberry ice cream))
(eqlist? '(strawberry ice cream) '(strawberry cream ice))
(eqlist? '(banana ((split))) '((banana) (split)))
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))

; Define equal?
;
; Checks if two S-expressions (an 
; S-expression is either an atom or
; a list of S-expressions) are equal.
;
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

; Define eqlist? (using equal?)
;
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

;
; ###############################################
;
;              The Sixth Commandment
;
;   Simplify only after the function is correct.
;
; ###############################################
;

; Define rember
;
; Removes S-expression s from list l.
;
(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))
