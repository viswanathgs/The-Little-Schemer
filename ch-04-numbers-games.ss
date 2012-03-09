; 
; Code snippets from "The Little Schemer", 4th edition, by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 04) Numbers Games)
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
;
(atom? 14)
(atom? -3)
(atom? 3.14159)

; Define add1
;
(define add1
  (lambda (n)
    (+ n 1)))
;
(add1 67)

; Define sub1
;
(define sub1
  (lambda (n)
    (- n 1)))
;
(sub1 5)
(sub1 0)

; zero?
;
(zero? 0)

; Define o+
;
; Adds two non-negative integers.
;
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))
;
(o+ 46 12)

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
;
(o- 14 3)
(o- 17 9)
(o- 18 25)

; Tuples
;
'(2 11 3 79 47 6)     ; This is a tup (tuple)
'(8 55 5 555)         ; Tup
'( 1 2 8 apple 4 3)   ; Not a tup; Just a list of atoms
'(3 (7 4) 13 9)       ; Not a tup; (7 4) is not a number
'()                   ; Empty tup

; Define addtup
;
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))
;
(addtup '(3 5 2 8))

; 
; ###############################################
;
;              The First Commandment
;
;                 (first revision)
;
;      When recurring on a list of atoms, lat,
;      ask two questions about it: (null? lat)
;      and else.
;      When recurring on a number, ask two 
;      questions about it: (zero? n) and else.
;
; ###############################################
;

; Define x
;
; Non-negative integer multiplication
;
(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))
;
(x 5 3)
(x 13 4)
(x 12 3)

;
; ###############################################
;
;              The Fourth Commandment
;
;                 (first revision)
;
;    Always change at least one argument while
;    recurring. It must be changed closer to 
;    termination. The changing argument must be
;    tested in the termination condition:
;    when using cdr, test termination with null?
;    and when using sub1, test termination with
;    zero?.
;
; ###############################################
;

; 
; ###############################################
;
;              The Fifth Commandment
;
;    When building a value with o+, always use
;    0 for the value of the terminating line,
;    for adding 0 does not change the value of
;    an addition.
;
;    When building a value with x, always use 1
;    for the value of the terminating line, for
;    multiplying by 1 does not change the value
;    of a multiplication.
;
;    When building a value with cons, always 
;    consider () for the value of the terminating
;    line.
;
; ###############################################
;

; Define tup+
;
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
;
(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
(tup+ '(2 3) '(4 6))
(tup+ '(3 7) '(4 6))
(tup+ '(3 7 8 1) '(4 6))

; Define o>
;
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))
;
(o> 12 133)
(o> 120 11)
(o> 3 3)

; Define o<
;
(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))
;
(o< 4 6)
(o< 8 3)
(o< 6 6)

; Define o=
;
(define o=
  (lambda (n m)
    (cond
      ((zero? n) (zero? m))
      ((zero? m) #f)
      (else (o= (sub1 n) (sub1 m))))))
; Define o= (using o< and o>)
;
(define o=
  (lambda (n m)
    (cond
      ((or (o< n m) (o> n m)) #f)
      (else #t))))

; Define ^
;
(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (^ n (sub1 m)))))))
;
(^ 1 1)
(^ 2 3)
(^ 5 3)

; Define o/
;
(define o/
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))
;
(o/ 15 4)

; Define len
;
; Counts the number of S-expressions 
; in lat.
;
(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))
;
(len '(ham and cheese on rye))

; Define pick
;
; Picks the n-th (1-based) S-expression 
; in lat.
;
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
;
(pick '4 '(lasagna spaghetti ravioli macaroni meatball))

; Define rempick
;
; Removes the n-th (1-based) S-expression
; in lat.
;
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
;
(rempick '3 '(hotdogs with hot mustard))

; number?
;
(number? 'tomato)
(number? 76)

; Define no-nums
;
; Removes all numbers from lat.
;
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))
;
(no-nums '(5 pears 6 prunes 9 dates))

; Define all-nums
;
; Extracts a tup from a lat using 
; all the numbers in the lat.
;
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

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

; Define occur
;
; Counts the number of times an atom
; a appears in a lat.
;
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

; Define one?
;
; True if n is 1.
;
(define one?
  (lambda (n)
    (cond
      (else (o= n 1)))))

; Define one? (without cond)
;
(define one?
  (lambda (n)
    (o= n 1)))

; Define rempick (using one?)
;
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
