; 
; Code snippets from "The Little Schemer", 4th edition, by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 08) Lambda the Ultimate) 
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

; Define rember-f
;
; Takes a test function, an element and a list 
; as arguments, and removes the first element for
; which the test succeeds.
;
(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else (cons (car l) (rember-f test? a (cdr l)))))))
;
(rember-f = '5 '(6 2 5 3))
(rember-f eq? 'jelly '(jelly beans are good))
(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))

; Note:
;
; (lambda (a)
;   (lambda (x)
;     (eq? x a))) 
; This is a function that, when passed an argument a, 
; returns the function 
;   (lambda (x)
;     (eq? x a)), where a is just that argument.
;
; This is called "Currying".
;

; Define eq?-c
;
; Takes an atom and returns a function that tests
; if its argument is equal to that atom.
;
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))
;
(eq?-c 'salad)  ; Returns a function
(define eq?-salad (eq?-c 'salad))  ; Giving that function a name
(eq?-salad 'salad)
(eq?-salad 'tuna)
((eq?-c 'salad) 'salad)
((eq?-c 'salad) 'tuna)

; Define rember-f (curried)
;
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))
;
((rember-f eq?) 'tuna '(shrimp salad and tuna salad))
((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))

; Define insertL-f
;
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old) (cons new (cons old (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

; Define insertR-f
;
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

; Define seqL
;
; conses new onto the cons of old onto l.
;
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

; Define seqR
;
; conses old onto the cons of new onto l.
;
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

; Define insert-g
;
; Takes a function as an argument (seqL or seqR),
; and returns the corresponding insert
; function (insertL or insertR respectively).
;
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

; Define insertL (using insert-g)
;
(define insertL (insert-g seqL))

; Define insertR (using insert-g)
;
(define insertR (insert-g seqR))

; Define insertL (using insert-g, without naming seqL)
;
(define insertL
  (insert-g
    (lambda (new old l)
      (cons new (cons old l)))))

; Define seqS
;
; conses new onto l. Ignores old.
;
(define seqS
  (lambda (new old l)
    (cons new l)))

; Define subst (using insert-g)
;
; Replaces the first occurrence of new by old.
;
(define subst (insert-g seqS))

; Define seqrem
;
(define seqrem
  (lambda (new old l)
    l))

; Define rember-f (using insert-g)
;
(define rember-f
  (lambda (a l)
    ((insert-g seqrem) #f a l)))
;
(rember-f 'sausage '(pizza with sausage and bacon))

;
; ###############################################
;
;              The Ninth Commandment
;
;  Abstract common patterns with a new function.
;
; ###############################################
;

; Define 1st-sub-exp (from chapter 06)
;
; Returns exp1 in (op exp1 exp2)
; 
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

; Define 2nd-sub-exp (from chapter 06)
; 
; Returns exp2 in (op exp1 exp2)
;
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

; Define operator (from chapter 06)
;
; Returns op in (op exp1 exp2) 
;
(define operator
  (lambda (aexp)
    (car aexp)))

; Define atom-to-function
;
; Takes an atom (+, x or ^) and returns
; the corresponding function.
;
(define atom-to-function
  (lambda (op)
    (cond
      ((eq? op '+) +)
      ((eq? op 'x) *)
      (else expt))))

; Define value-prefix (from chapter 06; using atom-to-function)
;
(define value-prefix
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else 
        ((atom-to-function (operator nexp))
          (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp)))))))
;
(value-prefix '(x (+ 3 1) (^ 5 2)))

; Define multirember-f
;
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))
;
((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))

; Define multirember-eq?
;
(define multirember-eq? (multirember-f eq?))

; Define eq?-tuna
;
(define eq?-tuna
  (eq?-c 'tuna))

; Define multiremberT
;
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))
;
(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))

; Define multirember_co (using collector function)
;
; Takes three arguments: atom a, lat, and function col.
; Looks at every atom of the lat to see if it is eq? to a.
; Those atoms that are not are collected in one list ls1; 
; the others for which the answer is true are collected 
; in a second list ls2. Finally, it determines the value 
; of (col ls1 ls2).
;
;
; Note:
;
; col stands for "collector" (sometimes called as 
; continuation).
;
(define multirember_co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? (car lat) a)
        (multirember_co a (cdr lat) 
          (lambda (newlat seen)
            (col newlat (cons (car lat) seen)))))
      (else 
        (multirember_co a (cdr lat)
          (lambda (newlat seen)
            (col (cons (car lat) newlat) seen)))))))

; Define a-friend
;
(define a-friend
  (lambda (x y)
    (null? y)))

; Define last-friend
;
(define last-friend
  (lambda (x y)
    (length x)))

; Define ls1
;
(define ls1
  (lambda (x y)
    x))

; Define ls2
;
(define ls2
  (lambda (x y)
    y))

;
(multirember_co 'tuna '() a-friend)                                     ; #t
(multirember_co 'tuna '(tuna) a-friend)                                 ; #f
(multirember_co 'tuna '(strawberries tuna and swordfish) a-friend)      ; #f
(multirember_co 'tuna '(strawberries shrimp and swordfish) a-friend)    ; #t
(multirember_co 'tuna '(strawberries tuna and swordfish) last-friend)   ; 3
(multirember_co 'tuna '(strawberries shrimp and swordfish) last-friend) ; 4
(multirember_co 'tuna '(strawberries tuna and swordfish) ls1)           ; (strawberries and swordfish)
(multirember_co 'tuna '(strawberries tuna and swordfish) ls2)           ; (tuna)

;
; ###############################################
;
;               The Tenth Commandment
;
;     Build functions to collect more than one 
;     value at a time.
;
; ###############################################
;

; Define multiinsertLR
;
; Inserts new to the left of oldL and
; to the right of oldR in lat if oldL
; and oldR are different.
;
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

; Define multiinsertLR_co (using collector function)
;
; Similar to multiinsertLR, except that it has an additional
; argument col, which is a collector function that takes
; three arguments - newlat (the list of atoms after performing
; multiinsertLR), L (number of left insertions), 
; R (number of right insertions).
;
(define multiinsertLR_co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL)
        (multiinsertLR_co new oldL oldR (cdr lat)
          (lambda (newlat L R)
            (col (cons new (cons oldL newlat)) (+ L 1) R))))
      ((eq? (car lat) oldR)
        (multiinsertLR_co new oldL oldR (cdr lat)
          (lambda (newlat L R)
            (col (cons oldR (cons new newlat)) L (+ R 1)))))
      (else 
        (multiinsertLR_co new oldL oldR (cdr lat)
          (lambda (newlat L R)
            (col (cons (car lat) newlat) L R)))))))
;
(multiinsertLR_co 'cranberries 'fish 'chips '() 
  (lambda (newlat L R) newlat))  ; ()
(multiinsertLR_co 'cranberries 'fish 'chips '()
  (lambda (newlat L R) L))       ; 0
(multiinsertLR_co 'cranberries 'fish 'chips '()
  (lambda (newlat L R) R))       ; 0
(multiinsertLR_co 'salty 'fish 'chips
  '(chips and fish or fish and chips)
  (lambda (newlat L R) newlat))  ; (chips salty and salty fish or salty fish and chips salty)
(multiinsertLR_co 'salty 'fish 'chips
  '(chips and fish or fish and chips)
  (lambda (newlat L R) L))       ; 2
(multiinsertLR_co 'salty 'fish 'chips
  '(chips and fish or fish and chips)
  (lambda (newlat L R) R))       ; 2

; Define even?
;
(define even?
  (lambda (n)
    (= (* (/ n 2) 2) n)))

; Define evens-only*
;
; Removes all odd numbers from a list of
; nested lists.
;
(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
          (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))
;
(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

; Define evens-only*_co (using collector function)
;
; Builds a nested list of even numbers by
; removing odd ones from its argument and
; simultaneously multiplies the even numbers
; and sums up the odd numbers that occur in its
; argument.
;
(define evens-only*_co
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l))
        (cond
          ((even? (car l))
            (evens-only*_co (cdr l)
              (lambda (newl p s)
                (col (cons (car l) newl) (* p (car l)) s))))
          (else 
            (evens-only*_co (cdr l)
              (lambda (newl p s)
                (col newl p (+ s (car l))))))))
      (else (evens-only*_co (car l)
              (lambda (al ap as)
                (evens-only*_co (cdr l)
                  (lambda (dl dp ds)
                    (col (cons al dl) (* ap dp) (+ as ds))))))))))

; Define the-last-friend
;
(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

;
(evens-only*_co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) 
   the-last-friend)   ; (38 1920 (2 8) 10 (() 6) 2)
