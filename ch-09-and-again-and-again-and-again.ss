; 
; Code snippets from "The Little Schemer", 4th edition, by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 09) ...and Again, and Again, and Again,...) 
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

; Define pick (from chapter 04)
;
; Picks the n-th (1-based) S-expression 
; in lat.
;
(define pick
  (lambda (n lat)
    (cond
      ((zero? (- n 1)) (car lat))
      (else (pick (- n 1) (cdr lat))))))

; Define looking
;
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

; Define keep-looking
;
; If sorn (symbol or number) is a number,
; then recursion continues with sorn as the
; sorn-th element in lat. If sorn is an atom,
; it checks if it is eq? to a.
;
(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

;
(looking 'caviar '(6 2 4 caviar 5 7 3))      ; #t
(looking 'caviar '(6 2 grits caviar 5 7 3))  ; #f
; (looking 'caviar '(7 2 4 7 5 6 3))         ; Never terminates...
; (looking 'caviar '(7 1 2 caviar 5 6 3))    ; Never terminates...

; Note:
;
; keep-looking does not recur on a part of lat.
; This is called "unnatural" recursion.
; Moreover, it may never terminate. 
; Functions like these are called "partial functions".
; Functions we have seen so far are "total functions".
;

; Define eternity
;
; This is the most partial function.
; This is the most unnatural recursion since for 
; none of its arguments eternity reaches its goal.
;
(define eternity
  (lambda (x)
    (eternity x)))

; Define first (from chapter 07)
; 
; First of a pair.
;
(define first
  (lambda (p)
    (car p)))

; Define second (from chapter 07)
;
; Second of a pair.
;
(define second
  (lambda (p)
    (car (cdr p))))

; Define build (from chapter 07)
; 
; Builds a pair from two S-expressions.
;
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

; Define shift
; 
; Takes a pair whose first component is a pair and
; builds a pair by shifting the second part of the 
; first component into the second component.
;
(define shift
  (lambda (pair)
    (build (first (first pair))
      (build (second (first pair)) (second pair)))))
;
(shift '((a b) c))     ; (a (b c))
(shift '((a b) (c d))) ; (a (b (c d)))

; Define align
;
; Note:
;   The argument to align in the second line under
;   cond is (shift pora), which does not reduce 
;   the length of the atoms (violating The Seventh
;   Commandment). The function still terminates for
;   all arguments (i.e., align is a total function).
;
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((pair? (first pora)) (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))
;
(align '(a b))                     ; (a b)
(align '((a b) c))                 ; (a (b c))
(align '((a b) (c d)))             ; (a (b (c d)))
(align '((a (b c)) (((d e) f) g))) ; (a (b (c (d (e (f g))))))

; Define length*
;
; Counts the number of atoms in align's arguments.
;
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora)) (length* (second pora)))))))

; Define weight* 
;
; Paying twice the attention to the first component
; that length*. This is more suitable than using length* 
; to analyze align's arguments.
;
(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* (weight* (first pora)) 2) (weight* (second pora)))))))
;
(weight* '((a b) c)) ; 7
(weight* '(a (b c))) ; 5

; Define revpair (from chapter 07)
; 
; Reverses a pair.
;
(define revpair
  (lambda (p)
    (build (second p) (first p))))

; Define shuffle 
;
; Like align, but uses revpair instead of shift.
;
; The functions shuffle and revpair swap the components
; of pairs when the first component is a pair.
;
; Note:
;   shuffle is not a total function.
;
(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))
;
(shuffle '(a (b c)))       ; (a (b c))
(shuffle '(a b))           ; (a b)
; (shuffle '((a b) (c d))) ; Never terminates...

; Define C
;
; Collatz conjecture. Check wikipedia. 
; The function always reaches 1 for a positive n.
; Hence, it is a total function.
;
(define C
  (lambda (n)
    (cond
      ((= n 1) 1)
      ((even? n) (C (/ n 2)))
      (else (C (+ (* 3 n) 1))))))

; Define A
;
; Ackermann function. Check wikipedia.
;
(define A
  (lambda (m n)
    (cond
      ((= m 0) (+ n 1))
      ((= n 0) (A (- m 1) 1))
      (else (A (- m 1) (A m (- n 1)))))))
;
(A 1 0)   ; 2
(A 1 1)   ; 3
(A 2 2)   ; 7
; (A 4 3) ; I dare you to execute this.

; Define will-stop?
;
; Allegedly returns #t if the function passed to it
; as the argument stops when applied to '(), #f otherwise.
;
; (define will-stop?
;   (lambda (f)
;     ...))
;
; will-stop? is total as it always returns #t or #f.
;
; (will-stop? length)   ; #t as (length '()) return 0.
; (will-stop? eternity) ; #f as (eternity '()) doesn't return a value.
;
; (define last-try
;   (lambda (x)
;     (and (will-stop? last-try) (eternity x))))
;
; (will-stop? last-try) ; What is the result?
;
; Let's say (will-stop? last-try) is #f.
; But, (last-try '()) will stop as (and #f (eternity '())) is #f. 
; This means (will-stop? last-try) cannot be #f.
;
; If (will-stop? last-try) is #t, then (eternity '()) will
; get executed, which means that last-try would not stop.
; Hence, (will-stop? last-try) cannot be #t also.
;
; will-stop? cannot be defined.
;
; Refer Turing's halting problem and Godel's incompleteness theorems.
;

; <awesomeness>

; Y-Combinator:
;
; A clever construct that allows an anonymous function to call itself,
; i.e., it allows anonymous function recursion.
;
; The rest of this chapter is Y-Combinator derivation.
;

; For reference's sake, define len.
;
(define len
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 (len (cdr l)))))))

; The following function determines the length
; of the empty list and nothing else. Note that 
; the function is anonymous.
;
(lambda (l)
  (cond
    ((null? l) 0)
    (else (+ 1 (eternity (cdr l))))))

; Anonymous function that determines the length of
; lists that contain <= 1 element.
;
(lambda (l)
  (cond
    ((null? l) 0)
    (else 
      (+ 1
        ((lambda (l)
          (cond
            ((null? l) 0)
            (else (+ 1 (eternity (cdr l)))))) 
         (cdr l))))))

; Function that determines the length of lists
; with <= 2 elements.
;
(lambda (l)
  (cond
    ((null? l) 0)
    (else 
      (+ 1 
        ((lambda (l)
          (cond
            ((null? l) 0)
            (else 
              (+ 1 
                ((lambda (l)
                  (cond
                    ((null? l) 0)
                    (else (+ 1 (eternity (cdr l))))))
                 (cdr l))))))
         (cdr l))))))

; Removing repetition by abstracting
;
; Function to calculate length of just empty list.
;
((lambda (len)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 (len (cdr l)))))))
 eternity)

; For lists with length <= 1
;
((lambda (len)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 (len (cdr l)))))))
 ((lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l)))))))
  eternity))

; For lists with length <= 2
;
((lambda (len)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 (len (cdr l)))))))
 ((lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l)))))))
  ((lambda (len)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (+ 1 (len (cdr l)))))))
   eternity)))
 
; Name the function that takes len as an argument
; and that returns a function that looks like len.
;
; For empty lists
;
((lambda (mk-length)
  (mk-length eternity))
 (lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l))))))))

; For lists with length <= 1
;
((lambda (mk-length)
  (mk-length (mk-length eternity)))
 (lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l))))))))

; For lists with length <= 2
;
((lambda (mk-length)
  (mk-length (mk-length (mk-length eternity))))
 (lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l))))))))

; Here comes the clever trick
;
; Works only for empty lists because the mk-length
; in else is supposed to take a function as an argument
; (like mk-length or eternity) so that it returns the 
; appropriate length function for (cdr l).
;
((lambda (mk-length)
  (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (mk-length (cdr l))))))))

; For lists with length <= 1
;
((lambda (mk-length)
  (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 ((mk-length eternity) (cdr l))))))))

; For all lists. Finally. But wait, this isn't Y-Combinator.
;
((lambda (mk-length)
  (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 ((mk-length mk-length) (cdr l))))))))

; Example
;
(((lambda (mk-length)
  (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 ((mk-length mk-length) (cdr l))))))))
 '(a b c 1 g))  ; 5

; Currying to make the inner function look more like len
;
; But the below function would overflow the recursion stack as
; we keep applying mk-length to itself repeatedly.
;
((lambda (mk-length)
  (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (len)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (len (cdr l)))))))
    (mk-length mk-length)))) ; Aborting!: maximum recursion depth exceeded

; To overcome this problem, convert the application of 
; mk-length to itself (mk-length mk-length) into a function.
;
((lambda (mk-length)
  (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (len)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (len (cdr l)))))))
    (lambda (x) ((mk-length mk-length) x)))))

; Separating the length function by giving it a name
;
((lambda (le)
  ((lambda (mk-length)
    (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x) ((mk-length mk-length) x))))))
 (lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1 (len (cdr l))))))))

; Separating the function that makes len from the 
; function that looks like len
;
(lambda (le)
  ((lambda (mk-length)
    (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x) ((mk-length mk-length) x))))))

; This function is called the applicative-order Y combinator
;
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

; Example
((Y  
  (lambda (len)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (+ 1 (len (cdr l))))))))
'(a b c 1 g)) ; 5
; </awesomeness>

; My hat sure doesn't fit anymore.
