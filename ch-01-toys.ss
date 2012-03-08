; 
; Code snippets from "The Little Schemer", 4th edition, by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 01) Toys)
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Atoms
;
(quote atom)
'atom
'turkey
1492
'u
'*abc$

; Lists
;
(quote (atom))
'(atom)
'(atom turkey or)
'((atom turkey) or)

; S-expressions
;
; All atoms and lists are S-expressions
;
'xyz
'(x y z)
'((x y) z)
'(how are you doing so far)
'(((how) are) ((you) (doing so)) far)
'()
'(() () () ())

; The Law of Car
;
;   The primitive car is defined only 
;   for non-empty lists.
;
(car '(a b c))
(car '((a b c) x y z))
(car '(((hotdogs)) (and) (pickle) relish))
(car (car '(((hotdogs)) (and) (pickle) relish)))

; The Law of Cdr
;
;   The primitive cdr is defined only 
;   for non-empty lists. The cdr of any 
;   non-empty list is always another list.
;
(cdr '(a b c))
(cdr '(hamburger))
(cdr '((x) t r))
(car (cdr '((b) (x y) ((c)))))
(cdr (cdr '((b) (x y) ((c)))))

; The Law of Cons
;
;   The primitive const takes two arguments.
;   The second argument to cons must be a
;   list. The result is a list.
;   
;   The first argument is any S-expression.
;   cons adds this to the front of a list.
;
(cons 'peanut '(butter and jelly))
(cons '(banana and) '(peanut butter and jelly))
(cons '((help) this) '(is very ((hard) to learn)))
(cons '(a b (c)) '())
(cons 'a '())
(cons 'a (car '((b) c d)))
(cons 'a (cdr '((b) c d)))

; The Law of Null?
;
;   The primitive null? is defined only
;   for lists.
;
;   In practice, (null? a) is false for 
;   everything except when a is an empty
;   list.
;
(null? '())
(null? (quote ()))
(null? '(a b c))

; Define atom?
; 
; True if S-expression is an atom.
;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
;
(atom? 'Harry)
(atom? '(Harry had a heap of apples))
(atom? (car '(Harry had a heap of apples)))
(atom? (cdr '(Harry had a heap of apples)))
(atom? (cdr '(Harry)))
(atom? (car (cdr '(swing low sweet cherry oat))))
(atom? (car (cdr '(swing (low sweet) cherry oat))))

; The Law of Eq?
;
;   The primitive eq? takes two arguments.
;   Each must be a non-numeric atom.
;
(eq? 'Harry 'Harry)
(eq? 'margarine 'butter)
(eq? (car '(Mary had a little lamb chop)) 'Mary)
(eq? (car '(beans beans we need jelly beans)) (car (cdr '(beans beans we need jelly beans))))

; 
; ###############################################
;
;           This space reserved for
;                JELLY STAINS!
;
; ###############################################    
;
