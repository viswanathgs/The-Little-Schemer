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

; Entry:
;
; An entry is a pair of lists whose first list is a set.
; Also, the two lists must be of equal length.
;
; Examples:
; ((appetizer entree beverage)
;  (pate boeuf vin))
; ((appetizer entree beverage)
;  (beer beer beer))
; ((beverage dessert)
;  ((food is) (number one with us)))
; 

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

; Define third (from chapter 07)
;
(define third
  (lambda (l)
    (car (cdr (cdr l)))))


; Define build (from chapter 07)
; 
; Builds a pair from two S-expressions.
;
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

; Define new-entry
;
; Build an entry from a set of names and a list of values.
; (This is like a map)
;
(define new-entry build)
;
(new-entry '(appetizer entree beverage)
           '(pate boeuf vin))
(new-entry '(appetizer entree beverage)
           '(beer beer beer))
(new-entry '(beverage dessert)
           '((food is) (number one with us)))

; Define lookup-in-entry
;
; Searches for name in (first entry). The corresponding element
; in (second entry) is returned if there is a match. 
; If no match is found, then entry-f function is called.
;
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

; Define lookup-in-entry-help
;
; Helper for lookup-in-entry
;
(define lookup-in-entry-help
  (lambda (name nameset valuelist entry-f)
    (cond
      ((null? nameset) (entry-f name))
      ((eq? (car nameset) name) (car valuelist))
      (else (lookup-in-entry-help name (cdr nameset) (cdr valuelist) entry-f)))))

;
(lookup-in-entry 'entree '((appetizer entree beverage) (food tastes good)) #f) ; tastes

; Table:
;
; A table (also called an environment) is a list of entries.
; 
; Examples:
; ()
; (((appetizer entree beverage) (pate boeuf vin))
;  ((beverage dessert) ((food is) (number one with us))))
;

; Define extend-table
;
; Takes an entry and a table and creates a new table
; by putting the entry in front of the old table.
;
(define extend-table cons)

; Define lookup-in-table
;
; Checks its entries for a match for name. Returns the first match.
; table-f is called when name is not found in the table.
;
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else 
        (lookup-in-entry name (car table)
          (lambda (name) (lookup-in-table name (cdr table) table-f)))))))
;
(lookup-in-table 'entree '(((entree dessert) (spaghetti spumoni))
                           ((appetizer entree beverage) (food tastes good))) #f) ; spaghetti

; Writing a basic Scheme interpreter (yeah, something like eval)
;
; Types: *const, *quote, *identifier,
;        *lambda, *cond and *application
;
; Types are represented by functions called as "actions".
; Actions are functions that do "the right thing" when applied
; to the appropriate type of expression.
; 
; value should find out the type of expression passed to it
; and then use the associated action.
;

; Define expression-to-action
;
; Produces the correct action (or function) for
; each possible S-expression.
;
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

; Define atom-to-action
;
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

; Define list-to-action
;
(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
        (cond
          ((eq? (car e) 'quote) *quote)
          ((eq? (car e) 'lambda) *lambda)
          ((eq? (car e) 'cond) *cond)
          (else *application)))
      (else *application))))

; Define value
;
; Takes an expression and evaluates it. An empty table is passed
; to the function meaning as one of the arguments. 
;
; The function value, together will all the functions it uses, is
; the interpreter. The function value approximates the function 
; eval of Scheme.
;
(define value
  (lambda (e)
    (meaning e '())))

; Define meaning
;
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

; Define *const
;
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

; Define *quote
;
(define *quote
  (lambda (e table)
    (text-of e)))

; Define text-of
;
(define text-of second)

; Define *identifier
;
; (Given that the table contains the values of the identifiers)
;
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

; Define initial-table
;
(define initial-table
  (lambda (name)
    (car '())))

; Define *lambda
;
; Takes an expression which is basically the representation of
; a lambda function in a list format. Returns a list of the form
; (non-primitive (table formal-arguments function-body)).
;
(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

; Define table-of
;
(define table-of first)

; Define formals-of
;
(define formals-of second)

; Define body-of
;
(define body-of third)

; Define *cond
;
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

; Define cond-lines-of
;
(define cond-lines-of cdr)

; Define evcon
;
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
        (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
        (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

; Define else?
;
(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))

; Define question-of
;
(define question-of first)

; Define answer-of
;
(define answer-of second)

; Define *application
;
; An application is represented as a list of expressions
; whose car position contains an expression whose value
; is a function. 
;
; An application must always determine the 
; meaning of all its arguments before applying the function
; on the arguments.
;
(define *application
  (lambda (e table)
    (apply-function (meaning (function-of e) table) (evlis (arguments-of e) table))))

; Define function-of
;
(define function-of car)

; Define arguments-of
;
(define arguments-of cdr)

; Define evlis
;
; Takes a list of (representations of) arguments and a table,
; and returns a list composed of the meaning of each argument.
;
(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else (cons (meaning (car args) table) (evlis (cdr args) table))))))

; Define primitive?
;
(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

; Define non-primitive?
;
(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

; Define apply-function
;
; This function approximates the apply function of Scheme.
;
(define apply-function
  (lambda (fun vals)
    (cond
      ((primitive? fun) (apply-primitive (second fun) vals))
      ((non-primitive? fun) (apply-closure (second fun) vals)))))

; Define apply-primitive
;
(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons) (cons (first vals) (second vals)))
      ((eq? name 'car) (car (first vals)))
      ((eq? name 'cdr) (cdr (first vals)))
      ((eq? name 'null?) (null? (first vals)))
      ((eq? name 'eq?) (eq? (first vals) (second vals)))
      ((eq? name 'atom?) (:atom? (first vals)))
      ((eq? name 'zero?) (zero? (first vals)))
      ((eq? name 'number?) (number? (first vals))))))

; Define :atom?
;
(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))

; Define apply-closure
;
; Applying a non-primitive function - a closure - to a list
; of values is the same as finding the meaning of the closure's
; body with its table extended by an entry of the form
;   (formals values)
; In this entry, formals is the formals of the closure and 
; values is the result of evlis.
;
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure) 
      (extend-table (new-entry (formals-of closure) vals) (table-of closure)))))

; It's time for a banquet.
