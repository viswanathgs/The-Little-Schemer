; 
; Code snippets from "The Little Schemer", 4th edition, by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 07) Friends and Relations) 
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

; Define member? (from chapter 02)
;
; True if a is a member of lat.
;
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) 
              (member? a (cdr lat)))))))

; Define set?
;
; Checks if a lat is a set.
;
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))
;
(set? '(apple peaches apple plum))    ; #f
(set? '(apple peaches pears plums))   ; #t
(set? '(apple 3 pear 4 9 apple 3 4))  ; #f

; Define makeset
;
; Creates a set from lat.
;
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))
;
(makeset '(apple peach pear peach 
           plum apple lemon peach))   ; (pear plum apple lemon peach)

; Define multirember (from chapter 03)
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

; Define makeset (maintaining the order of atoms)
;
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))
;
(makeset '(apple peach pear peach
           plum apple lemon peach))     ; (apple peach pear plum lemon)
(makeset '(apple 3 pear 4 9 apple 3 4)) ; (apple 3 pear 4 9)

; define subset?
;
; Checks if set1 is a subset of set2.
;
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))
;
(subset? '(5 chicken wings)
         '(5 hamburgers
           2 pieces fried chicken and
           light duckling wings))       ; #t
(subset? '(4 pounds of horseradish)
         '(four pounds chicken and   
           5 ounces horseradish))       ; #f

; define eqset?
;
; Checks if set1 and set2 are equal.
;
(define eqset?
  (lambda (set1 set2)
    (cond
      ((subset? set1 set2) (subset? set2 set1))
      (else #f))))

; define eqset? (simplified)
;
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))
;
(eqset? '(6 large chickens with wings) 
        '(6 chickens with large wings)) ; #t

; define intersect?
;
; Checks if set1 and set2 have at least
; one common atom.
;
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))
;
(intersect? '(stewed tomatoes and macaroni)
            '(macaroni and cheese))     ; #t

; define intersect
;
; Produces the intersecting set of set1
; and set2.
;
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))
;
(intersect '(stewed tomatoes and macaroni)
           '(macaroni and cheese))       ; (and macaroni)

; define union
;
; Produces the union set of set1 and set2.
;
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))
;
(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))          ; (stewed tomatoes casserole macaroni and cheese)

; define difference
;
; Produces set1 - set2
;
(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

; define intersectall
;
; Produces the intersection of a non-empty list of sets.
;
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))
;
(intersectall '((a b c)
                (c a d e)
                (e f g h a b)))                   ; (a)
(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples))) ; (6 and)

; Define a-pair?
;
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))
;
(a-pair? '(pear pear))    ; #t
(a-pair? '(3 7))          ; #t
(a-pair? '((2) (pair)))   ; #t
(a-pair? '(full (house))) ; #t
(a-pair? '())             ; #f
(a-pair? '(a b c))        ; #f

; Define first
;
; First of a pair.
;
(define first
  (lambda (p)
    (car p)))

; Define second
;
; Second of a pair.
;
(define second
  (lambda (p)
    (car (cdr p))))

; Define build
;
; Builds a pair from two S-expressions.
;
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

; Define third
;
(define third
  (lambda (l)
    (car (cdr (cdr l)))))

; Note:
;
; rel:
;   A relation (rel) is a set of pairs.
;
;   ((apple peaches) (pumpkin pie)) is a rel.
;   ((apple peaches) (pumpkin pie) (apple peaches)) is not a rel.
;
;	fun:
;   A function (fun) is a rel in which the
;   firsts of each pair in the rel form a set.
;   (fun here refers to mathematical functions)
; 
;   ((4 3) (7 3) (4 2)) is not a fun.
;   ((4 3) (7 3) (5 2)) is a fun.
;

; Define firsts (from chapter 03)
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

; Define fun?
;
; Checks if a rel is a fun.
;
(define fun?
  (lambda (rel)
    (set? (firsts rel))))
;
(fun? '((4 3) (4 2) (7 6) (6 2) (3 4))) ; #f
(fun? '((8 3) (4 2) (7 6) (6 2) (3 4))) ; #t
(fun? '((d 4) (b 0) (b 9) (e 5) (g 4))) ; #f

; Define revpair
;
; Reverses a pair.
;
(define revpair
  (lambda (p)
    (build (second p) (first p))))

; Define revrel
;
; Reverses each pair in a rel.
;
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))
;
(revrel '((8 a) (pumpkin pie) (got sick)))  ; ((a 8) (pie pumpkin) (sick got))

; Define seconds
; 
; Takes a list that is either null or contains
; only non-empty lists as an argument.
; Builds a list composed of the second S-expression
; of each internal list.
;
(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

; Define one-one?
;
; Checks if fun is one-one.
;
(define one-one?
  (lambda (fun)
    (set? (seconds fun))))

; Define one-one? (without using seconds)
;
(define one-one?
  (lambda (fun)
    (fun? (revrel fun))))
;
(one-one? '((8 3) (4 2) (7 6) (6 2) (3 4)))               ; #f
(one-one? '((8 3) (4 8) (7 6) (6 2) (3 4)))               ; #t
(one-one? '((grape raisin) (plum prune) (stewed prune)))	; #f
(one-one? '((grape raisin) (plum prune) (stewed grape)))  ; #t
(one-one? '((chocolate chip) (doughy cookie)))            ; #t

; ###############################################
;           
;          Or better yet, make your own.
;
; ###############################################
;
