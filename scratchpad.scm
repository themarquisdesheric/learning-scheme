; remove first instance of member from list of atoms
(define removeMember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
             (removeMember a (cdr lat)))))))

(removeMember `sauce (list `soy `sauce `and `other `sauce))
;Value: (soy and other sauce)


; remove all instances of member from list of atoms
(define removeAllMembers
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (removeAllMembers a (cdr lat)))
      (else (cons (car lat)
                  (removeAllMembers a (cdr lat)))))))

(removeAllMembers `sauce (list `soy `sauce `and `other `sauce))
;Value: (soy and other)


; given a list that contains non-empty lists, return a list containing the first S-expression of each nested list
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(firsts (list (list `a `b `c)
              (list `d `e `f)
              (list `g `h `i)))
;Value: (a d g)

(firsts (list (list `a `b `c)
              (list (list `nested `list))
              (list `d `e `f)))
;Value: (a (nested list) d)


; given the parameters (new old lat) return a list of atoms with new inserted to the right of the first occurrence of old, followed by the rest of the list
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old) (cons (car lat)
                                         (cons new
                                               (cdr lat))))
              (else (cons (car lat)
                          (insertR new old (cdr lat)))))))))

(insertR `practice `more (list `give `me `some `more `please))
;Value: (give me some more practice please)


; given the parameters (new old lat) return a list of atoms with new inserted to the left of the first occurrence of old, followed by the rest of the list
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old) (cons new lat))
              (else (cons (car lat)
                          (insertL new old (cdr lat)))))))))

(insertL `spicy `food (list `give `me `some `food))
;Value: (give me some spicy food)


; replace the first instance of old in the lat with new
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old) (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst new old (cdr lat)))))))))

(subst `kind `mean (list `you `should `be `mean `to `people))
;Value: (you should be kind to people)


; replace either the first occurrence of o1 or the first occurrence of o2 with new
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((or (eq? (car lat) o1) (eq? (car lat) o2)
               (cons new (cdr lat))))
              (else (cons (car lat)
                          (subst2 new o1 o2 (cdr lat)))))))))

(subst2 `vanilla `chocolate `banana
  (list `banana `ice `cream `with `chocolate `topping))
; Value: (vanilla ice cream with chocolate topping)

; write a function that adds 1
(define add1
  (lambda (n)
    (+ n 1)))

(add1 5)
;Value: 6

;write a function that subtracts 1
(define sub1
  (lambda (n)
    (- n 1)))

(sub1 5)
;Value: 4

; write the function sum which adds two non-negative integers
(define sum
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (sum n (sub1 m)))))))

(sum 46 12)
;Value: 58

; write the function subtract which subtracts two non-negative integers
(define subtract
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (subtract n (sub1 m)))))))

(subtract 14 3)
;Value: 11

; write the function addtup which sums a tup
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (sum (car tup) (addtup (cdr tup)))))))

(addtup (list 1 2 3 4 5))
;Value: 15

; write the function multiply which multiplies two integers
(define multiply
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (sum n (multiply n (sub1 m)))))))

(multiply 12 3)
;Value: 36

; write the function tup+ which takes two tups as input and outputs a tup with the sum of the first elements
; of each tup, followed by the sum of second elements of each tup, ...
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2))
       (quote ()))
      (else (cons (sum (car tup1) (car tup2))
                    (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ (list 3 6 9 11 4)
      (list 8 5 2 0 7))
;Value: (11 11 11 11 11)

; write a greater than function
(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(> 5 3)
;Value: #t

; write a less than function
(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(< 5 3)
;Value: #f

; write the function =
(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(= 4 4)
;Value: #t

(define powerof
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (multiply n (powerof n (sub1 m)))))))

(powerof 5 3)
;Value: 125

; what is a good name for this function? exercise
(define ???
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (??? (- n m) m))))))

; answer: division (no remainder)
(??? 6 3)
;Value: 2

; (??? 6 3) = 1 + (??? 3 3)
;           = 1 + 1 + (??? 0 3)
;           = 1 + 1 + 0
;Value: 2

; (??? 6 2) = 1 + (??? 4 2)
;           = 1 + 1 + (??? 2 2)
;           = 1 + 1 + 1 + (??? 0 2)
;           = 1 + 1 + 1 + 0
;Value: 3

; (??? 6 4) = 1 + (??? 2 4)
;           = 1 + 0
;Value: 1
