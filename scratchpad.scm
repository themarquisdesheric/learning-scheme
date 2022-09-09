; remove first instance of member from list of atoms
(define removeMember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
             (removeMember a (cdr lat)))))))

; (removeMember `sauce (list `soy `sauce `and `other `sauce))
;Value: (soy and other sauce)


; remove all instances of member from list of atoms
(define removeAllMembers
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (removeAllMembers a (cdr lat)))
      (else (cons (car lat)
                  (removeAllMembers a (cdr lat)))))))

; (removeAllMembers `sauce (list `soy `sauce `and `other `sauce))
;Value: (soy and other)


; given a list that contains non-empty lists, return a list containing the first S-expression of each nested list
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

; (firsts (list (list `a `b `c)
;               (list `d `e `f)
;               (list `g `h `i)))
;Value: (a d g)

; (firsts (list (list `a `b `c)
;               (list (list `nested `list))
;               (list `d `e `f)))
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

; (insertR `practice `more (list `give `me `some `more `please))
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

; (insertL `spicy `food (list `give `me `some `food))
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

; (subst `kind `mean (list `you `should `be `mean `to `people))
;Value: (you should be kind to people)


; replace either the first occurrence of o1 or the first occurrence of o2 with new
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) o1) (cons new (cdr lat)))
              ((eq? (car lat) o2) (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst2 new o1 o2 (cdr lat)))))))))

(subst2 `vanilla `chocolate `banana
  (list `banana `ice `cream `with `chocolate `topping))
; Value: (vanilla ice cream with chocolate topping)
