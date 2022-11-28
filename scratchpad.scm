; remove first instance of member `a` from list of atoms
(define removeMember
  (lambda (s l)
    (cond
     ((null? l) (quote ()))
     ((equal? (car l) s) (cdr l))
     (else (cons (car l)
                 (removeMember s (cdr l)))))))

(removeMember `sauce (list `soy `sauce `and `other `sauce))
;Value: (soy and other sauce)


; remove all instances of member `a` from list of atoms
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


; given the parameters `(new old lat)` return a list of atoms with `new` inserted to the right of the first occurrence of `old`, followed by the rest of the list
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


; given the parameters `(new old lat)` return a list of atoms with `new` inserted to the left of the first occurrence of `old`, followed by the rest of the list
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


; replace the first instance of `old` in the `lat` with `new`
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


; replace either the first occurrence of `o1` or the first occurrence of `o2` with `new`
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

; write the function `add1` that adds 1 to an integer
(define add1
  (lambda (n)
    (+ n 1)))

(add1 5)
;Value: 6

;write the function `sub1` which subtracts 1 from an integer
(define sub1
  (lambda (n)
    (- n 1)))

(sub1 5)
;Value: 4

; write the function `sum` which adds two non-negative integers
(define sum
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (sum n (sub1 m)))))))

(sum 46 12)
;Value: 58

; write the function `subtract` which subtracts two non-negative integers
(define subtract
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (subtract n (sub1 m)))))))

(subtract 14 3)
;Value: 11

; write the function `addtup` which sums a tup
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (sum (car tup) (addtup (cdr tup)))))))

(addtup (list 1 2 3 4 5))
;Value: 15

; write the function `multiply` which multiplies two integers
(define multiply
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (sum n (multiply n (sub1 m)))))))

(multiply 12 3)
;Value: 36

; write the function `tup+` which takes two tups as input and outputs a tup with the sum of the first elements
; of each tup, followed by the sum of the second elements of each tup, ...
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

; write the function `>` (greater than)
(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(> 5 3)
;Value: #t

; write the function `<` (less than)
(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(< 5 3)
;Value: #f

; write the function `=` (equals)
(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(= 4 4)
;Value: #t

; write the function `↑` (power of/exponent)
(define ↑
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (multiply n (↑ n (sub1 m)))))))

(↑ 5 3)
;Value: 125

; what is a good name for this function?
(define ???
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (??? (- n m) m))))))

; answer: division (no remainder though)

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

; write the function `length` which takes a `lat` and returns the count of its S-expressions
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(length (list `here `are `four `words))
;Value: 4

; write the function `pick` which takes two arguments, `n` and `lat`, and returns the S-expression at index `n` (indexes are 1-based in Scheme)
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(pick 3 (list `retrieve `the `third `item `and `remember `it's `1-based))
;Value: third

; write the function `rempick` which returns a list of S-expressions, omitting the one at index `n`
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))

(rempick 3 (list `fix `this `broken `sentence))
;Value: (fix this sentence)

; write the function `no-nums` which removes numbers from a list of atoms
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else 
        (cond
          ((number? (car lat)) (no-nums (cdr lat)))
          (else (cons (car lat)
                      (no-nums (cdr lat)))))))))

(no-nums (list 1 `hey 2 `hello 3 `hi))
;Value: (hey hello hi)

; write the function `all-nums` which extracts a tup from its input `lat`
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? (car lat))
               (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))

(all-nums (list 1 `hey 2 `hello 3 `hi))
;Value: (1 2 3)

; write the function `eqan?` which returns true if both its arguments `a1` and `a2` are the same atom
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
        (= a1 a2))
      ((or (number? a1) (number? a2))
        #f)
      (else (eq? a1 a2)))))

(eqan? `hey `hey)

; write the function `occur` which counts the number of times an atom `a` appears in a `lat`
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
        (cond
          ((eq? (car lat) a) 
            (add1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))))

(occur `tyger (list `tyger `tyger `burning `bright))
;Value: 2

; write the function `one?` which returns true if `n` is 1 and false otherwise
(define one?
  (lambda (n)
    (cond
      ((and (number? n) (= 1 n))
        #t)
      (else #f))))

(one? 1)
;Value: #t

; rewrite the function `rempick` which removes the nth atom from a lat. Use the function `one?` in your answer
(define removepick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (removepick (sub1 n) (cdr lat)))))))

(rempick 3 (list `fix `this `broken `sentence))

; `atom?` is a primitive defined in the book, necessary for subsequent exercises
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? `an_atom)
;Value: #t

; `lat?` is a primitive defined in the book, necessary for subsequent exercises
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(lat? (list `a `list `of `atoms))
;Value: #t

; write the function `rember*` which removes occurences of `a` regardless of where it appears (in nested lists, etc)
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                         ((eq? (car l) a) (rember* a (cdr l)))
                         (else (cons (car l)
                                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

;             ((coffee) cup ((tea) cup) (and (hick)) cup)
(rember* `cup (list
                (list `coffee)
                `cup
                (list
                  (list `tea)
                  `cup)
                (list
                  `and
                  (list `hick))
                `cup))
;Value: ((coffee) ((tea)) (and (hick)))

; write the function `insertR*` which inserts the atom `new` to the right of `old` regardless of where `old` occurs
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                         ((eq? (car l) old) (cons (car l)
                                                  (cons new
                                                        (insertR* new old (cdr l)))))
                          (else (cons (car l)
                                      (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))

;                          (((good (good)) good (good)))
(insertR* `prevailed `good (list
                             (list
                               (list
                                 `good
                                 (list `good))
                                 `good
                                 (list `good))))
;Value: (((good prevailed (good prevailed)) good prevailed (good prevailed)))

; write the function `occur*` that counts the number of times an atom `a` appears in a list, regardless of where it occurs
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) (cond
                         ((eq? (car l) a) (add1 (occur* a (cdr l))))
                         (else (occur* a (cdr l)))))
      (else (sum (occur* a (car l))
                 (occur* a (cdr l)))))))

;               (chocolate banana (another banana) (((banana))))
(occur* `banana (list `chocolate `banana (list `another `banana) (list (list (list `banana)))))
;Value: 3

; write the function `subst*` which replaces each instance of `old` with `new`, regardless of where it occurs
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                         ((eq? (car l) old)
                          (cons new (subst* new old (cdr l))))
                         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))

;                   (create war (so much ((war))))
(subst* `peace `war (list `create `war (list `so `much (list (list `war)))))
;Value: (create peace (so much ((peace))))

; write the function `insertL*` which inserts the atom `new` to the left of `old` regardless of where `old` occurs
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (cond
                         ((eq? (car l) old) (cons new
                                                  (cons (car l)
                                                        (insertL* new old (cdr l)))))
                         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))
;                      (let us have a party wallace (((a party))))
(insertL* `drug `party (list `let `us `have `a `party `wallace (list (list (list `a `party)))))
;Value: (let us have a drug party wallace (((a drug party))))

; write the function `member*` which returns true if an atom is a member of a list and false otherwise
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eq? (car l) a) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(member* `hey (list `now (list `you `hey `now)))
;Value: #t

; write the function `leftmost` which returns the leftmost atom in a non-empty list of S-expressions that does not contain the empty list
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(leftmost (list (list (list `here)) (list `not `this `or `this)))
;Value: here

; write the function `eqlist?` which determines if two lists are equal (including nested lists). Use the function `eqan?`
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? (list `hey) (list `hey `hey))
;Value: #f
(eqlist? (list (list `hey `now)) (list (list `hey `now)))
;Value: #t

; write the function `equal?` which compares two S-expressions
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(equal? `hey `hey) ;Value: #t
(equal? `hey (list `hey)) ;Value: #f
(equal? (quote ()) (quote ())) ;Value: #t
(equal? (list `hey) (list `hey)) ;Value: #t
(equal? (list (quote ())) (list (quote ()))) ;Value: #t
(equal? (list (quote ()) `hey) (list (quote ()))) ;Value: #f

; write the function `numbered?` which determines if a representation of an arithmetic expression contains only numbers aside from the operator working upon those numbers
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp)))))))))

(numbered? 5) ;Value: #t
(numbered? (list 2 `x 3)) ;Value: #t

(define operator
  (lambda (aexp)
    (car aexp)))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

; write the function `value` which returns the natural value of a numbered arithmetic expression
(define value
  (lambda (nexp)
    (cond
      ((and (atom? nexp) (number? nexp)) nexp)
      ((eq? (operator nexp) `+)
       (sum (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) `x)
       (multiply (value (1st-sub-exp nexp))
                 (value (2nd-sub-exp nexp))))
      (else (↑ (value (1st-sub-exp nexp))
               (value (2nd-sub-exp nexp)))))))

(value 5) ;Value: 5
(value (list `+ 5 7)) ;Value: 12
(value (list `x 2 5)) ;Value: 10
(value (list `↑ 3 3)) ;Value: 27
(value (list `+ 3 (list `+ 3 3))) ;Value: 9

; write the function `makeset` which creates a unique (de-duped) list from a given list, using `removeAllMembers`
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat)
                  (makeset (removeAllMembers (car lat) (cdr lat))))))))

(makeset (list `apple `peach `pear `peach `plum `apple `lemon `peach))
;Value: (apple peach pear plum lemon)

; write the function `member?` which returns `true` if a `member` exists in a given `lat` and `false` if not
(define member?
  (lambda (member lat)
    (cond
      ((null? lat) #f)
      ((equal? member (car lat)) #t)
      (else (member? member (cdr lat))))))

(member? `this (list `here `is `a `sentence `that `contains `the `word `this)) ;Value: #t
(member? `this (list `here `is `one `that `does `not)) ;Value: #f

; write the function `subset?` which determines if all members of a list are present in another list (intersection)
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2)
       (subset? (cdr set1) set2))
      (else #f))))

(subset? (list `here `are `words)
         (list `here `are `some `other `words)) ;Value: #t
(subset? (list `one `of `these `doesn't `exist)
         (list `in `this `other `list `one `these `doesn't `exist)) ;Value: #f

; write `subset?` using `(and...)`
(define subsetA?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
                 (subsetA? (cdr set1) set2))))))

(subsetA? (list `here `are `words)
         (list `here `are `some `other `words)) ;Value: #t
(subsetA? (list `one `of `these `doesn't `exist)
         (list `in `this `other `list `one `these `doesn't `exist)) ;Value: #f

; write the function `eqset?` which returns true if two sets contain exactly the same members, in any order. (complete intersection? not a statistician, obvs)
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(eqset? (list `here `are `words)
        (list `here `words `are)) ;Value: #t
(eqset? (list `here `are `some `words)
        (list `here `are `words)) ;Value: #f

; write the function `intersect?` which returns true if at least one member from `set1` exists in `set2`
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (intersect? (cdr set1) set2)))))

(intersect? (list `none `of `these `words `appear `below)
            (list `the `world `has `gone `mad)) ;Value: #f
(intersect? (list `stewed `tomatoes `and `macaroni)
            (list `macaroni `and `cheese)) ;Value: #t

; write `intersect?` using `(or...)`
(define intersectO?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersectO? (cdr set1) set2))))))

(intersectO? (list `none `of `these `words `appear `below)
            (list `the `world `has `gone `mad)) ;Value: #f
(intersectO? (list `stewed `tomatoes `and `macaroni)
            (list `macaroni `and `cheese)) ;Value: #t

; write the function `intersect` which returns a list of members from `set1` that appear in `set2`
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(intersect (list `stewed `tomatoes `and `macaroni)
           (list `macaroni `and `cheese))
;Value: (and macaroni)

; write the function `union` which returns a list of members from `set1` that don't appear in `set2`, with `set2` concatenated
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))

(union (list `stewed `tomatoes `and `macaroni `casserole)
       (list `macaroni `and `cheese))
;Value: (stewed tomatoes casserole macaroni and cheese)

; write the function `intersectall`. It takes an `l-set`, which is a non-empty list of lists. It returns a list of members that are shared amongst each of the sublists of `l-set`
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))

(intersectall (list (list `a `b `c)
                    (list `c `a `d `e)
                    (list `e `f `g `h `a `b)))

(intersectall (list (list 6 `pears `and)
                    (list 3 `peaches `and 6 `peppers)
                    (list 8 `pears `and 6 `plums)
                    (list `and 6 `prunes `with `some `apples)))

; write the funtion `a-pair?` which returns true if its argument `x` is a list containing two S-expressions
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(a-pair? 5)
(a-pair? (list))
(a-pair? (list 5))
(a-pair? (list 5 7))
(a-pair? (list (list 5 7) (list 5 7)))
(a-pair? (list 5 7 3))

(define first
  (lambda (l)
    (car l)))

(define second
  (lambda (l)
    (car (cdr l))))

(define build
  (lambda (s1 s2)
    (cons s1
      (cons s2 (quote ())))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(revpair (list 1 2))

; write the function `revrel` that, given a rel (set of pairs) returns a rel with each of the pairs reversed
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      ((cons (revpair (car rel))
             (revrel (cdr rel)))))))

(revrel (list (list 1 2)
              (list 3 4)
              (list 5 6)))
