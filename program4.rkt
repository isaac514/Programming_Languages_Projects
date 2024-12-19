#lang scheme
"+++++++++++++++++++++++++++++++(CS_4121_Project4)+++++++++++++++++++++++++++++++"
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
;Problem 1)
;Function for finding index of last occurance of an element

(define (last a L)
  (define (innerfunc L index result)
    (cond
      [(null? L) result] ; If list is empty default case
      [(equal? (first L) a) (innerfunc (cdr L) (+ index 1) index)] ;If equal set the update the index
      [else (innerfunc (cdr L) (+ index 1) result)])) ;If true update the index
  (innerfunc L 0 -1))

"Problem 1 output)"
"-----------------"
(last 3 '(2 1 3 4))
(last 3 ' (2 1 3 3 4))
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

;Problem 2)
;Returns M with a pair of parenthese wrapped around each atom occuring in M.
(define (wrap M)
  (cond
    [(null? M) '()]  ;Base case
    [else
     (let ((current (first M)))  ;Get the first element
       (define enclose
         (if (list? current)  ;If the current element treat it as an atom so it can be wrapping
             (cons (wrap current) '())  ;Wrap sublist
             (list current)))  ;Wrap atom
       (cons enclose (wrap (cdr M))))])) 

"Problem 2 output)"
"-----------------"
(wrap '(1  ()))
(wrap '(1 (2) 3))
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

;Problem 3)
;(count-parens-all M) counts the number of opening and closing parentheses in M.
(define (count-parens-all M)
  (cond
    [(null? M) 2] ; Base case empty list `()`
    [(not (list? M)) 0] ; If the current element is not a list, it contributes 0 parentheses
    [else
        (+ 2 
        (count-parens-all (car M)))])) ; Count parentheses in the first element
        

"Problem 3 output)"
"-----------------"
(count-parens-all '())
(count-parens-all '((a b) c))
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

;Problem 4)
;(insert-right-all new old M) builds a list obtained by inserting the item new to the right of all
;occurrences of the item old in the list M.
(define (insert-right-all new old M)
  (cond
    [(null? M) '()] ; If the list is empty, return an empty list
    [(not (list? (car M))) ; If the current element is not a list
     (if (equal? (car M) old) ; Check if current element equals old
         (cons (car M) (cons new (insert-right-all new old (cdr M)))) ; Insert the new element
         (cons (car M) (insert-right-all new old (cdr M))))] ; Move to the next element
    [else ; Current element is a sublist
     (cons (insert-right-all new old (car M)) ; Recur on the sublist
           (insert-right-all new old (cdr M)))])) ; Process the rest of the list
     

"Problem 4 output)"
"-----------------"
(insert-right-all 'z 'a '(a b (a c (a))))
(insert-right-all 'dog 'cat '(my dog is fun))
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

;Problem 5)
;(invert M), where M is a list that is a list of pairs (lists of length two), returns a list with each pair
;reversed.

(define (invert M)
  (cond
    [(null? M) '()] ;If list is empty return 
    [(pair? (car M)) ;Check if first element is a pair
     (cons (list (cadr (car M)) (car (car M))) (invert (cdr M)))]  ;Reverse the pair and recur on the rest of the list
     [else (cons (car M) (invert (cdr M)))])) ;Handle elements that are not pairs

"Problem 5 output)"
"-----------------"
(invert '((a 1) (a 2) (b 1) (b 2)))
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

;Problem 6)
;(filter-out pred L) return a flat list of those elements in L that do not satify the predicate pred.
(define (filter-out pred L)
  (cond
    [(null? L) '()]
    [(pred (car L))
        (filter-out pred (cdr L))]
    [else (cons (car L) (filter-out pred (cdr L)))]))


"Problem 6 output)"
"-----------------"
(filter-out number? '(a 2 #f b 7))
(filter-out symbol? '(a 2 #f b 7))
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

;Problem 7)
;summatrices M1 M2) returns the sum of matrices M1 and M2. A matrix is represented as a list of
;rows, eachof which is a flat list of numbers.

(define (summatrices M1 M2)
  (define (rowaddition r1 r2) ;helper function to add each of the elements in the sublists
      (map + r1 r2))
  (cond
    [(null? M1) '()]
    [(null? M2) '()]
    [else (map rowaddition M1 M2)]))
    
    
"Problem 7 output)"
"-----------------"
(summatrices '((1 2 3)) '((4 5 6)))
(summatrices '((1 2 3) (4 5 6)) '((10 10 10) (20 20 20)))
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

;Problem 8)
;(swapper a1 a2 M) returns M with all occurrences of a1 replaced by a2 and all occurrences of a2
;replaced by a1.

(define (swapper a1 a2 M)
  (cond
    [(null? M) '()] ;return an empty list
    [(equal? (car M) a1) ;If element is a1
     (cons a2 (swapper a1 a2 (cdr M)))] ;Replace with a2 and act of the rest of the list
    [(equal? (car M) a2) ;If element is a2
     (cons a1 (swapper a1 a2 (cdr M)))] ;Replace it with a1 and act on the rest of the list
    [else ; If the element is neither 
     (cons (car M) (swapper a1 a2 (cdr M)))])) ;Move on to the remainder of the list

"Problem 8 output)"
"-----------------"
(swapper 'a 'd '(a b c d))
(swapper 'x 'y '((x) y (z (x))))
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

;Problem 9)
;(flatten M) returns M with all inner parenthese removed. flatten turns a list into a flat list

(define (flatten M)
  (cond
    ((null? M) '()) ; return empty list
    ((list? (car M)) ; If the first element is a list, recur until a single element is reached
     (append (flatten (car M)) (flatten (cdr M))))
    (else (cons (car M) (flatten (cdr M)))))) ; Otherwise, include the element and recurse on the rest
  


"Problem 9 output)"
"-----------------"
(flatten '(a b c))
(flatten '((a) () (b ()) () (((c)))))
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

;Problem 10)
;(binary-tree-insert T n) that takes T and an
;integer n, and inserts n into T while maintaining its binary search tree property.

(define (binary-tree-insert T n)
  (cond
    ((null? T) (list '() n '())) ; Empty tree n is the new root
    ((< n (cadr T)) ; If n is less than the current root
     (list (binary-tree-insert (car T) n) (cadr T) (caddr T))) ; Insert n in the left subtree
    ((> n (cadr T)) ; If n is greater than the current root
     (list (car T) (cadr T) (binary-tree-insert (caddr T) n))) ; Insert n in the right subtree
    (else T))) ; If n is equal to the root no nothing

"Problem 10 output)"
"-----------------"
(binary-tree-insert '((() 2 ()) 3 (() 4 (() 5 ()))) 6)
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

;Problem 11)
;Create a functional abstraction of the following two Scheme functions and then redefine each function
;in terms of the abstraction.

;Abstracted function
(define (lst M element base output)
  (cond
    [(null? M) base] ; Case for empty lists
    [(not (pair? (first M))) ; Case for individual elements
     (output (first M) (lst (cdr M) element base output) element)]
    [else ; Case for nested lists
     (output (lst (first M) element base output)
             (lst (cdr M) element base output)
             element)]))

;Function definitions
(define (rember* a M)
  (lst 
   M
   a ; Element to remove
   '() ; Case for empty lists
   (lambda (x y elem) ;Output function
     (if (eq? x elem) y (cons x y)))))

(define (depth M)
  (lst
   M
   #f ;unused for depth
   1 ; Case for empty lists
   (lambda (x y _) ; Combine function
     (if (number? x) ; Check to see if x is a number
         (max (add1 x) y) ; Add 1 to the depth of the nested list
         y)))) ; Ignore individual elements and return depth of the rest




    

