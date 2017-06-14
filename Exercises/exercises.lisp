;; Antoine Carpentier
;; Exercises lisp for Artificial Intelligence Programming Paradigms


;; Write function that can sort a list of numbers in ascending and
;; descending order depending on the value of a key-parameter. Example
;; (sort-numbers '(3 5 7 2 1) :ascending t) = (1 2 3 5 7)
;; (sort-numbers '(3 5 7 2 1) :ascending nil) = (7 5 3 2 1)

(defun quicksort (l)
  	(let ((pivot (car l)))
		(if (equal pivot nil) '() 
			(let ((less (remove pivot l :test '<=))
	      	      		(more (remove pivot l :test '>=)))
			(append (quicksort less) (list pivot) (quicksort more))))))

(defun sort-numbers (l &key ascending) 
	(let ((sorted (quicksort l)))
	     (if ascending sorted (reverse sorted)))
)

(print "-- Tests sorting --")
(print (equal (sort-numbers '(3 5 7 2 1) :ascending t) '(1 2 3 5 7)))
(print (equal (sort-numbers '(3 5 7 2 1) :ascending nil) '(7 5 3 2 1)))

;; Write a function to exponentiate , or raise a number to an integer
;; power. For example: (power 3 2) = 9

(defun power (x y) (if (= y 0) 1 (* x (power x (- y 1)))))

(print "-- Tests power --")
(print (equal (power 3 2) 9))
(print (equal (power 4 3) 64))
(print (equal (power 7 1) 7))


;; Write a function that counts the number of atoms in an
;; expression. For example (count-atoms '(a (b) c)) = 3.

(defun count-atoms (l) (if (listp l) (reduce '+ (map 'list 'count-atoms l)) 1))


(print "-- Tests count-atoms --")
(print (equal (count-atoms '(a (b) c)) 3))
(print (equal (count-atoms '(a b c (b c d) (f (a a)) 5)) 10))

;; Write a function that counts the number of times an expression
;; occurs anywhere within another expression. Example: (count-anywhere
;; 'a '(a ((a) b) a)) = 3

(defun flatten (l) (if (not (listp l)) 
		       (list l) 
		       (if (equal l nil) 
			   '() 
			   (reduce 'append (map 'list 'flatten l)))))

(defun count-anywhere (a l) (list-length (remove a (flatten l) :test-not 'equal)))

(print "-- Tests count-anywhere --")
(print (equal (count-anywhere 'a '(a ((a) b) a)) 3))
(print (equal (count-anywhere 'a '(a a a (b b (c c c)) (d d d) (a a))) 5))

;; Write a function to compute the dot-product of two sequences of
;; numbers, represented as lists. Example: (dot-product '(10 20) '(3
;; 4)) = 110

(defun dot-product (l1 l2) (reduce '+ (mapcar '* l1 l2)))

(print "-- Tests dot-product --")
(print (equal (dot-product '(10 20) '(3 4)) 110))
(print (equal (dot-product '(7 10) '(4 20)) 228))

;; Write a function that flattens a tree-like structure to a
;; list. Example: (flatten '(1 (2 (3)))) = (1 2 3)

;; already written for count-anywhere, see above

(print "-- Tests flatten --")
(print (equal (flatten '(1 (2 (3)))) '(1 2 3)))

;; Write a function that given a set returns al it’s subsets.
;; Example: (subsets (1 2 3)) = ( () (1) (2) (3) (1 2) (1 3) (2 3) (1
;; 2 3) ) Extra: try to write both an iterative and a recursive
;; version.

(defun maplam (lam l)
  	(loop for item in l collect (funcall lam item)))

(defun subsets (l)
	(if (not (car l)) ; if empty list return empty list 
	    (list l)
	    (let ((others (subsets (cdr l))))
	         (append others (maplam (lambda (x) (cons (car l) x)) others)))))

; not working
(print "--Tests subsets --")
(print (equal (subsets '()) '(())))
(print (equal (subsets '(1)) '(NIL (1)))) 
(print (equal (subsets '(1 2 3)) '(NIL (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))))


;; Optional: Try out the following built-in functions and play around with the
;; :test and :key parameters. Knowing these things and being able to
;; use the keywords can spare you lots of time.

;; subst, subst-if, tree-equal, append, mapcar, assoc and rassoc, intersection, adjoin, subsetp, union

;; concatenate, remove, delete, delete-if, reverse, sort, count, find, find-if, member




