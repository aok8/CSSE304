;Alain Kouassi
;CSSE304
;Assignment02

;#1a
;used more efficient fact
(define fact
	(lambda (n)
		(if (negative? n) 
			"error"
			(fact-acc n 1)
		)
	)
)
(define fact-acc
	(lambda (n acc)
		(if (zero? n)
			acc
			(fact-acc (- n 1) (* n acc))
		)
	)
)

;#1b
; n choose k is (n!)/((n-k)!*k!)
(define choose
	(lambda(n k)
		(/ 
			(fact n)
			(*
				(fact k)
				(fact (- n k))
			)
		)
	)
)

;#2
(define sum-of-squares
	(lambda (n)
		(if (null? n) 
			0
			(sum-of-squares-acc n 0)
		)
	)
)

;used an accumulator style helper to sum the squares
(define sum-of-squares-acc
	(lambda (n acc)
		(if (null? n)
			;if list is empty return accumulator value
			acc
			;recurse through list, adding squares as stepping in
			(sum-of-squares-acc 
				(cdr n) 
				(+ 
					(expt (car n) 2) 
					acc
				)
			)
		)
	)
)

;#3
(define range
	(lambda (n m )
		(if (>= n m)
			;if n>= m return empty list
			(list)
			;used accumulator style helper with end truncated 
			(range-acc n (- m 1) (list))
		)
	)
)
(define range-acc
	(lambda (n m l)
		(if (> n m)
			;reached end where m =n return list
			l
			;decrement m and add latest value to head of list (moving backwards)
			(range-acc n (- m 1) (cons m l))
		)
	)
)

;#4
;helper method used in #4 and #5 to find out if a value is in a list
(define contains?
	(lambda (x list)
		(if (null? list)
			#f
			(if (equal? x (car list))
				#t
				(contains? x (cdr list))
			)
		)
	)
)
(define set? 
	(lambda(list)
		(if (null? list)
			;if list is empty no duplicates so it is a set
			#t
			;list is not empty, use helper to have better efficiency
			(set-check list)
		)
	)
)
(define set-check
	(lambda(list)
		(if (null? list)
			;reached end of list without finding duplicates
			#t
			(if (contains? (car list) (cdr list))
				;found duplicate of current value later in the list
				#f
				;did not find duplicate of current value, check again with next value and shorter list
				(set-check (cdr list))
			)
		)
	)
)

;#5
(define union
	(lambda (list1 list2)
		(cond
			;if first list is empty union is just second list
			[(null? list1) list2]
			;check for values in list1 that are also in list2,
			;if value in list1 is found in list2, skip it, it will be added to union when 
			;list 1 is empty
			[(contains? (car list1) list2) (union (cdr list1) list2)]
			;value was just in list1, add value and call again on shorter list 1 and original list 2
			;recurse until list 1 is empty, by then, to get the union all that is needed is 
			;to add list 2 ( will be accomplished in first cond: if list one is null) 
			[else (cons (car list1) (union (cdr list1) list2))]
		)
	)
)

;#6
;code taken from 1.ss
(define first
	(lambda (a)
		(car a)
	)
)
(define second
	(lambda (a)
		(cadr a)
	)
)
(define third
	(lambda (a)
		(caddr a)
	)
)

(define i-cross-product
	; for finding the i value of the cross product
	; for vectors (a1 a2 a3) and (b1 b2 b3): i value is
	; ((a2*b3)-(a3*b2))
	(lambda (a b)
		(- (* (second a ) (third b)) (* (third a ) (second b)))
	)
)
(define j-cross-product
	; for finding the j value of the cross product
	; for vectors (a1 a2 a3) and (b1 b2 b3): j value is
	; -((a1*b3)-(a3*b1))
	(lambda (a b)
		(- (* (first a ) (third b)) (* (third a ) (first b)))
	)
)
(define k-cross-product
	; for finding the k value of the cross product
	; for vectors (a1 a2 a3) and (b1 b2 b3): k value is
	; ((a1*b2)-(a2*b1))
	(lambda (a b)
		(- (* (first a ) (second b)) (* (second a ) (first b)))
	)
)
(define cross-product
	(lambda (a b)
		(list
			;cross product is i -j + k 
			(i-cross-product a b)
			;negate j
			(* (j-cross-product a b) -1)
			(k-cross-product a b) 
		)
	)
)

;#7
(define parallel? 
	(lambda (a b)
		;if two points are parallel, their cross product gives a 0 vector
		(if (equal? (cross-product a b) '(0 0 0))
			#t
			#f
		)
	)
)

;#8
;code taken from 1.ss
(define magnitude
	(lambda (a)
		(sqrt 
			(+ 
				(expt (first a) 2)
				(expt (second a) 2)
				(expt (third a) 2)
			)
		)
	)
)
(define make-vec-from-points
	(lambda (a b)
		(list 
			(- (first b) (first a))
			(- (second b) (second a))
			(- (third b) (third a))
		)
	)
)
;three points are collinear if the magnitude of the cross product of vectors from a to b and b to c equals 0 
(define collinear?
	(lambda (a b c)
		(if (zero? 
				(magnitude
					(cross-product 
						(make-vec-from-points a b)
						(make-vec-from-points b c)
					)
				)
			)
			#t
			#f
		)
	)
)

;#9
;code taken from 1.ss
(define distance 
	(lambda (a b)
		(sqrt
			(+
				(expt (- (first b) (first a)) 2)
				(expt (- (second b) (second a)) 2)
				(expt (- (third b) (third a)) 2)
			)
		)
	)
)
(define nearest-point 
	(lambda (a b)
		;use helper method to recurse and find smallest distance point
		;use first point in list as the current closest point, find distance and place into helper
		(nearest-help a (cdr b) (distance a (car b)) (car b))
	)
)
(define nearest-help
	(lambda (point l min_length closest)
		;recurse through list keeping track of the point, list, minimum length found, and closest point
		(if (null? l)
		;finished recursing through, return closest point 
			closest
			(cond
				;if this current point's distance from the test point is smaller than the minimum distance
				;then there is a new closest point, store it and its distance while recursing through the rest of the list
				[(< (distance point (car l)) min_length)
					(nearest-help 
						point 
						(cdr l) 
						(distance point (car l))
						(car l)
					)
				]
				[else
				;current point's distance is not a new minimum, move onto rest of the list, keep other values the same
					(nearest-help 
						point 
						(cdr l) 
						min_length
						closest
					)
				]
			)
		)
	)
)