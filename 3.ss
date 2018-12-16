;Alain Kouassi
;CSSE304
;Assignment#

;#1
(define contains?
	(lambda (x list)
		(if (null? list)
			#f
			(if (equal? x (car list))
				#t
				(contains? x (cdr list))))))

(define intersection
	(lambda (list1 list2)
		(cond
			;List1 is empty, nothing left to check, return list1 to finish list
			[(null? list1) list1]
			
			;if value is in both lists, add to head of return list
			[(contains? (car list1) list2) (cons (car list1) (intersection (cdr list1) list2))]
			
			;value is not in both lists, skip
			[else (intersection (cdr list1) list2)])))

;#2
(define subset?
	(lambda (set1 set2)
		(cond

			;null set is a subset of all sets
			[(null? set1) #t]

			;if set2 is null, set1 cannot be a subset
			[(null? set2) #f]

			;value in set1 is not in set2, not a subset
			[(not (contains? (car set1) set2)) #f]

			;value in set1 is in set2, currently still a subset, continue to next value
			[else (subset? (cdr set1) set2)])))

;#3
(define set? 
	(lambda(list)
		(if (null? list)
			;if list is empty no duplicates so it is a set
			#t
			;list is not empty, use helper to have better efficiency
			(set-check list))))
(define set-check
	(lambda(list)
		(if (null? list)
			;reached end of list without finding duplicates
			#t
			(if (contains? (car list) (cdr list))
				;found duplicate of current value later in the list
				#f
				;did not find duplicate of current value, check again with next value and shorter list
				(set-check (cdr list))))))

(define relation?
	(lambda (a)
		(cond

			;if a is either not a list or not a set it can't be a relation
			[(not (list? a)) #f]
			[(not (set? a)) #f]

			;if a is null it can still be a relation
			[(null? a) #t]

			;if first value is a point (2 values) check rest
			[(point? (car a) 0) (relation? (cdr a))]

			;not a point, cant be a relation
			[else #f])))

(define point?
	(lambda (point acc)
		(cond
			;count number of values, should be 2 for a point
			[(not (list? point)) #f]
			[(null? point) (if (equal? acc 2) #t #f)]
			[else (point? (cdr point) (+ acc 1))])))

;#4
(define domain
	(lambda (a)
		;if it's a valid relation a domain can be found
		(if (relation? a) (domain-help a '()) #f)))
(define domain-help
	(lambda (a acc)
		(cond
			;if null return domain
			[(null? a) acc]

			;make sure current domain does not contain the current value before adding
			[(not (contains? (car (car a)) acc)) (domain-help (cdr a) (cons (car (car a)) acc))]
			
			;current domain contains value, skip it
			[else (domain-help (cdr a) acc)])))

;#5
(define range
	(lambda (a)
		(if (relation? a) (range-help a '()) #f)))
(define range-help
	(lambda (a acc)
		(cond
			[(null? a) acc]
			[(not (contains? (cadr (car a)) acc)) (range-help (cdr a) (cons (cadr (car a)) acc))]
			[else (range-help (cdr a) acc)])))

(define reflexive?
	(lambda (a)
		(cond
			;if not a relation cant be reflexive
			[(not (relation? a)) #f]
			
			;if null it is reflexive
			[(null? a) #t]

			; call helper storing the domain and range
			[else (reflexive-helper a (domain a) (range a))])))
(define reflexive-helper
	(lambda (a domain range)
		(cond
			;if reach 
			[(null? a) #f]

			;if the set contains all permutations of the domain and range it is reflexive
			[(null? domain) #t]
			[(and 
					(contains? 
							(list 
								(car domain) 
								(car domain)
							)
							a
					)
					(contains?
							(list
								(car range)
								(car range)
							)
							a
					)
				) (reflexive-helper a (cdr domain) (cdr range))
			]
			[else #f])))

;#6
(define hailstone-step-count
	(lambda (number)
		(if (equal? number 1) 0 (hailstone-acc number 0))))
(define hailstone-acc
	(lambda (number acc)
		(cond
			[(equal? number 1) acc]
			[(equal? (modulo number 2) 0) (hailstone-acc (/ number 2) (+ 1 acc))]
			[(equal? (modulo number 2) 1) (hailstone-acc (+ (* number 3) 1) (+ 1 acc))])))

;#7
(define multi-set?
	(lambda (set)
		(cond 
			;if it's not a set or a list or a relation it cant be a multi set
			[(not (and (and (list? set) (set? set))(relation? set))) #f]
			
			; if the domain list is a set and the rang is only integers it is a multi set
			[else
				(if (set? (domain-acc set '())) (multi-set-helper set) #f )
			])))
(define multi-set-helper
	(lambda (set)
		(cond
			[(not (relation? set)) #f ]
			[(null? set) #t]
			[(and (symbol? (car (car set))) (and (integer? (cadr (car set))) (positive? (cadr (car set))))) (multi-set-helper (cdr set))]
			[else #f])))
(define domain-acc
	(lambda (set acc)
		(if (null? set) acc (domain-acc (cdr set) (cons (car (car set)) acc)))))

;#8
(define ms-size
	(lambda (ms)
		(if (null? ms) 0 (ms-size-acc ms 0))))
(define ms-size-acc
	(lambda (ms acc)
		(cond 
			[(null? ms) acc]
			[else (ms-size-acc (cdr ms) (+ acc (cadr (car ms))))])))

;#9
(define last
	(lambda (list)
		(cond
			[(null? list) '()]
			[(null? (cdr list)) (car list)]
			[else (last (cdr list))])))

;#10
(define all-but-last 
	(lambda (list)
		(cond
			;could add all but last by taking first value of list and cons with rest of list till last value.
			[(null? list) '()]
			[(null? (cdr list)) '()]
			[else (cons (car list) (all-but-last (cdr list)))])))