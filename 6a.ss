;Alain Kouassi
;CSSE304
;Assignment#6

;#1
(define curry2
	(lambda (op)
		(lambda (x)
			(lambda (y)
				(op x y)
			)	
		)

	)

)

;#2
(define curried-compose
	(lambda (x)
		(lambda (y)
			(lambda (z)
				(x (y z))
			)
		)
	)
)

;#3
(define compose
	(lambda list-of-functions
		(define comp-func
			(lambda (ls x)
				(if (null? ls) 
					x 
					((car ls) (comp-func (cdr ls) x)) 
				)
			)
		)
		(lambda (x)
			(comp-func list-of-functions x)
		)
	)
)

;#4
(define make-list-c
	(lambda (n)
		(define make-list
			(lambda (num x acc)
				(if (> num 0) 
					(make-list (- num 1) x (append (list x) acc))
					acc
				)
			)
		)
		(lambda (word)
			(make-list n word (list))
		)
	)
)

;#5
(define reverse-it
	(lambda (lst)
		(reverse-help lst (list))
	)
)
(define reverse-help
	(lambda (lst acc)
		(cond
			[(null? lst) acc]
			[else (reverse-help (cdr lst) (cons (car lst) acc))]
		)
	)
)

;#6
(define map-by-position
	(lambda (fn-list arg-list)
		(define apply-func
			(lambda (x y)
				(x y)
			)
		)
		(cond
			[(equal? (length fn-list) (length arg-list))
				(map apply-func fn-list arg-list)
			]
			[else '()]
		)
	)
)

;#7
(define empty-BST
	(lambda () '())
)


(define empty-BST?
	(lambda (obj)
		(equal? (empty-BST) obj)
	)
)
(define BST-left
	(lambda (bst)
		(cadr bst)
	)
)
(define BST-right
	(lambda (bst)
		(caddr bst)
	)
)
(define BST-element
	(lambda (bst)
		(car bst)
	)
)

(define BST-insert 
	(lambda (num bst)
		(cond
			[(empty-BST? bst) (list num)]
			[else 
				(cond
					[(equal? (car bst) num) bst]
					[(< num (car bst)) 
						(cond
							[(null? (cdr bst)) (list (car bst) (list num) (empty-BST))]
							[(empty-BST? (BST-left bst)) (list (car bst) (list num) (BST-right bst))]
							[else (list (car bst) (BST-insert num (BST-left bst)) (BST-right bst))]

						)
					]
					[(> num (car bst)) 
						(cond
							[(null? (cdr bst)) (list (car bst) (empty-BST) (list num))]
							[(empty-BST? (BST-right bst)) (list (car bst) (BST-left bst) (list num))]
							[else (list (car bst) (BST-left bst) (BST-insert num (BST-right bst)))]
						)
					]
				)
			]

		)
	)
)
(define BST-inorder-helper
	(lambda (bst)
		(cond
			[(null? bst) '()]
			[(null? (cdr bst))
				(list (car bst))
			]
			[(empty-BST? (BST-left bst)) (append (list (car bst)) (BST-inorder-helper (BST-right bst)))]
			[else
				(cond
					[(null? (BST-left bst)) (append (list (car bst)) (BST-inorder-helper(BST-right bst)))]
					[(null? (BST-right bst)) (append (BST-inorder-helper (BST-left bst)) (list (car bst)))]
					[else (append (append (BST-inorder-helper (BST-left bst)) (list (car bst))) (BST-inorder-helper(BST-right bst)))]
				) 
			]
		)
	)
)
(define BST-inorder
	(lambda (bst)
		(cond
			[(null? bst) '()]
			[(empty-BST? bst) '()]
			[(not (list? bst)) bst]
			[(null? (cdr bst)) bst]
			[(null? (BST-left bst))
				(if (or (null? (BST-right bst)) (equal? (empty-BST) (BST-right bst))) (car bst) (append (list (car bst)) (BST-inorder-helper (BST-right bst))))
			]
			[(null? (BST-right bst))
				(if (or (null? (BST-left bst)) (equal? (empty-BST) (BST-left bst))) (car bst) (append (BST-inorder-helper (BST-left bst)) (list (car bst))))
			]
			[
				else (append (BST-inorder-helper (BST-left bst)) (list (car bst)) (BST-inorder-helper (BST-right bst)))
			]
		)
	)
)
(define list-increasing
	(lambda (lst)
		(cond
			[(null? lst) #t]
			[(not (list? lst)) #t]
			[(null? (cdr lst)) #t]
			[else (if (> (car lst) (cadr lst)) #f (list-increasing (cdr lst)))]
		)
		; (if (null? lst) 
		; 	#t
		; 	(if (> (car lst) (cadr lst)) #f (list-increasing (cdr lst)))
		; )
	)
)
(define BST?
	(lambda (bst)
		(and (list? bst) (BST?-helper bst) (list-increasing (BST-inorder bst)))
	)
)

(define BST-insert-nodes
	(lambda (bst nums)
		(if (null? nums) 
			bst
			(BST-insert-nodes (BST-insert (car nums) bst) (cdr nums))
		)
	)
)
(define BST-contains?
	(lambda (bst num)
		(cond
			[(equal? (empty-BST) bst) #f]
			[(< num (car bst))
				(if (null? (cdr bst)) #f (BST-contains? (BST-left bst) num))
			]
			[(> num (car bst))
				(if (null? (cdr bst)) #f (BST-contains? (BST-right bst) num))
			]
			[(equal? num (car bst)) #t]
			[else #f]
		)
	)
)
(define BST?-helper
	(lambda (bst)
		(if (list? bst)
				(cond
					[(equal? (empty-BST) bst) #t]
					[(not (equal? (length bst) 3)) #f]
					[else 
						(if (number? (car bst))
							(cond
								[(null? (cdr bst)) #f]
								[(and (null? (BST-left bst)) (null? (BST-right bst)))
									#t
								]
								[(null? (BST-right bst)) 
									(if
									(and
										(< (car (cadr bst)) (car bst))
										(BST?-helper (BST-left bst))
									) 
									#t 
									#f)
								]
								[(null? (BST-left bst)) 
									(if 
										(and
											(> (car (caddr bst)) (car bst))
											(BST?-helper (BST-right bst))
										) 
										#t 
										#f)
								]
								[else (if 
									(and
										(and
											(< (cadr bst) (car bst))
											(> (caddr bst) (car bst))
										) 
										(and (BST?-helper (BST-left bst)) (BST?-helper (BST-right bst))) 
											
									)
									#t 
									#f
								)
								]
							)
							#f
						)
					]
			)
			#f
		)
	) 
)

(define BST-height
	(lambda (bst)
		(if (equal? (empty-BST) bst) 
			-1
			(cond 
				[(null? (cdr bst)) 0]
				[(null? (BST-right bst)) (+ 1 (BST-height (BST-left bst)))]
				[(null? (BST-left bst)) (+ 1 (BST-height (BST-right bst)))]
				[else (+ 1 (max (BST-height (BST-left bst)) (BST-height (BST-right bst))))]
			)
		)
	)
)