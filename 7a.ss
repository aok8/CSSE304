;Alain Kouassi
;CSSE304
;Assignment7a

;#1
(define vector-append-list
	(lambda (vec ls)
		(let ([new-vector (make-vector (+ (vector-length vec) (length ls)))])
			(copy-from-vector new-vector vec 0)
			(copy-from-list new-vector ls (vector-length vec)) new-vector)))

(define copy-from-vector
	(lambda (vector1 vector2 i)
		(cond
			[(not (>= i (vector-length vector2)))
				(vector-set! vector1 i (vector-ref vector2 i))
				(copy-from-vector vector1 vector2 (+ i 1))])))

(define copy-from-list
	(lambda (vector lst i)
		(cond
			[(not (null? lst)) 
				(vector-set! vector i (car lst)) 
				(copy-from-list vector (cdr lst) (+ i 1))])))

;#2
(define group-by-two
	(lambda (lst)
		(group-helper lst (list))))

(define group-helper
	(lambda (lst acc)
		(cond
			[(null? lst) acc]
			[(null? (cdr lst)) (append acc (list (list (car lst))))]
			[else (group-helper (cddr lst) (append acc (list (list (car lst)(cadr lst)))))])))

;#3
(define group-by-n
	(lambda (lst n)
		(group-by-n-helper lst n (list))))

(define group-by-n-helper
	(lambda (lst n acc)
		(cond
			[(null? lst) acc]
			[(equal? (get-ngroup lst n (list)) #f) (append acc (list lst))]
			[else (group-by-n-helper (get-return-ngroup lst n) n (append acc (list (get-ngroup lst n (list)))))])))

(define get-ngroup
	(lambda (lst n acc)
		(cond
			[(equal? n 0) acc]
			[(null? lst) #f]
			[else (get-ngroup (cdr lst) (- n 1) (append acc (list (car lst))))])))

(define get-return-ngroup
	(lambda (lst n)
		(cond
			[(equal? n 0) lst]
			[(null? lst) #f]
			[else (get-return-ngroup (cdr lst) (- n 1))])))

;#4
(define bt-leaf-sum
	(lambda (t)
		(if (list? t)
			(cond
				[(number? (car t)) (car t)]
				[(or (null? (cadr t)) (null? (caddr t))) (car t)]
				[(null? (cadr t)) (leaf-helper (cadr t))]
				[(null? (caddr t)) (leaf-helper (caddr t))]
				[else (+ (bt-leaf-sum (cadr t)) (bt-leaf-sum (caddr t)))]
			)
		t)))


(define bt-max
	(lambda (bt)
		(cond
			[(null? bt) 0]
			[(number? bt) bt]
			[(and (null? (cadr bt)) (null? (caddr bt))) 
				(if (number? (car bt))
					(car bt)
					'()
				)
			]
			[(null? (cadr bt)) (max (car bst) (bt-max (caddr bt)))]
			[(null? (caddr bt)) (max (car bst) (bt-max (cadr bt)))]
			[else 
				(if (number? (car bt))
					(max (car bt) (bt-max (cadr bt)) (bt-max (caddr bt)))
					(max (bt-max (cadr bt)) (bt-max (caddr bt))))])))

(define BST-element
	(lambda (bst)
		(car bst)))

(define BST-left
	(lambda (bst)
		(cond
			[(list? bst) (cadr bst)]
			[else '()])))

(define BST-right
	(lambda (bst)
		(cond
			[(list? bst) (caddr bst)]
			[else '()])))

(define empty-BST
	(lambda () '()))

(define empty-BST?
	(lambda (obj)
		(equal? (empty-BST) obj)))

(define BST-inorder-helper
	(lambda (bst)
		(cond
			[(null? bst) '()]
			[(not (list? bst)) '()]
			[(null? (cdr bst))
				(list)
			]
			[(empty-BST? (BST-left bst)) (append (list (car bst)) (BST-inorder-helper (BST-right bst)))]
			[else
				(cond
					[(null? (BST-left bst)) (append (list (car bst)) (BST-inorder-helper(BST-right bst)))]
					[(null? (BST-right bst)) (append (BST-inorder-helper (BST-left bst)) (list (car bst)))]
					[else (append (append (BST-inorder-helper (BST-left bst)) (list (car bst))) (BST-inorder-helper(BST-right bst)))])])))

(define bt-inorder-list
	(lambda (bst)
		(cond
			[(null? bst) '()]
			[(empty-BST? bst) '()]
			[(not (list? bst)) '()]
			[(null? (cdr bst)) bst]
			[(null? (BST-left bst))
				(if (or (null? (BST-right bst)) (equal? (empty-BST) (BST-right bst))) (car bst) (append (list (car bst)) (BST-inorder-helper (BST-right bst))))
			]
			[(null? (BST-right bst))
				(if (or (null? (BST-left bst)) (equal? (empty-BST) (BST-left bst))) (car bst) (append (BST-inorder-helper (BST-left bst)) (list (car bst))))
			]
			[
				else (append (BST-inorder-helper (BST-left bst)) (list (car bst)) (BST-inorder-helper (BST-right bst)))
			])))

(define bt-max-interior
	(lambda (bt)
		(car (bt-max-interior-helper bt))))

(define bt-max-interior-helper
	(lambda (bt)
		(cond
			[(number? bt) (list '() bt bt)]
			[else
				(let(
						(right (bt-max-interior-helper (BST-right bt)))
						(left (bt-max-interior-helper (BST-left bt)))
					)
					(max-subtree (list (BST-element bt) (+ (caddr left) (caddr right))) left right))])))

(define max-subtree
	(lambda (center left right)
		(cond
			[(and (null? (BST-element left)) (null? (BST-element right)))
				(list (car center) (cadr center) (cadr center)) 
			]
			[(null? (car left)) 
				(list (car (max-of-two-trees right center)) (cadr (max-of-two-trees right center)) (cadr center))
			]
			[(null? (car right))
				(list (car (max-of-two-trees left center)) (cadr (max-of-two-trees left center)) (cadr center))
			]
			[(max-of-three-trees left center right)
				(list (BST-element left) (cadr left) (cadr center))
			]
			[(max-of-three-trees right center left)
				(list (BST-element right) (cadr right) (cadr center))
			] 
			[else (list (car center) (cadr center) (cadr center))])))

(define max-of-two-trees
	(lambda (checkTree t2)
		(if (< (cadr t2) (cadr checkTree))
			checkTree
			t2
		)))

(define max-of-three-trees
	(lambda (checkTree t2 t3)
		(and (>= (cadr checkTree) (cadr t2)) (>= (cadr checkTree) (cadr t3)))))