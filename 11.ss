;Alain Kouassi
;CSSE304
;Assignment11
;#1a

(define-syntax my-let
	(syntax-rules ()
		[(_ ((x v) ...) e1 e2 ...)
			((lambda (x ...) e1 e2 ...)
				v ...)]
		[(_ name ((x v) ...) e1 e2 ...)
			(letrec ((name (lambda (x ...) e1 e2 ...)))
				(name v ...))]))

;#1b
(define-syntax my-or
	(syntax-rules ()
		[(_) #f]
		[(_ e1) e1]
		[(_ e1 e2 ...)
			(let ([temp e1])
				(if temp temp (my-or e2 ...)))]))
;#1c
;used define-syntax ++ to fully understand how += should be done
(define-syntax +=
  (syntax-rules ()
    [(_ x v) (begin (set! x (+ x v)) x)]))

;#1d
(define-syntax return-first 
	(syntax-rules ()
		[(_ x)
			x]
		[(_ x e1)
			(let ([temp x ] [e e1])
				e
				(return-first x))]
		[(_ x e1 e2 ...)
			(let ([temp x])
				e1 
				(return-first temp e2 ...))]))
;#2
(load "chez-init.ss")
(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left-tree bintree?)
   (right-tree bintree?)))

(define bintree-to-list
	(lambda (tree)
		(cases bintree tree
			(leaf-node (value) (list (quote leaf-node) value))
			(interior-node (sym left right) 
				(list 
					(quote interior-node )
					sym 
					(bintree-to-list left) 
					(bintree-to-list right))))))

;#3
(define max-interior
	(lambda (tree)
		(cases bintree tree
		(leaf-node (value) (list value))
		(interior-node (sym left right) (caadr (interior-helper tree)))
		)
	)
)
(define interior-helper
	(lambda (tree)
		(cases bintree tree
		(leaf-node (value) (list value '()))
		(interior-node (sym left right)
			(cases bintree left
				(leaf-node (value_L)
					(cases bintree right
						(leaf-node (value_R)
							(let ([sum (+ value_L value_R)])
								(list sum (list sym sum))))

						(interior-node (sym_R left_R right_R)
							(let* ([result_R (interior-helper right)][sum (+ value_L (cadr (cadr result_R)))])
								(cond
									[(>= sum (cadr (cadr result_R))) (list sum (list sym sum))]
									[else (list sum (cadr result_R))]))))
				)
				(interior-node (sym_L left_L right_L)
					(cases bintree right
						(leaf-node (value_R)
							(let* ([result_L (interior-helper left)] [sum (+ value_R (car result_L))])
								(cond
									[(>= sum (cadr (cadr result_L))) (list sum (list sym sum))]
									[else (list sum (cadr result_L))])))

						(interior-node (sym_R left_R right_R)
							(let* ([result_L (interior-helper left)] 
									[result_R (interior-helper right)] 
									[sum (+ (car result_L) (car result_R))])
								(cond
									[(and (>= sum (cadr (cadr result_R))) (>= sum (cadr (cadr result_L)))) 
										(list sum (list sym sum))]
									[(>= sum (cadr (cadr result_R)))
										(list sum (cadr result_L))]
									[(>= sum (cadr (cadr result_L)))
										(list sum (cadr result_R))]
									[else (if (>= (cadr (cadr result_L)) (cadr (cadr result_R)))
											(list sum (cadr result_L))
											(list sum (cadr result_R)))]))))))))))