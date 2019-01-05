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
							;case: an interior node with two leaf nodes 
							;list the sum of the two leaf nodes and a list of the symbol of the tree and the sumw 
							(let ([sum (+ value_L value_R)])
								(list sum (list sym sum))))

						(interior-node (sym_R left_R right_R)
							;case: an interior node with a leaf node on the left and another interior node on the right
							;compare the sum with the sum of the interior nodee -
							;if interior node sum is greater than full sum, list full sum and list of symbol of interior node and its sum
							;if full sum is greater, list full sum and list of symbol of tree and full sum
							(let* ([result_R (interior-helper right)][sum (+ value_L (cadr (cadr result_R)))])
								(cond
									[(>= sum (cadr (cadr result_R))) (list sum (list sym sum))]
									[else (list sum (cadr result_R))]))))
				)
				(interior-node (sym_L left_L right_L)
					(cases bintree right
						(leaf-node (value_R)
							;case: an interior node with an interior node on the left and a leaf node on the right
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

;#4
;STARTING CODE
;------------------------------------------------
(load "chez-init.ss") ; put this file in the same folder, or add a pathname

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lambda-exp
   (id symbol?)
   (body expression?)]
  [app-exp
   (rator expression?)
   (rand expression?)])

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(number? datum) (lit-exp datum)]
     [(pair? datum)
      (cond
       [(eqv? (car datum) 'lambda)
	(lambda-exp (car (2nd  datum))
		    (parse-exp (3rd datum)))]
      [else (app-exp (parse-exp (1st datum))
		     (parse-exp (2nd datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))
; An auxiliary procedure that could be helpful.
(define var-exp?
 (lambda (x)
   (cases expression x
     [var-exp (id) #t]
     [else #f])))
(var-exp? (var-exp 'a))