;Alain Kouassi
;CSSE304
;Assignment #6b

;#8
(define parameters
	(lambda (lst)
		(if (null? lst) 
			(list)
			(append (list (caar lst)) (parameters (cdr lst))))))
(define values
	(lambda (lst)
		(if (null? lst)
			(list)
			(append (list (cadr (car lst))) (values (cdr lst))))))
(define let->application
	(lambda (lst)
		(if (null? lst) 
			(list)
			(append (list (list (quote lambda) (parameters (cadr lst)) (caddr lst))) (values (cadr lst))))))

;#9
(define last
	(lambda (list)
		(cond
			[(null? list) '()]
			[(null? (cdr list)) (car list)]
			[else (last (cdr list))])))

(define let*->let
	(lambda (lst)
		(let-helper (cadr lst) (last lst))))
(define let-helper
	(lambda (lst x)
		(if (null? (cdr lst))
			(append (list (quote let) (list (car lst))) (list x))
			(append (list (quote let) (list (car lst))) (list (let-helper (cdr lst) x))))))

;#10
(define qsort
	(lambda (pred lst)
		(if (null? lst)
			(list)
			(append 
				(qsort pred (list-filter-out (negative-test pred (car lst)) (cdr lst) (list)))
				(list (car lst))
				(qsort pred (list-filter-out (positive-test pred (car lst)) (cdr lst) (list)))))))
(define positive-test
	(lambda (pred pivot)
		(lambda (value)
			(pred pivot value))))
(define negative-test
	(lambda (pred pivot)
		(lambda (value)
			(not(pred pivot value)))))
(define list-filter-out
	(lambda (check lst acc)
		(cond
			[(null? lst) acc]
			[(check (car lst)) (list-filter-out check (cdr lst) (append acc (list (car lst))))]
			[else (list-filter-out check (cdr lst) acc)]
		)
	)
)

;#11
(define compare
	(lambda (x y)
		(string<? x y)))
(define sort-list-of-symbols
	(lambda (los)
		(map string->symbol (list-sort compare (map symbol->string los)))))

;#12
(define contains
	(lambda (value lst count)
		(cond
			[(null? lst) #f]
			[(equal? value (car lst)) count]
			[else (contains value (cdr lst) (+ count 1))])))
(define ribassoc
	(lambda (s los v fail-value)
		(cond
			[(equal? (contains s los 0) #f) fail-value]
			[else (vector-ref v (contains s los 0))])))