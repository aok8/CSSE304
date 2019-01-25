(define apply-k
	(lambda (k vals)
		(if (procedure? k) 
			(k vals)
			(error 'apply-k "Error - k is not a procedure" k))))
(define make-k (lambda (k) k))

;1
(define member?-cps
	(lambda (item lst continuation)
		(cond
			[(null? lst) (apply-k continuation #f)]
			[(equal? item (car lst)) (apply-k continuation #t)]
			[else (member?-cps item (cdr lst) continuation)])))
;2
(define set?-cps
	(lambda (lst continuation)
		(cond
			[(null? lst) (apply-k continuation #t)]
			[(not (pair? lst)) (apply-k continuation #f)]
			[else (member?-cps (car lst) (cdr lst) (make-k (lambda (is-member?)
				(if is-member? (apply-k continuation #f) (set?-cps (cdr lst) continuation)))))])))

;3
(define set-of-cps 
	(lambda (l k)
		(cond 
			[(null? l) (apply-k k '())]
			[(member?-cps (car l) (cdr l) k) (set-of-cps (cdr s) k)]
			[else (cons (car l) (set-of (cdr l) k))]
			;[(member?-cps (car l) (cdr l) (make-k (lambda (is-member?)
			;	(if is-member? (set-of-cps (cdr l) k))))]
		)
	)
)

(define 1st-cps
	(lambda (l k)
		(apply-k k (car l))
	)
)

(define map-cps
	(lambda (proc-cps l k)
		(if (null? l)
			(apply-k k '())
			(map-cps proc-cps (cdr l) (make-k (lambda (rest) (proc-cps (car l) (make-k (lambda (answer) (apply-k k (cons answer rest))))))))
			;(apply-k k (proc-cps (car l) (lambda (x) (cons x (map-cps proc-cps (cdr l) k)))))
		)
	)
)
(define domain-cps
	(lambda (rel k )
		(set-of-cps (map-cps (car rel)))
	)
)

(define make-cps
	(lambda (proc)
		(lambda (l k) (apply-k k (proc l)))
	)
)

(define andmap-cps
	(lambda (pred-cps l k)
		(if (null? l)
			(apply-k k #t)
			(pred-cps (car l) (make-k (lambda (car-output) (if car-output (andmap-cps pred-cps (cdr l) k) (apply-k k #f)))))
			;(andmap-cps pred-cps (cdr l) (make-k (lambda (rest) (pred-cps (car l) (make-k (lambda (car-output) (if car-output (apply-k k rest) (apply-k k #f))))))))
		)
	)
)
