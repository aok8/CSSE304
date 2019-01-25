;Alain Kouassi
;CSSE304
;Assignment15


(define apply-k
	(lambda (k vals)
		(if (procedure? k) 
			(k vals)
			(error 'apply-k "Error - k is not a procedure" k))))
(define make-k (lambda (k) k))

;1a
(define member?-cps
	(lambda (item lst continuation)
		(cond
			[(null? lst) (apply-k continuation #f)]
			[(equal? item (car lst)) (apply-k continuation #t)]
			[else (member?-cps item (cdr lst) continuation)])))
;1b
(define set?-cps
	(lambda (lst continuation)
		(cond
			[(null? lst) (apply-k continuation #t)]
			[(not (pair? lst)) (apply-k continuation #f)]
			[else (member?-cps (car lst) (cdr lst) (make-k (lambda (is-member?)
				(if is-member? (apply-k continuation #f) (set?-cps (cdr lst) continuation)))))])))

;1c
(define set-of-cps 
	(lambda (l k)
		(cond 
			[(null? l) (apply-k k '())]
			[(member?-cps (car l) (cdr l) k) (set-of-cps (cdr s) k)]
			[else (cons (car l) (set-of (cdr l) k))])))

(define 1st-cps
	(lambda (l k)
		(apply-k k (car l))))

;1d
(define map-cps
	(lambda (proc-cps l k)
		(if (null? l)
			(apply-k k '())
			(map-cps proc-cps (cdr l) (make-k (lambda (rest) (proc-cps (car l) (make-k (lambda (answer) (apply-k k (cons answer rest)))))))))))

(define domain-cps
	(lambda (rel k )
		(set-of-cps (map-cps (car rel)))))

(define make-cps
	(lambda (proc)
		(lambda (l k) (apply-k k (proc l)))))

;1e
(define andmap-cps
	(lambda (pred-cps l k)
		(if (null? l)
			(apply-k k #t)
			(pred-cps (car l) (make-k (lambda (car-output) (if car-output (andmap-cps pred-cps (cdr l) k) (apply-k k #f))))))))

(define +-cps
	(lambda (a b k)
		(apply-k k (+ a b))))
(define rev-cps
	(lambda (a b k)
		(apply-k k (append b (list a)))))
(define max-cps
	(lambda (a b k)
		(apply-k k (max a b))))

(define cps-snlist-recur
	(lambda (base-value item-proc-cps list-proc-cps)
		(letrec ([helper (lambda (ls k)
				(if (null? ls)
					base-value
					(let check ([c (car ls)])
						(if (or (pair? c) (null? c))
							(list-proc-cps (helper c k) (helper (cdr ls) (make-k (lambda (x) x))) (make-k (lambda (x) (apply-k k x))))
							(item-proc-cps c (helper (cdr ls) (make-k (lambda (x) x))) (make-k (lambda (x) (apply-k k x))))
						)
					)
				)
			)]) helper)))


(define sn-list-sum-cps
	(lambda (ls k)
		((cps-snlist-recur 0 +-cps +-cps) ls k)
	)
)
(define sn-list-reverse-cps
	(lambda (ls k)
		((cps-snlist-recur '() rev-cps rev-cps) ls k)
	)
)

(define sn-list-depth-cps
	(lambda (ls k)
		((cps-snlist-recur 
			1 
			;max-cps
			;max-cps 
			(make-k (lambda (x y z) (max-cps (+ 1 x) y z)))
			(make-k (lambda (x y z) (apply-k z y)))
		) ls k)
	)
)
