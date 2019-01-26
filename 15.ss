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
			[else (member?-cps (car l) (cdr l)
				(make-k (lambda (is-member?)
					(if is-member? (set-of-cps (cdr l) k)
						(set-of-cps (cdr l)
							(make-k (lambda (rest) (apply-k k (cons (car l) rest))))
						)
					)
				))
		)])))

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
		(set-of-cps (map-cps 1st-cps rel (lambda (x) x)) k)))

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

(define max+1-cps
	(lambda (a b k)
		(apply-k k (max (+ a 1) b))
	)
)
(define max-cps
	(lambda (a b k)
		(apply-k k (max a b))
	)
)



(define cps-snlist-recur
	(lambda (base-value item-proc-cps list-proc-cps)
		(letrec ([helper (lambda (ls k)
				(if (null? ls)
					(apply-k k base-value)
					(trace-let test ([c (car ls)])
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
	(cps-snlist-recur '() rev-cps rev-cps)
)

(define sn-list-depth-cps
	(lambda (ls k)
		((cps-snlist-recur 
			1
			max-cps
			max+1-cps
		) ls k)
	)
)
(define occurs
	(lambda (a b val)
		(if (null? b)
			val
			(if (eqv? a (car b)) 
				(occurs a (cdr b) (+ 1 val))
				(occurs a (cdr b) val)
			)
		)
	)
)
(define occur-cps
	(lambda (a b k)
		(apply-k k (occurs a b 0))
	)
)
(define sn-list-occur-cps
	(lambda (item ls k)
		((cps-snlist-recur
			0
			(lambda (x y z) (apply-k k (if (equal? x item) (+ 1 y) y)))
			(lambda  (x y z)  (apply-k z (+ x y)))
		) ls k)
	)
)

;2
(define memoize
	(lambda (f hash equiv?)
		(let ([ht (make-hashtable hash equiv?)])
			(lambda (x . y)
				(let ([result (hashtable-ref ht (cons x y) #f)])
					(if (boolean? result)
						(let ([answer (apply f (cons x y))])
								(hashtable-set! ht (cons x y) answer)
								answer
						)	
						result))))))

;took a second to get, i implemented leftmost in a way where using let-values wasn't neccessary since i was returning 1 value constantly
(define subst-leftmost
	(lambda (new old slist equality-pred?)
		(let checker ([slist slist])
			(cond 
				[(null? slist) '()]
				[(and (symbol? (car slist)) (equality-pred? old (car slist))) (cons new (cdr slist))]
				[(symbol? (car slist)) 
					(let ([end (checker (cdr slist))])
						(if (null? end)
							(values '())
							(values (cons (car slist) end))))]
				[else (let ([start (checker (car slist))])
				 	(if (null? start)
				 		(values (cons (car slist) (checker (cdr slist))))
				 		(values (cons start (cdr slist)))))]))))