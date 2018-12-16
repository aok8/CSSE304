;Alain Kouassi
;CSSE304
;Assignment8

;#1
;found this question significantly easier than #2
(define make-stack
	(lambda ()
 		(let ([stk '()])
 			(lambda (msg . args )
 				(case msg ; Scheme's case is a similar to switch in some other languages.
 					[(empty?) (null? stk)]
 					[(push) (set! stk (cons (car args) stk))]
 					[(pop) (let ([top (car stk)])
 							(set! stk (cdr stk))
 					top)]
 	[else (errorf 'stack "illegal message to stack object: ~a" msg)])))))

(define make-slist-leaf-iterator
	(lambda (lst)
		(let ([stk (make-stack)] )
			(stk 'push lst)
			(lambda (msg)
				(case msg
					[(next)
						(cond
							[(stk 'empty?) #f]
							[else 
								(let popper ([stk stk] [popped (stk 'pop)])
									(cond
										[(null? popped) 
											(if (stk 'empty?) 
												#f
												(popper stk (stk 'pop)))]
										[(list? popped) 
											(stk 'push (cdr popped))
											(popper stk (car popped))]
										[else popped]))])]
					[else (errorf 'slist-leaf-iterator "illegal message to iterator: ~a" msg)])))))

;#2
;(define subst-leftmost
;	(lambda (new old slist equality-pred?)
;		(if (null? slist) 
;			'() 
;			(trace-let checker ([current (car slist)][rest-of-list (cdr slist)])
;				(cond
;					[(and (null? rest-of-list) (not (list? current)))
;						(if (equality-pred? old current) (cons new '()) (cons current '()))
;					]
;					[(or (symbol? current) (null? current))
;						(if (equality-pred? old current) (cons new rest-of-list) (cons current (checker (car rest-of-list) (cdr rest-of-list))))
;					]
;					[(and (null? rest-of-list) (list? current)) (cons (checker (car current) (cdr current)) '())]
;					[(list? current) (cons (checker (car current) (cdr current)) (checker (car rest-of-list) (cdr rest-of-list)))] 
;				)
;			)
;		)
;	)
;)

;kept previous code, it worked for most tests but didnt try the other part of the list for the last test,
;had to rework it to get it to pass all tests. 
;had to get a decent amount of help, will probably need to talk to Dr. Anderson about this problem and max interior before the test 
(define subst-leftmost
	(lambda (new old slist equality-pred?)
		(let checker ([slist slist])
			(cond 
				[(null? slist) '()]
				[(and (symbol? (car slist)) (equality-pred? old (car slist))) (cons new (cdr slist))]
				[(symbol? (car slist)) 
					(let check ([end (checker (cdr slist))])
						(if (null? end)
							'()
							(cons (car slist) end)))]
				[else (let check ([start (checker (car slist))])
				 	(if (null? start)
				 		(cons (car slist) (checker (cdr slist)))
				 		(cons start (cdr slist))))]))))