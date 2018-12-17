;Alain Kouassi
;CSSE304
;Exam 1

(define path-to
  (lambda (slist sym)
    (let pathto ([slist slist] [path-so-far '()])
      (cond [(null? slist) #f]
	    [(eq? (car slist) sym)
	     (reverse (cons 'car path-so-far))]
            [(symbol? (car slist))
	     (pathto (cdr slist) (cons 'cdr path-so-far))]
	    [else
	     (or (pathto (car slist) 
                         (cons 'car path-so-far))
		 (pathto (cdr slist) 
                         (cons 'cdr path-so-far)))]))))

; Put yout Problem 1 solution here
;(define matrix-ref
;	(lambda (m row col)
;		(if (and (<= row (length m)) (<= col (length (car m)))) (get-item (get-item m row) col) null)))
;(define symmetric?
;	(lambda (m)
;		(let ([i (length m)] [j (length (car m))])
;			(and )
;		)
;	)
;)
(define matrix?
	(lambda (m)
		(cond
			[(null? m) #t]
			[(and (list? m) (not (null? (car m))) (list? (car m)) ) (matrix-check m (length (car m)))]
			[else #f])))
(define matrix-check
	(lambda (m lngth)
		(cond
			[(null? m) #t]
			[(and (integer-check (car m)) (equal? lngth (length (car m)))) (matrix-check (cdr m) lngth)]
			[else #f])))
(define integer-check
	(lambda (list)
		(cond
			[(null? list) #t]
			[(number? (car list)) (integer-check (cdr list))]
			[else #f])))



; Put yout Problem 2 solution here
(define (sum-of-depths slist)
	(let total ([slist slist] [depth 0])
		(cond
			[(null? slist) depth]
			[(symbol? (car slist)) (total (cdr slist) (+ depth 1))]
			[else (+ (total (car slist) (+ depth 1)) (total (cdr slist) depth))])))


; Put yout Problem 3 solution here
(define (notate-depth slist) 
  (let notate ([slist slist]
	       [depth 1])
    (cond [(null? slist) '()]
	  [(symbol? (car slist))
	   (cons (list (car slist))
		 (notate (cdr slist) depth))]
	  [else  ; the car is an s-list
	   (cons (notate (car slist) (+ 1 depth))
		 (notate (cdr slist) depth))])))
;(define un-notate
;	(lambda (slist)

;	)
;)
; Put yout Problem 4 solution here
(define find-by-path
	(lambda (pathlist slist)
		(cond
			[(null? pathlist) slist]
			[else (find-by-path (cdr pathlist) (applyinghelper (car pathlist) slist))]
		)
	)
)
(define applyinghelper
	(lambda (input slist)
		(cond
			[(equal? input 'car) (car slist)]
			[(equal? input 'cdr) (cdr slist)]
		)
	)
)

; Put yout Problem 5 solution here
;Couldn't think of an easier way to store the position, used a stack in a weird way but it works
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

(define make-vec-iterator
	(lambda (v)
		(let ([stk (make-stack)])
			(stk 'push 0)
			(lambda msg
				(case (car msg)
					[(val)
						(let ([pos (stk 'pop)])
							(stk 'push pos)
							(vector-ref v pos)
						)
					]
					[(next)
						;(check (+ pos 1))
						(let ([pos (stk 'pop)])
							(stk 'push (+ pos 1))
						)
					]
					[(prev)
						;(if (zero? pos)
						;	(check 0)
						;	(check (- pos 1))
						;)
						(let ([pos (stk 'pop)])
							(stk 'push (- pos 1))
						)
					]
					[(set-val!)
						;(lambda (val)
						;	(vector-set! v pos val)
						;)
						(let ([pos (stk 'pop)])
							(stk 'push pos)
							(vector-set! v pos (cadr msg))
						)	
					]
					[(pos)
						(let ([pos (stk 'pop)])
							(stk 'push pos)
							pos
						)
					]
				)
			)
		)
	)
)
