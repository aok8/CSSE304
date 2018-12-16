;Alain Kouassi
;CSSE304
;Assignment7b

;#5
(define (slist-map proc slist)
	(let x ([slist slist])
		(cond 
			[(null? slist) '()]
			[(symbol? (car slist)) (cons (proc (car slist)) (x (cdr slist)))]
			[else (cons (x (car slist)) (x (cdr slist)))])))

(define (slist-reverse slist)
	(let rev ([slist slist])
		(cond
			[(null? slist) '()]
			[(symbol? (car slist)) (append (rev (cdr slist)) (list (car slist)))]
			[else (append (rev (cdr slist)) (list (rev (car slist))))])))
(define (slist-paren-count slist)
	(+ 2(let paren ([slist slist])
		(cond
			[(null? slist) 0]
			[(symbol? (car slist)) (+ 0 (paren (cdr slist)))]
			[else (+ 2 (paren (car slist)) (paren (cdr slist)))]))))

(define (slist-depth slist)
	(let total ([slist slist] [depth 1])
		(cond
			[(null? slist) depth]
			[(symbol? (car slist)) (total (cdr slist) depth)]
			[else (max (total (car slist) (+ depth 1)) (total (cdr slist) depth))])))

(define (slist-symbols-at-depth slist d)
(let symbols ([slist slist] [depth 1])
	(cond
			[(null? slist) '()]
			[(and (symbol? (car slist)) (= d depth)) (append (list (car slist)) (symbols (cdr slist) depth))]
			[(symbol? (car slist)) (symbols (cdr slist) depth)]
			[else (append (symbols (car slist) (+ depth 1)) (symbols (cdr slist) depth))])))
;#6
(define (path-to slist symbol)
	(let path ([slist slist] [lst '()])
		(cond
			[(null? slist) #f]
			[(and (symbol? (car slist)) (equal? symbol (car slist))) (append lst '(car) )]
			[(symbol? (car slist)) (path (cdr slist) (append lst '(cdr)))]
			;(path (car slist) (append lst '(car))) (path (cdr slist) (append lst '(cdr)))
			[else (or (path (car slist) (append lst '(car))) (path (cdr slist) (append lst '(cdr))))])))


;#7
(define compose
 	(case-lambda
 	[() (lambda (x) x)]
 	[(first . rest)
 	(let ([composed-rest (apply compose rest)])
 	(lambda (x) (first (composed-rest x))))]))
(define make-c...r
	(lambda (proc)
		(apply compose (map eval (map checker (string->list proc))))))
(define checker
	(lambda (proc)	
		(cond
			[(equal? #\a proc) 'car]
			[(equal? #\d proc) 'cdr])))