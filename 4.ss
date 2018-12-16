;Alain Kouassi
;CSSE304
;Assignment#4

;#1
(define get-item
	(lambda (list position)
		(cond
			[(null? list) null]
			[(<= position (length list)) (get-item-helper list position 0)])))

(define get-item-helper
	(lambda (list position acc)
		(if (equal? position acc)
			(car list)
			(get-item-helper (cdr list) position (+ acc 1)))))
(define matrix-ref
	(lambda (m row col)
		(if (and (<= row (length m)) (<= col (length (car m)))) (get-item (get-item m row) col) null)))

;#2
(define matrix?
	(lambda (m)
		(cond
			[(null? m) #f]
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

;#3
(define matrix-transpose
	(lambda (m)
		(apply map list m)))

;#4
(define filter-in 
	(lambda (condition 1st)
		(filter-in-helper condition 1st (list))))
(define filter-in-helper
	(lambda (condition 1st acc)
		(cond
			[(null? 1st) acc] 
			[(condition (car 1st)) (filter-in-helper condition (cdr 1st) (append acc (list (car 1st))))]
			[else (filter-in-helper condition (cdr 1st) acc) ])))

;#5
(define filter-out
	(lambda (condition 1st)
		(filter-out-helper condition 1st (list))))
(define filter-out-helper
	(lambda (condition 1st acc)
		(cond
			[(null? 1st) acc] 
			[(not (condition (car 1st))) (filter-out-helper condition (cdr 1st) (append acc (list (car 1st))))]
			[else (filter-out-helper condition (cdr 1st) acc) ])))

;#6
(define invert
	(lambda (1st)
		(invert-helper 1st (list))))
(define invert-helper
	(lambda (1st acc)
		(cond
			[(null? 1st) acc]
			[else (invert-helper (cdr 1st) (append acc (list (reverse (car 1st)))))])))

;#7
(define list-index
	(lambda (pred ls)
		(list-index-helper pred ls 0)))
(define list-index-helper
	(lambda (pred ls acc)
		(cond
			[(null? ls) #f]
			[(pred (car ls)) acc]
			[else (list-index-helper pred (cdr ls) (+ acc 1))])))

;#8
(define fact
	(lambda (n)
		(if (negative? n) 
			"error"
			(fact-acc n 1))))
(define fact-acc
	(lambda (n acc)
		(if (zero? n)
			acc
			(fact-acc (- n 1) (* n acc)))))
(define choose
	(lambda(n k)
		(cond
			[(equal? k 0) 1]
			[(equal? n k) 1]
			[else
				(/ 
					(fact n)
					(*
						(fact k)
						(fact (- n k))))])))
(define pascal-triangle
	(lambda (n)
		(cond 
			[(negative? n) '()]
			[else (pascal-helper n (list))])))
(define pascal-helper
	(lambda (n acc)
		(cond
			[(< n 0) acc]
			[else (pascal-helper (- n 1) (append acc (list (choose-helper n 0))))])))
(define choose-helper
	(lambda (n acc)
		(cond
			[(<= n acc) (list 1)]
			[else (cons (choose n acc) (choose-helper n (+ acc 1)))])))