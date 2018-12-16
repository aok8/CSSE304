;Alain Kouassi
;CSSE304
;Assignment9

(define snlist-recur
	(lambda (base-case proc-list proc-other)
		(letrec
			([snlist-helper 
				(lambda (snlist)
					(cond 
						;if null return base case
						[(null? snlist) base-case]
						;first item is a list, return the list procedure on a recursive call of the start of list and rest
						[(list? (car snlist)) (proc-list (snlist-helper (car snlist)) (snlist-helper (cdr snlist)))]
						;first item is not a list, return the non-list procedure on the start of list and recurse on rest of list
						[else (proc-other (car snlist) (snlist-helper (cdr snlist)))]))
					])
			snlist-helper)))

;#1a
(define sn-list-sum 
	(snlist-recur 0 + +)
)

;#1b
(define sn-list-map 
	(lambda (proc slist)
		((snlist-recur '() cons (lambda (x y) (cons (proc x) y))) slist)
	)
)

;#1c
;dont want to return any symbols or values, just values for lists
(define paren-helper
	(lambda (a b)
		b
	)
)
(define sn-list-paren-count
	(snlist-recur 2 + paren-helper))

;#1d
(define dneppa
	(lambda (a b)
		(append b (list a))))
(define sn-list-reverse
	(snlist-recur '() dneppa dneppa))

;#1e
(define sn-list-occur 
	(lambda (s snlst)
		((snlist-recur 
			0 
			+ 
			(lambda (x y) 
				(cond
					[(eq? x s) (+ 1 y)]
					[else (+ 0 y)]))) snlst)))

(define sn-list-depth
	(snlist-recur 
		1 
		(lambda (x y)
			(max (+ 1 x) y))
		(lambda (x y) y)))

;#2
(define bt-recur
	(lambda (proc-leaf proc-bt)
		;numbers are leaves, symbols are trees
		(letrec
			([bt-helper
				(lambda (bt)
					(cond
						[(number? bt) (proc-leaf bt)]
						[(symbol? (car bt)) (proc-bt (car bt) (bt-helper (cadr bt)) (bt-helper (caddr bt)))]))])
		bt-helper)))

(define bt-sum
	(lambda (bt)
		((bt-recur 
			(lambda (x) x)
			(lambda (x y z) (+ y z))) bt)))

(define bt-inorder
	(lambda (bt)
		((bt-recur
			;return null if found leaf
			(lambda (x) '())
			(lambda (x y z) 
				(cond
					[(and (list? y) (list? z)) (append y (list x) z)]
					[(list? y) (append y x)]
					[else (append x z)]))) bt)))