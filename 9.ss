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
(define sn-list-sum 
	(snlist-recur 0 + +)
)
(define sn-list-map 
	(lambda (proc slist)
		((snlist-recur '() cons (lambda (x y) (cons (proc x) y))) slist)
	)
)