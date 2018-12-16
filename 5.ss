;Alain Kouassi
;CSSE304
;Assignment# 5

;#1
(define interval-contains?
	(lambda (a b)
	(if (>= b (car a)) 
		(if (<= b (cadr a))
			#t 
			#f) 
		#f)))
(define interval-intersects?
	(lambda (a b)
	(if (<= (car b) (car a))
		(if (>= (cadr b) (car a))
			#t
			#f)
		(if (>= (car b) (car a)) 
			(if (<= (car b) (cadr a)) 
				#t 
				#f) 
			#f))))
(define interval-union
	(lambda (a b)
		(if (interval-intersects? a b)	
			(if (<= (car a) (car b))
				(if (>= (cadr a) (cadr b))
					(list (list (car a) (cadr a)))
					(list (list (car a ) (cadr b)))
					)
				(if (>= (cadr b) (cadr a))
					(list (list (car b) (cadr b)))
					(list (list (car b) (cadr a)))
					)
				)
			(list a b))))
(define minimize-interval-list
	(lambda (ls)
		(define sortedList (list-sort interval-comp ls))
		(interval-helper (cdr sortedList) (list) (list (car sortedList)))))

(define interval-helper
	(lambda (ls final acc)
		(cond 
			[(null? ls) (append final acc)]
			[(interval-intersects? (car acc) (car ls)) 
			(interval-helper (cdr ls) final (interval-union (car acc) (car ls)))]
			[else (interval-helper (cdr ls) (append final acc) (list (car ls)))])))
(define interval-comp
	(lambda (a b)
		(cond
			[(<= (car a ) (car b)) #t]
			[else #f])))

;#2
(define exists?
	(lambda (pred lst)
		(cond
			[(null? lst) #f]
			[(pred (car lst)) #t]
			[else (exists? pred (cdr lst))])))

;#3
(define vector-index
	(lambda (pred v)
		(vector-index-helper pred v 0)))
(define vector-index-helper
	(lambda (pred v acc)
		(cond
			[(>= acc (vector-length v)) #f]
			[(pred (vector-ref v acc)) acc]
			[else (vector-index-helper pred v (+ acc 1))])))


;#4
(define product
	(lambda (set1 set2)
		(product-helper set1 set2 (list))))
(define product-helper
	(lambda (set1 set2 acc)
		(cond
			[(null? set1) acc]
			[else (product-helper (cdr set1) set2 (append acc (pair (car set1) set2 (list))))])))
(define pair
	(lambda (value set1 acc)
		(cond
			[(null? set1) acc]
			[else (pair value (cdr set1) 
				(append acc (list (append (list value) (list (car set1))))))])))

;#5
(define replace
	(lambda (x y lst)
		(replace-helper x y lst (list))))
(define replace-helper
	(lambda (x y lst acc)
		(cond
			[(null? lst) acc]
			[(equal? x (car lst)) (replace-helper x y (cdr lst) (append acc (list y)))]
			[else (replace-helper x y (cdr lst) (append acc (list (car lst))))])))
;#6
(define last
	(lambda (lst)
		(cond
			[(null? lst) '()]
			[(null? (cdr lst)) (car lst)]
			[else (last (cdr lst))])))
(define all-but-last 
	(lambda (lst)
		(cond
			[(null? lst) '()]
			[(null? (cdr lst)) '()]
			[else (cons (car lst) (all-but-last (cdr lst)))])))
(define remove-last
	(lambda (element lst)
		(remove-last-helper element lst (list))))
(define remove-last-helper
	(lambda (element lst acc)
		(cond
			[(null? lst) acc]
			[(equal? element (last lst)) 
				(remove-last-helper '() 
				(all-but-last lst) 
				acc)
			]
			[else (remove-last-helper element 
				(all-but-last lst) 
				(append (list (last lst)) acc))])))