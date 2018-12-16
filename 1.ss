;Alain Kouassi
;CSSE 304
;Assignment 01

;#1
(define interval-contains?
	(lambda (a b)
	(if (>= b (car a)) 
		(if (<= b (cadr a)) 
			;if the value is greater than or equal to the minimum value of the interval
			;and less than or equal to the maximum value of the interval then
			;the interval contains the value
			#t 
			#f) 
		#f)))

;#2
(define interval-intersects?
	(lambda (a b)
	(if (<= (car b) (car a))
		;if the minimum value of interval b is smaller than 
		;the minimum of interval a and the maximum of interval b
		;is greater than the minimum of interval a then there is an intersect
		;#t
		(if (>= (cadr b) (car a))
			#t
			#f)
		;if the minimum value is not smaller, than the only other way for an 
		;intersect to occur is for the minimum of one to be contained by the other
		;begin interval-contains? code
		(if (>= (car b) (car a)) 
			(if (<= (car b) (cadr a)) 
				#t 
				#f) 
			#f))))

;#3
(define interval-union
	(lambda (a b)
		(if (interval-intersects? a b)	
			(if (<= (car a) (car b))
				;case if the minimum value of a is less than or equal to the minimum value of b
				(if (>= (cadr a) (cadr b))
					;maximum value of a is greater than or equal to the maximum value of b 
					(list (list (car a) (cadr a)))
					;maximum value of b is greater than the maximum value of a
					(list (list (car a ) (cadr b)))
					)
				;case if the minimum value of b is less than the minimum value of a
				(if (>= (cadr b) (cadr a))
					;maximum value of b is greater than or equal to the maximum value of a
					(list (list (car b) (cadr b)))
					;maximum value of a is greater than the maximum value of b 
					(list (list (car b) (cadr a)))
					)
				)
			;intervals do not intersect, union will be both intervals
			(list a b)
		)
	)
)

;#4
(define first
	(lambda (a)
		(car a)
	)
)

(define second
	(lambda (a)
		(cadr a)
	)
)

(define third
	(lambda (a)
		(caddr a)
	)
)

;#5
(define make-vec-from-points
	(lambda (a b)
		(list 
			(- (first b) (first a))
			(- (second b) (second a))
			(- (third b) (third a))
		)
	)
)

;#6
(define dot-product
	(lambda (a b)
		(+
			(* (first a) (first b))
			(* (second a) (second b))
			(* (third a) (third b))
		)
	)
)

;#7
(define magnitude
	(lambda (a)
		(sqrt 
			(+ 
				(expt (first a) 2)
				(expt (second a) 2)
				(expt (third a) 2)
			)
		)
	)
)

;#8
(define distance 
	(lambda (a b)
		(sqrt
			(+
				(expt (- (first b) (first a)) 2)
				(expt (- (second b) (second a)) 2)
				(expt (- (third b) (third a)) 2)
			)
		)
	)
)