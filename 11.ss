;Alain Kouassi
;CSSE304
;Assignment11

;#1a
(define-syntax my-let
	(syntax-rules ()
		[(_ ((x v) ...) e1 e2 ...)
			((lambda (x ...) e1 e2 ...)
				v ...)]))
;#1b

;#1c
;used define-syntax ++ to fully understand how += should be done
(define-syntax +=
  (syntax-rules ()
    [(_ x v) (begin (set! x (+ x v)))]))

;#1d
(define-syntax return-first 
	(syntax-rules ()
		[(_ x e1)
			(let ([temp x])
				e1
				temp)]))
