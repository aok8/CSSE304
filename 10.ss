;Alain Kouassi
;CSSE304
;Assignment#10

;#1
(define free-vars
  (lambda (exp)
    (cond
      [(null? exp) '()]
      [(symbol? exp) (list exp)]
      [(equal? (car exp) 'lambda)
        (cond
          [(symbol? (caddr exp))
            (if (not (member (caddr exp) (cadr exp)))
              (list (caddr exp))
              '())]
          [else 
            (if (equal? (caaddr exp) 'lambda)
              (free-vars (caddr exp))
              (free-helper (caddr exp) (cadr exp)))])]
      [else
        (if (and (equal? (car exp) (cadr exp)) (symbol? (car exp)))
          (list (car exp))
          (append (free-vars (car exp)) (free-vars (cadr exp))))])))

(define free-helper
  (lambda (a b)
    (cond
      [(null? a) '()]
      [(equal? b (car a)) (free-helper (car a) b)]
      [else (append (list (car a)) (free-helper (cdr a) b))])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;originally had most of the checks in the recursive helper, found it made it a lot more difficult and unorganized than 
;just moving the checks to the main function
;old checks follow, they also failed the last test
      ;[(symbol? exp) (list exp)]
      ;[(and (symbol? (car exp)) (not (equal? (car exp) 'lambda))) (free-helper (cadr exp) (append free (list (car exp))))]
      ;[(equal? (car exp) 'lambda) 
      ;  (let ([var (cadr exp)][body (caddr exp)])
      ;    (if (list? body)
      ;      ;(append free (filter (lambda (x) (not (equal? x 'lambda))) (filter (lambda (x) (not (equal? x (car var)))) body)))
      ;      (append free (filter (lambda (x) (not (equal? x 'lambda))) (filter (lambda (x) (not (equal? x (car var)))) (list body))))
      ;      ;(append free (filter (lambda (x) (not (equal? x 'lambda))) (filter (lambda (x) (not (equal? x (car var)))) (free-helper body free))))
      ;    ) 
      ;  )
      ;]
;    )
;  )
;)

(define bound-vars
(lambda (exp)
    (cond
      [(null? exp) '()]
      [(symbol? exp) '()]
      [(equal? (car exp) 'lambda)
        (cond
          [(symbol? (caddr exp))
            (if (not (member (caddr exp) (cadr exp)))
              '()
              (list (caddr exp))
              )
          ]
          [(equal? (caaddr exp) 'lambda)
            (if (member (caddr (caddr exp)) (cadr exp))
              (list (caddr (caddr exp)))
              (bound-vars (caddr exp))
            )
          ]
          [(equal? (caadr (caddr exp)) 'lambda)
            (bound-vars (cadr (caddr exp)))
          ]
          [else 
            (bound-helper (caddr exp) (cadr exp))])]
      [else
        (if (symbol? (car exp))
            (bound-vars (cadr exp))
            (append (bound-vars (car exp)) (bound-vars (cadr exp)))
        )])))

(define bound-helper
  (lambda (a b)
    (cond
      [(null? a) '()]
      [(equal? b (car a)) (append (list (car a)) (bound-helper (cdr a) b))]
      [else (bound-helper (car a) b)])))

;#2
(define occurs-free?
  (lambda (var exp)
    (cond
      [(null? exp) #f]
      [(symbol? exp) (eqv? var exp)]
      [(equal? (car exp) 'lambda) 
       (and (not (member var (cadr exp))) (occurs-free? var (caddr exp)))
      ]
      
      [(equal? (car exp) 'set!)
        (equal? (caddr exp) var)
      ]
      [(equal? (car exp) 'let)
        (occurs-free? var (let->application exp))
      ]
      [(equal? (car exp) 'let*)
        (occurs-free? var (let*->let exp))
      ]
      [(equal? (car exp) 'if) (if (member var (cadr exp)) #t (occurs-free? var (caddr exp)))] 
      [else (or (occurs-free? var  (car exp))
                (occurs-free? var (cdr exp)))
      ])))

(define parameters
  (lambda (lst)
    (if (null? lst) 
      (list)
      (append (list (caar lst)) (parameters (cdr lst))))))
(define values
  (lambda (lst)
    (if (null? lst)
      (list)
      (append (list (cadr (car lst))) (values (cdr lst))))))
(define let->application
  (lambda (lst)
    (if (null? lst) 
      (list)
      (append (list (list (quote lambda) (parameters (cadr lst)) (caddr lst))) (values (cadr lst))))))
(define last
  (lambda (list)
    (cond
      [(null? list) '()]
      [(null? (cdr list)) (car list)]
      [else (last (cdr list))])))
(define let*->let
  (lambda (lst)
    (let-helper (cadr lst) (last lst))))
(define let-helper
  (lambda (lst x)
    (if (null? (cdr lst))
      (append (list (quote let) (list (car lst))) (list x))
      (append (list (quote let) (list (car lst))) (list (let-helper (cdr lst) x))))))

(define occurs-bound?
  (lambda (var exp)
    (cond
      [(null? exp) #f]
      [(symbol? exp) #f]
      [(eqv? (car exp) 'lambda)
       (or (occurs-bound? var (caddr exp))
           (and (not (not (member var (cadr exp))))
                (occurs-free? var (caddr exp))))
      ]
      [(equal? (car exp) 'let) (occurs-bound? var (let->application exp))]
      [(equal? (car exp) 'let*) (occurs-bound? var (let*->let exp))]
      [(equal? (car exp) 'if)
        (or (occurs-bound? var (caddr exp)) (occurs-bound? var (cadddr exp)))
      ]
      [else (or (occurs-bound? var  (car exp))
                (occurs-bound? var (cdr exp)))
      ])))


