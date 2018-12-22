;Alain Kouassi
;CSSE304
;Assignment#10

;#1
;fails one test on the server, not quite sure why
(define free-vars
  (lambda (expr)
    (cond
      [(null? expr) '()]
      [(symbol? expr) (list expr)]
      [(equal? (car expr) 'lambda)
        (cond
          [(symbol? (caddr expr))
            (if (not (member (caddr expr) (cadr expr)))
              (list (caddr expr))
              '())]
          [else 
            (if (equal? (caaddr expr) 'lambda)
              (free-vars (caddr expr))
              (free-helper (caddr expr) (cadr expr)))])]
      [else
        (if (and (equal? (car expr) (cadr expr)) (symbol? (car expr)))
          (list (car expr))
          (append (free-vars (car expr)) (free-vars (cadr expr))))])))

(define free-helper
  (lambda (a b)
    (cond
      [(null? a) '()]
      [(equal? b (car a)) (free-helper (car a) b)]
      [else (append (list (car a)) (free-helper (cdr a) b))])))

(define bound-vars
(lambda (expr)
    (cond
      [(null? expr) '()]
      [(symbol? expr) '()]
      [(equal? (car expr) 'lambda)
        (cond
          [(symbol? (caddr expr))
            (if (not (member (caddr expr) (cadr expr)))
              '()
              (list (caddr expr))
              )
          ]
          [(equal? (caaddr expr) 'lambda)
            (if (member (caddr (caddr expr)) (cadr expr))
              (list (caddr (caddr expr)))
              (bound-vars (caddr expr))
            )
          ]
          [(equal? (caadr (caddr expr)) 'lambda)
            (bound-vars (cadr (caddr expr)))
          ]
          [else 
            (bound-helper (caddr expr) (cadr expr))])]
      [else
        (if (symbol? (car expr))
            (bound-vars (cadr expr))
            (append (bound-vars (car expr)) (bound-vars (cadr expr)))
        )])))

(define bound-helper
  (lambda (a b)
    (cond
      [(null? a) '()]
      [(equal? b (car a)) (append (list (car a)) (bound-helper (cdr a) b))]
      [else (bound-helper (car a) b)])))

;#2
(define occurs-free?
  (lambda (var expr)
    (cond
      [(null? expr) #f]
      [(symbol? expr) (eqv? var expr)]
      [(equal? (car expr) 'lambda) 
       (and (not (member var (cadr expr))) (occurs-free? var (caddr expr)))
      ]
      
      [(equal? (car expr) 'set!)
        (equal? (caddr expr) var)
      ]
      [(equal? (car expr) 'let)
        (occurs-free? var (let->application expr))
      ]
      [(equal? (car expr) 'let*)
        (occurs-free? var (let*->let expr))
      ]
      [(equal? (car expr) 'if) (if (member var (cadr expr)) #t (occurs-free? var (caddr expr)))] 
      [else (or (occurs-free? var  (car expr))
                (occurs-free? var (cdr expr)))
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
  (lambda (var expr)
    (cond
      [(null? expr) #f]
      [(symbol? expr) #f]
      [(eqv? (car expr) 'lambda)
       (or (occurs-bound? var (caddr expr))
           (and (not (not (member var (cadr expr))))
                (occurs-free? var (caddr expr))))
      ]
      [(equal? (car expr) 'let) (occurs-bound? var (let->application expr))]
      [(equal? (car expr) 'let*) (occurs-bound? var (let*->let expr))]
      [(equal? (car expr) 'if)
        (or (occurs-bound? var (caddr expr)) (occurs-bound? var (cadddr expr)))
      ]
      [else (or (occurs-bound? var  (car expr))
                (occurs-bound? var (cdr expr)))
      ])))
;#3
;took a lot of help and time to figure this out. Spent a large amount bug fixing, probably need to find a better way to do this in case it is tested later
;had to fully redo lexical and unlexical after thinking about a better mindset
(define lexical-address
  (lambda (expr)
    (lexical-address-helper expr)))

(define lexical-address-helper
  (lambda (expr)
    (let recur ([expr expr] [lex-env (list)])
      (cond 
        [(null? expr) '()]
        [(symbol? expr) (get-lex-at-env expr lex-env)]
        [(equal? (car expr) 'lambda) (list 'lambda (cadr expr) (recur (caddr expr) (cons (cadr expr) lex-env)))]
        [(equal? (car expr) 'let) (application->let (recur (let->application expr) lex-env))] 
        [(equal? (car expr) 'set!) (list 'set! (cadr expr) (recur (caddr expr) lex-env))]
        [(equal? (car expr) 'if) (list 'if (recur (cadr expr) lex-env) (recur (caddr expr) lex-env) (recur (cadddr expr) lex-env))]
        [else (map (lambda (e) (recur e lex-env)) expr)]))))

(define get-lex-at-env 
  (lambda (symbol env)
    (let recur ([symbol symbol] [env env] [depth 0])
      (cond
        [(null? env) (list ': 'free symbol)]
        [(member symbol (car env))
          (list ': depth (find-symbol symbol (car env)))
        ]
        [else (recur symbol (cdr env) (+ 1 depth))]))))

(define find-symbol
  (lambda (symbol lst)
    (let recur-find ([symbol symbol] [lst lst][pos 0])
      (cond
        [(null? lst) '()]
        [(equal? (car lst) symbol) pos]
        [else (recur-find symbol (cdr lst) (+ pos 1))]))))

(define application->let
  (lambda (lst)
    (if (null? lst)
      (lst)
      (append (append (list 'let) (list (let-params (cadar lst) (cdr lst) (list)) (caddr (car lst))))))))

(define let-params
  (lambda (x y acc)
    (cond
      [(null? x) acc]
      [else (let-params (cdr x) (cdr y) (append acc (list (list (car x) (car y)))))])))

(define find-depth-in-list
  (lambda (symbol bounds max depth)
    (cond 
      [(null? bounds) max]
      [(and (null? max) (equal? (caar bounds) symbol) (> depth (cadar bounds))) 
        (find-depth-in-list symbol (cdr bounds) (car bounds) depth)
      ]
      [(and (equal? (caar bounds) symbol) (> depth (cadar bounds)) (< (- depth (cadar bounds)) (- depth (cadr max))))
        (find-depth-in-list symbol (cdr bounds) (car bounds) depth)
      ]
      [else (find-depth-in-list symbol (cdr bounds) max depth)])))

(define get-var 
  (lambda (varlist x y)
    (cond
      [(null? varlist) '()]
      [(equal? x 0) (list-ref (car varlist) y)]
      [else (get-var (cdr varlist) (- x 1) y)])))

(define get-sym-at-lex
  (lambda (address lex-env)
    (let sym-helper ([depth (cadr address)] [position (caddr address)] [lex-env lex-env])
      (list-ref (list-ref lex-env depth) position))))

(define un-lexical-address
  (lambda (expr)
    (un-lexical-helper expr)))

(define un-lexical-helper
  (lambda (expr)
    (let recur ([expr expr] [lex-env (list)])
      (cond 
        [(null? expr) '()]
        [(and (equal? (car expr) ':) (equal? (cadr expr) 'free)) (caddr expr)]
        ;colon number number?
        [(equal? (car expr) ':) (get-sym-at-lex expr lex-env)]
        [(equal? (car expr) 'lambda) (list 'lambda (cadr expr) (recur (caddr expr) (cons (cadr expr) lex-env)))]
        [(equal? (car expr) 'let) (application->let (recur (let->application expr) lex-env))] 
        [(equal? (car expr) 'set!) (list 'set! (cadr expr) (recur (caddr expr) lex-env))]
        [(equal? (car expr) 'if) (list 'if (recur (cadr expr) lex-env) (recur (caddr expr) lex-env) (recur (cadddr expr) lex-env))]
        [else (map (lambda (e) (recur e lex-env)) expr)]))))