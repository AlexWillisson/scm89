;; our code ~atw

(define *the-empty-environment* (make-top-level-environment))

(define main-env (make-top-level-environment))

(define (bind! variable value)
  (set! main-env (extend-top-level-environment main-env
					       (list variable)
					       (list value))))

(define (sub-in-known expr)
  (if (pair? expr)
      (cond ((eq? (car expr) 'set!)
	     (if (evalable? (caddr expr))
		 (begin (bind! (cadr expr) (eval (caddr expr) main-env)) ':ok)
		 (string "error: cannot set variable to a value not"
			 " currently defined. Use <- instead of = to"
			 " set up relationships")))
	    ((eq? (car expr) 'bind!)
	     (bind! (cadr expr) (caddr expr)) ':ok)
	    (else 
	     (if (evalable? expr)
		 (eval expr main-env)
		 (cons (car expr)
		       (map sub-in-known (cdr expr))))))
      (if (and (symbol? expr) (environment-bound? main-env expr))
	  (eval-and-simplify (eval expr main-env))
	  expr)))

(define (eval-and-simplify expr)
  (algebra-simplifier (sub-in-known expr)))

(define (evalable? expr)
  (if (pair? expr)
      (or (eq? (car expr) 'lambda)
	  (and (evalable? (car expr)) (evalable? (cdr expr))))
      (if (symbol? expr)
	  (if (environment-bound? main-env expr)
	      #t
	      #f)
	  #t)))


;; borrowing heavily (well, stolen) from ps3
(define algebra-simplifier
  (rule-simplifier
   (list

    ;; Sums

    (rule `(+ (? a)) a)

    (rule `(+ (?? a) (+ (?? b)) (?? c))
	  `(+ ,@a ,@b ,@c))

    (rule `(+ (?? a) (? y) (? x) (?? b))
	  (and (expr<? x y)
	       `(+ ,@a ,x ,y ,@b)))
    

    ;; Products

    (rule `(* (? a)) a)

    (rule `(* (?? a) (* (?? b)) (?? c))
	  `(* ,@a ,@b ,@c))

    (rule `(* (?? a) (? y) (? x) (?? b))
	  (and (expr<? x y)
	       `(* ,@a ,x ,y ,@b)))


    ;; Distributive law

    (rule `(* (?? a) (+ (?? b)) (?? c))
	  `(+ ,@(map (lambda (x) `(* ,@a ,x ,@c)) b)))

    ;; Numerical simplifications below

    (rule `(+ 0 (?? x)) `(+ ,@x))

    (rule `(+ (? x ,number?) (? y ,number?) (?? z))
	  `(+ ,(+ x y) ,@z))


    (rule `(* 0 (?? x)) 0)
     
    (rule `(* 1 (?? x)) `(* ,@x))

    (rule `(* (? x ,number?) (? y ,number?) (?? z))
	  `(* ,(* x y) ,@z))

    ;; Collecting terms

    (rule `(+ (?? y) (+ (? x) (? x)) (?? z))
	  `(+ ,@y (* 2 ,x) ,@z))

    (rule `(+ (?? y) (? x) (? x) (?? z)) `(+ ,@y (* 2 ,x) ,@z))

    (rule `(+ (?? y) (* (? n) (? a) (? b)) (* (? m) (? a) (? b)) (?? z))
	  `(+ ,@y (* (+ ,n ,m) ,a ,b) ,@z))

    (rule `(+ (?? y) (? a) (* (? n) (? a)) (?? z))
	  `(+ ,@y (* (+ ,n 1) ,a) ,@z))
    )))

':ok
