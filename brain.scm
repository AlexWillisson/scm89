;; should evaluate everything it can, and simplify afterwards.

;; (define env (make-top-level-environment))

;; (define (eval-and-simplify expr)
;;   (if (pair? expr)
;;       (if (evalable? expr)
;; 	  (eval expr env)
;; 	  (cons (car expr) (map eval-and-simplify (cdr expr))))
;;       expr))

;; (define (evalable? expr)
;;   (if (pair? expr)
;;       (every (lambda (x) x) (map evalable? expr))
;;       (or (not (symbol? expr)) (environment-bound? env expr))))

(define *the-empty-environment* (make-top-level-environment))

(define main-env (make-top-level-environment))

(define (bind! variable value)
  (set! main-env (extend-top-level-environment main-env
					       (list variable)
					       (list value))))
(define (sub-in-known expr env)
  (if (pair? expr)
      (cons (car expr)
	    (map (lambda (expr) (sub-in-known expr env)) (cdr expr)))
      (if (and (symbol? expr) (environment-bound? env expr))
	  (eval expr env)
	  expr)))

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

(define (eval-and-simplify expr env)
  (algebra-simplifier (sub-in-known expr env)))

':ok