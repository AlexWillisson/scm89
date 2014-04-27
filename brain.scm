;; should evaluate everything it can, and simplify afterwards.

(define env (make-top-level-environment))

(define (eval-and-simplify expr)
  (if (pair? expr)
      (if (evalable? expr)
	  (eval expr env)
	  (cons (car expr) (map eval-and-simplify (cdr expr))))
      expr))

(define (evalable? expr)
  (if (pair? expr)
      (every (lambda (x) x) (map evalable? expr))
      (or (not (symbol? expr)) (environment-bound? env expr))))
