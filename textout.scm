(define (lisp->infix expr)
  (if (pair? expr)
      (let ((func (car expr)) (args (cdr expr)))
	(cond ((eq? func '+)
	       (wrap-with-parens (join args " + ")))
	      ((eq? func '-)
	       (if (= (length args) 1)
		   (wrap-with-parens (string "-" (car args)))
		   (wrap-with-parens (join args " - "))))
	      ((eq? func '*)
	       (wrap-with-parens (join args " * ")))
	      ((eq? func '/)
	       (wrap-with-parens (join args " / ")))
	      ((eq? func 'expt)
	       (string "(" (car args) ")^(" (cadr args) ")"))
	      ((eq? func 'modulo)
	       (string "(" (car args) ") % (" (cadr args) ")"))
	      (else
	       (string func "(" (join args ", ") ")"))))
      expr))

(define (wrap-with-parens expr)
  (string "(" expr ")"))

(define (join lst token)
  (define (loop lst)
    (if (null? lst)
	""
	(string token (car lst) (loop (cdr lst)))))
  (string (car lst) (loop (cdr lst))))

':ok
