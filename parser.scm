(define (p:fail)
  (lambda (input)
    '()))

(define (p:symbol symbol)
  (lambda (input)
    (if (string-prefix? symbol input)
	(list symbol
	      (string-tail input (string-length symbol)))
	'())))

(define (p:return value)
  (lambda (input)
    (list value input)))

(define (p:sequence . parsers)
  (lambda (input)
    (let ((results (make-vector (length parsers))))
      (let lp ((p (car parsers))
	       (rest (cdr parsers))
	       (input input)
	       (i 0))
	(let ((result (p input)))
	  (if (pair? result)
	      (begin
		(vector-set! results i (car result))
		(if (pair? rest)
		    (lp (car rest) (cdr rest) (cadr result) (+ i 1))
		    (list (vector->list results) (cadr result))))
	      '()))))))

(define (p:choice . parsers)
  (lambda (input)
    (let lp ((p (car parsers))
	     (rest (cdr parsers)))
      (let ((result (p input)))
	(if (pair? result)
	    result
	    (if (pair? rest)
		(lp (car rest) (cdr rest))
		'()))))))

(define (p:any . symbols)
  (apply p:choice (map p:symbol symbols)))

(define (p:apply function parser)
  (lambda (input)
    (let ((result (parser input)))
      (if (pair? result)
	  (list (function (car result)) (cadr result))
	  '()))))

(define (stitch strings)
  (apply string-append strings))

(define (p:stitch . parsers)
  (p:apply stitch (apply p:sequence parsers)))

(define (p:many parser min)
  (lambda (input)
    (let lp ((results '())
	     (input input)
	     (i 0))
      (let ((result (parser input)))
	(if (pair? result)
	    (lp (append results (list (car result))) (cadr result) (+ i 1))
	    (if (>= i min)
		(list results input)
		'()))))))

(define digit
  (p:any "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(define number
  (p:apply string->number (p:apply stitch (p:many digit 1))))

(number "123456 hello")


