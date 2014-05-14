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

(define (p:tokenize symbol token)
  (p:apply car (p:sequence (p:return token) (p:symbol symbol))))

(define none
  (p:tokenize "" '()))

(define (p:opt parser)
  (p:choice parser none))

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

(define (insert-left! tree value)
  (if (pair? tree)
      (if (list? (cdr tree))
	  (begin
	    (set-car! tree (list (list)))
	    (set-cdr! tree value))
	  (insert-left! (car tree) value))))

(define (flip-tree tree)
  (let ((flipped (list (list))))
    (let lp ((tree tree))
      (if (pair? tree)
	  (begin
	    (lp (cdr tree))
	    (pp (car tree))
	    (insert-left! flipped (car tree)))))
    flipped))

(define (flatten tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (flatten (car tree))
		      (flatten (cdr tree))))))

(define (associate tree)
  (if (null? (car tree))
      '()
      (if (null? (cdar tree))
	  (cdr tree)
	  (let ((associated (associate (caar tree))))
	    (list (cdar tree) associated (cdr tree))))))

(define (left-associate tree)
  (associate (flip-tree tree)))

(define plus
  (p:tokenize "+" '+))

(define minus
  (p:tokenize "-" '-))

(define digit
  (p:any "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(define number
  (p:apply string->number (p:apply stitch (p:many digit 1))))

(define (id input) input)

(define expr
  (p:apply flatten
	   (p:sequence number
		       (p:opt (p:sequence (p:choice plus minus)
					  (lambda (input) (expr input)))))))

(define parse
  (p:apply left-associate expr))

(parse "3+4-5")



