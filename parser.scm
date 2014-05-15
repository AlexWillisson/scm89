(define (p:fail)
  (lambda (input)
    '()))

(define (p:symbol symbol)
  (lambda (input)
    (let ((input (string-trim-left input)))
      (if (string-prefix? symbol input)
	  (list symbol
		(string-tail input (string-length symbol)))
	  '()))))

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

(define (p:apply function parser)
  (lambda (input)
    (let ((result (parser input)))
      (if (pair? result)
	  (list (function (car result)) (cadr result))
	  '()))))

(define (p:tokenize symbol token)
  (p:apply car (p:sequence (p:return token) (p:symbol symbol))))

(define none
  (p:tokenize "" '()))

(define (p:opt parser)
  (p:choice parser none))

(define (p:any . symbols)
  (apply p:choice (map p:symbol symbols)))

(define (stitch strings)
  (apply string-append
	 (map
	  (lambda (s)
	    (if (null? s) "" s))
	  strings)))

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
      (if (null? (cdr tree))
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
	    (insert-left! flipped (car tree)))))
    flipped))

(define (flatten tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (flatten (car tree))
		      (flatten (cdr tree))))))

; If a list is made of lists, crunch will remove one layer of nesting.
(define (crunch list)
  (apply append list))

; Same as crunch, except first element is untouched.
; Used for tree manipulation.
(define (crunch-back list)
  (cons (car list) (crunch (cadr list))))

(define (associate tree)
  (if (null? (car tree))
      '()
      (if (null? (cdar tree))
	  (cdr tree)
	  (let ((associated (associate (caar tree))))
	    (list (cdar tree) associated (cdr tree))))))

(define (left-associate tree)
  (associate (flip-tree tree)))

(define (right-associate tree)
  (if (null? (cdr tree))
      (car tree)
      (list (cadr tree) (car tree) (right-associate (cddr tree)))))

(define (p:operator op sub-exp associativity)
  (p:apply associativity
	   (p:apply crunch-back
		    (p:sequence sub-exp
				(p:many (p:sequence op sub-exp) 0)))))

(define open
  (p:tokenize "(" '()))

(define close
  (p:tokenize ")" '()))

(define comma
  (p:tokenize "," '()))

(define dot
  (p:symbol "."))

(define plus
  (p:tokenize "+" '+))

(define minus
  (p:tokenize "-" '-))

(define times
  (p:tokenize "*" '*))

(define divide
  (p:tokenize "/" '/))

(define exponent
  (p:tokenize "^" 'expt))

(define set
  (p:tokenize "=" 'set!))

(define bind
  (p:tokenize "<-" 'bind!))

(define digit
  (p:any "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(define lower
  (p:any "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
	 "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"
	 "w" "x" "y" "z"))

(define upper
  (p:any "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K"
	 "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V"
	 "W" "X" "Y" "Z"))

(define alpha
  (p:choice lower upper))

(define alphanumeric
  (p:choice alpha digit))

(define integer
  (p:apply stitch (p:many digit 1)))

(define float
  (p:choice
   (p:stitch (p:opt integer) dot integer)
   (p:stitch integer dot)
   integer))

(define number
  (p:apply
   string->number
   (p:stitch
    float
    (p:opt
     (p:stitch
      (p:any "e" "E")
      (p:any "+" "-" "")
      integer)))))

(define identifier
  (p:apply
   string->symbol
   (p:stitch alpha (p:apply stitch (p:many alphanumeric 0)))))

(define parameter-list
  (p:apply
   (lambda (results)
     (cons (cadr results) (caddr results)))
   (p:sequence
    open
    (lambda (input) (add-expr input))
    (p:many
     (p:apply
      cadr
      (p:sequence
       comma
       (lambda (input) (add-expr input)))) 0)
    close)))

(define parens
  (p:choice 
   number
   (p:apply
    (lambda (results)
      (cons (car results) (cadr results)))
    (p:sequence identifier parameter-list))
   identifier
   (p:apply cadr
	    (p:sequence open
			(lambda (input) (add-expr input))
			close))))

(define unary
  (p:apply
   (lambda (results)
     (if (null? (car results))
	 (cadr results)
	 results))
   (p:sequence (p:opt minus) parens)))

(define expt-expr
  (p:operator exponent unary right-associate))

(define mul-expr
  (p:operator (p:choice times divide) expt-expr left-associate))

(define add-expr
  (p:operator (p:choice plus minus) mul-expr left-associate))

(define declaration
  (p:apply
   (lambda (results)
     (list 'set! (car results) (caddr results)))
   (p:sequence identifier set add-expr)))

(define relation
  (p:apply
   (lambda (results)
     (list 'bind! (car results) (caddr results)))
   (p:sequence identifier bind add-expr)))

(define statement
  (p:choice declaration relation add-expr))

(define (repl)
  (let lp ()
    (display "> ")
    ;(write-line (eval (car (add-expr (read-line))) user-initial-environment))
    (let ((result (statement (read-line))))
      (pp (car result))
      (if (> (string-length (cadr result)) 0)
	  (begin
	    (display "Unconsumed input: ")
	    (write-line (cadr result)))))
    (lp)))

(repl)

