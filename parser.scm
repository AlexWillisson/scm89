;;; parser.scm
;;; Author: Vineel Adusumilli

;;; In parser.scm, we implement a parser combinator system meant to take in infix mathematical syntax.

;;; A parser is a function that takes a string representing uncomsumed input, and returns a list containing the symbolic interpretation of the consumed input and any unconsumed input. If a parser is to fail, it returns the empty list.

; p:fail is the simplest parser. It fails on every input.
(define (p:fail)
  (lambda (input)
    '()))

; p:symbol will match any string.
(define (p:symbol symbol)
  (lambda (input)
    ;; The string trimming means the parser is completely obliviosu to any
    ;; whitespace
    (let ((input (string-trim-left input)))
      (if (string-prefix? symbol input)
	  (list symbol
		(string-tail input (string-length symbol)))
	  '()))))

; p:return is handy for injecting results into the parsing process.
; It always passes and consumes no input.
(define (p:return value)
  (lambda (input)
    (list value input)))

; p:sequence applies a list of parsers in sequence, and fails if any single one
; of them does. It returns a list of the results of the individual parsers.
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

; p:choice keeps trying parsers until one of them succeeds, and then returns
; the result from that. p:choice fails if all of the parsers passed to it fail.
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

; p:apply will apply the given function to the result value from the upstream
; parser. It consumes no input.
(define (p:apply function parser)
  (lambda (input)
    (let ((result (parser input)))
      (if (pair? result)
	  (list (function (car result)) (cadr result))
	  '()))))

; p:tokenize is a handy way to replace a string with a symbol or other object.
(define (p:tokenize symbol token)
  (p:apply car (p:sequence (p:return token) (p:symbol symbol))))

; none matches the empty string. Used as a way to implement optional text.
(define none
  (p:tokenize "" '()))

(define (p:opt parser)
  (p:choice parser none))

; p:any is a shortcut for accepting one of a series of symbols
(define (p:any . symbols)
  (apply p:choice (map p:symbol symbols)))

(define (stitch strings)
  (apply string-append
	 (map
	  (lambda (s)
	    (if (null? s) "" s))
	  strings)))

; p:stitch is a handy way to concatenate symbols in a sequence.e
(define (p:stitch . parsers)
  (p:apply stitch (apply p:sequence parsers)))

; p:many will keep running the parser until it fails. It will automataically
; fail if the parser is not run the minimum number of times.
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

; insert-left! is used for tree manipulation magic needed in order to implement
; left-associativity. (Combinator parsers don't support left-recursion, making
; left-associativity tricky.)
(define (insert-left! tree value)
  (if (pair? tree)
      (if (null? (cdr tree))
	  (begin
	    (set-car! tree (list (list)))
	    (set-cdr! tree value))
	  (insert-left! (car tree) value))))

; flip-tree changes a tree from "right-leaning" to "left-leaning", which looks
; like a lot of improper dotted pairs.
(define (flip-tree tree)
  (let ((flipped (list (list))))
    (let lp ((tree tree))
      (if (pair? tree)
	  (begin
	    (lp (cdr tree))
	    (insert-left! flipped (car tree)))))
    flipped))

; flatten tree will flatten a tree into a single list
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

; associate applies left-association to a flipped tree.
(define (associate tree)
  (if (null? (car tree))
      '()
      (if (null? (cdar tree))
	  (cdr tree)
	  (let ((associated (associate (caar tree))))
	    (list (cdar tree) associated (cdr tree))))))

(define (left-associate tree)
  (associate (flip-tree tree)))

; right-associate right-associates a tree. This is much simpler to do with
; combinator parsers.
(define (right-associate tree)
  (if (null? (cdr tree))
      (car tree)
      (list (cadr tree) (car tree) (right-associate (cddr tree)))))

; p:operator is a handy shortcut for creating an operator
(define (p:operator op sub-exp associativity)
  (p:apply associativity
	   (p:apply crunch-back
		    (p:sequence sub-exp
				(p:many (p:sequence op sub-exp) 0)))))

; The following are tokens used in the grammar.

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

; float will accept numbers like: 1 1. 1.0 .0
(define float
  (p:choice
   (p:stitch (p:opt integer) dot integer)
   (p:stitch integer dot)
   integer))

; number adds support for scientific notation to float and casts the resulting
; string into an actual number.
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

; identifier recognizes text of the form [a-zA-Z][a-zA-Z1-9]*
(define identifier
  (p:apply
   string->symbol
   (p:stitch alpha (p:apply stitch (p:many alphanumeric 0)))))

; parameter-list accepts a comma separated list used for function invocations.
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

; parens matches values in parenthesis, numbers, identifiers, or function
; calls. It is meant to represent atomic values in the grammar.
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

; unary allows a - sign before an atom, indicating negation.
(define unary
  (p:apply
   (lambda (results)
     (if (null? (car results))
	 (cadr results)
	 results))
   (p:sequence (p:opt minus) parens)))

; expt-expr handles the ^ operator.
(define expt-expr
  (p:operator exponent unary right-associate))

; mul-expr handles * and /
(define mul-expr
  (p:operator (p:choice times divide) expt-expr left-associate))

; add-expr handles + and -
(define add-expr
  (p:operator (p:choice plus minus) mul-expr left-associate))

; declaration allows syntax like x = y + 3
; The value of x is set once when the statement is executed.
(define declaration
  (p:apply
   (lambda (results)
     (list 'set! (car results) (caddr results)))
   (p:sequence identifier set add-expr)))

; relation allows syntax like x <- y + 3
; The value of x is recalculated every time it is used, and is thus dependent
; on the current value of y.
(define relation
  (p:apply
   (lambda (results)
     (list 'bind! (car results) (caddr results)))
   (p:sequence identifier bind add-expr)))

; function allows function definition in the form of f(x)=x+3
(define function
  (p:apply
   (lambda (results)
     (list 'set! (car results) (list 'lambda (cadr results) (cadddr results))))
   (p:sequence identifier parameter-list set add-expr)))

; statement ties the grammar together
(define statement
  (p:choice function declaration relation add-expr))

; repl is a simple function used to help test if the parser is parsing the
; grammar correctly.
(define (repl)
  (let lp ()
    (display "> ")
    ;(write-line (eval (car (add-expr (read-line))) user-initial-environment))
    (let ((result (statement (read-line))))
      (if (> (string-length (cadr result)) 0)
	  (begin
	    (display "Unconsumed input: ")
	    (write-line (cadr result))))
      (pp (lisp->infix (eval-and-simplify (car result)))))
    (lp)))


':ok
