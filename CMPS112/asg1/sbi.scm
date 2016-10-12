#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;;------------------------------------------------------------------
;; sbi.scm
;; Silly Basic Interpreter
;; Assignment 1
;; CMPS 112 Fall 2016
;; Megan Nguyen
;; mednguye@ucsc.edu
;; Code Template Provided by Professor Mackey in the CMPS112 files
;;------------------------------------------------------------------


;; defines *stderr* as the output port for errors and logging
;; provided by Mackey
(define *stderr* (current-error-port))


;; defines *run-file* as sbi.scm
;; basically, it deconstructs the file path into a smaller path, sbi.scm.
;; provided by Mackey
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)


;; defines the function "die" with a list parameter.
;; displays each item in the list passed through to *stderr*
;; provided by Mackey
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)


;; calls the die function to run the error message, "Usage: sbi.scm filename"
;; provided by Mackey
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)


;; reads the input file, if unable to read then it throws an error with die
;; provided by Mackey
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))


;; the function write-program-by-line first displays the *run-file* and filename
;; then each line of the file is printed out
;; provided by Mackey
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))


;;HELPER FUNCTIONS--------------------------------------------------
;; Helper functions to implement dim, let, goto, if, print, input
;;------------------------------------------------------------------

;; creates an array given by the variable name and inserts it into the
;; variable table, replacing any previous variable, array, or function
;; already in the table. The dimension of the array is given by the expression.
(define (help_dim expr)
	(set! expr (car expr))
	(let ((arr (make-vector (evalexpr (cadr expr)) (car expr))))
		(variable-put! (car expr) (+ (evalexpr (cadr expr)) 1))))

;; makes an assignment to a variable.
;; the variable name and the evaluated expression is put into the variable table.
(define (help_let expr)
	(variable-put! (car expr) (evalexpr (cadr expr))))

;; Each of the operands is printed in sequence, with a space before
;; Expression values. A newline is output at the end of the print statement.
(define (help_print expr)
	(map (lambda (x) (display (evalexpr x))) expr)
	(newline))

;; Numeric values are read in and assigned to the input variables in sequence.
;; the value is inserted into the Symbol table under that variable’s key.
;; The variable inputcount is inserted into the symbol table at end of execution
;; of this statement and initialized to the number of values successfully read in.
;; A value of −1 is returned to indicate end of file.
(define (help_input expr)
	(variable-put! 'inputcount 0)
	(if (null? (car expr))
		(variable-put! 'inputcount -1) (begin (variable-put! 'inputcount (help_input_count expr 0)))))

;; this function helps manage the inputcount variable for the input function
(define (help_input_count expr count)
	(if (null? expr) count (let ((input (read))) (if (eof-object? input) -1
		(begin (variable-put! (car expr) input)
			(set! count (+ 1 count)) (help_input_count (cdr expr) count))))))



;;*function-table*--------------------------------------------------
;; holds all of the functions, which include the operators. 
;; the key is the function symbol, value is the implementation
;; most implementations are built into scheme,
;; excluding the 6 that need helper functions and % & <>
;;
;; dim, let, goto, if, print, input, + - * / % ^ = <> > < >= <=
;; abs, acos, asin, atan, ceil, cos, exp, floor,
;; log, log10, log2, round, sin, sqrt, tan, trunc
;;------------------------------------------------------------------

;; makes the hash table for functions
(define *function-table* (make-hash))

;; get the value of the function pertaining to the key
(define (function-get key)
	(hash-ref *function-table* key))

;; map the value of the function to the key
(define (function-put! key value)
	(hash-set! *function-table* key value))

;; maps all function values to the function symbols
(for-each
	(lambda (pair)
		(function-put! (car pair) (cadr pair)))
	`(
		(dim, help_dim)
		(let, help_let)
		(goto (void))
		(if (void))
		(print, help_print)
		(input, help_input)
		(+, +)
		(-, -)
		(*, *)
		(/, /)
		(%, (lambda (x y) (- x (* (trunc (/ x y)) y))))
		(^, expt)
		(=, =)
		(<>, (lambda (x y) (not(= x y))))
		(>, >)
		(<, <)
		(>=, >=)
		(<=, <=)
		(abs, abs)
		(acos, acos)
		(asin, asin)
		(atan, atan)
		(ceil, ceiling)
		(cos, cos)
		(exp, exp)
		(floor, floor)
		(log, log)
		(log10, (lambda (x) (/ (log x) (log 10.0))))
		(log2, (lambda (x) (/ (log x) (log 2.0))))
		(round, round)
		(sin, sin)
		(sqrt, sqrt)
		(tan, tan)
		(trunc, truncate)
		))




;;*label-table*-----------------------------------------------------
;; holds the addresses of each line, one level up from statements.
;; the key is the label, value is the statement
;;------------------------------------------------------------------

;; makes the hash table for labels
(define *label-table* (make-hash))

;; goes through the program to find labels and add them to hash
;; if the line has 3 items then it has a label.
;; OR if a line has two items and the 2nd is not a list, it is a label.
;; the label is put into the label table as the key, while address
;; starting from the label is stored as the value.
(define (put-in-label-table program)
	(map (lambda (line)
		(when (not (null? line))
			(when (or (= 3 (numberOfItems line)) 
			      (and (= 2 (numberOfItems line)) 
                              (not (list? (cadr line)))))
			(hash-set! *label-table* (cadr line) (- (car line) 1))))) program))


;; taken from listhash.scm to test if label table inserted correctly
(define (show label item)
        (newline)
        (display label) (display ":") (newline)
        (display item) (newline))
	


;;*variable-table*--------------------------------------------------
;; holds the value of all variables.
;; the key is variable name, value is real #s or vectors of real #s
;;------------------------------------------------------------------

;; makes the hash table for variables
(define *variable-table* (make-hash))

;; get the value of the variable pertaining to the key
(define (variable-get key)
	(hash-ref *variable-table* key))

;; map the value of the variable to the key
(define (variable-put! key value)
	(hash-set! *variable-table* key value))

;; inserts values for the variables pi and e as stated in asg
(for-each
	(lambda (pair)
		(variable-put! (car pair) (cadr pair)))
	`(
		(pi 3.141592653589793238462643383279502884197169399)
		(e 2.718281828459045235360287471352662497757247093)
		))


;;FUNCTIONS FOR EVALUATING THE STATEMENTS---------------------------

;; find the # of items in the list
(define numberOfItems
	(lambda (x)
		(if (null? x) 0
			(+ (numberOfItems (cdr x)) 1))))

;; starts running through the sbir file to find statements to evaluate
(define (run-through program lineNumber)
	;; do this while line number < the # of lines in the sbir file
	(when (> (numberOfItems program) lineNumber)
		;; list-ref returns the line in the program at that lineNumber
		(let ((line (list-ref program lineNumber)))
			(cond ((= (numberOfItems line) 3)
				(set! line (cddr line)) (run-through-line (car line) program lineNumber))
			((and (= (numberOfItems line) 2) (list? (cadr line)))
				(set! line (cdr line)) (run-through-line (car line) program lineNumber))
			(else (run-through program (+ lineNumber 1)))))))

;; checks the function table to see if there is an appropriate function in the line.
;; displays error message and exits if it isn't a valid function symbol
;; parses through to see if the function is goto, if, or print 
;; performs what is needed if it is those three, else checks the function table
;; for what to do for other functions
(define (run-through-line funct program lineNumber)
	(when (not (hash-has-key? *function-table* (car funct)))
		(die "~s not a valid function in the table" (car funct)))
	(cond ((eq? (car funct) 'goto)
		(run-through program (hash-ref *label-table* (cadr funct))))
	((eq? (car funct) 'if)
		(if (evalexpr (car (cdr funct)))
			(run-through program (hash-ref *label-table* (cadr (cdr funct))))
			(run-through program (+ lineNumber 1))))
	((eq? (car funct) 'print)
		(if (null? (cdr funct))
			(newline) (help_print (cdr funct)))
		(run-through program (+ lineNumber 1)))
	(else ((hash-ref *function-table* (car funct)) (cdr funct))
		(run-through program (+ lineNumber 1))))
	)

;; evaluates expressions
;; if the expr is a number, add 0.0 to make it a real number
;; checks in the variable table if the expr is in there
;; checks in function table to find function and goto evalexpr
(define (evalexpr expr)
	(cond ((number? expr) (+ expr 0.0))
		((string? expr) expr)
		((hash-has-key? *variable-table* expr)
			(hash-ref *variable-table* expr))
		((list? expr)
			(if (hash-has-key? *function-table* (car expr))
				(let ((head (hash-ref *function-table* (car expr))))
					(cond ((number? head) head)
						((vector? head) (vector-ref head (cadr expr)))
						((procedure? head)
							(apply head (map (lambda (x) (evalexpr x)) (cdr expr))))
						(else (die "Error with evaluating the expression"))))
				(die (list (car expr) " expression not found in the function table"))))))



;;MAIN--------------------------------------------------------------

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))

            ;; puts the program labels into the label hash table
            (put-in-label-table program)

            ;; testing: printing out the label table
            ;;(hash-for-each *label-table*
                ;;(lambda (key value) (show key value)))

            ;; execute the file statements starting with first item(line)
            (run-through program 0)

              ;;(write-program-by-line sbprogfile program)
              )))

(main (vector->list (current-command-line-arguments)))


