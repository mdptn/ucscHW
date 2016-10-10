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

(define (help-dim x))

(define (help-let x))

(define (help-goto x))

(define (help-if x))

(define (help-print x))

(define (help-input x))





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
		(goto, help_goto)
		(if, help_if)
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
;; for each line, if the line doesn't just contain the line number,
;; AND if the next element is a symbol, then that element is a label
;; and it is put into the label table as the key, while the rest of the
;; program starting from the label is stored as the value.
(define (put-in-label-table program)
	(for-each (lambda (line)
		(when (and (not (null? (cdr line))) (symbol? (cadr line)))
			(hash-set! *label-table* (cadr line) (member line program)))
		) program))


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


;;MAIN--------------------------------------------------------------

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))

            ;; puts the program labels into the label hash table
            (put-in-label-table program)

            ;; testing: printing out the label table
            (hash-for-each *label-table*
                (lambda (key value) (show key value)))

            ;; execute the program statements
            ;;(execute-program program)

              ;;(write-program-by-line sbprogfile program)
              )))

(main (vector->list (current-command-line-arguments)))


