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


;;*function-table*--------------------------------------------------
;;  holds all of the functions, which include the operators. 
;;------------------------------------------------------------------


;;*label-table*--------------------------------------------------
;;  holds the addresses of each line, one level up from statements.
;;------------------------------------------------------------------


;;*variable-table*--------------------------------------------------
;;  holds the value of all variables.
;;------------------------------------------------------------------



(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

(main (vector->list (current-command-line-arguments)))


