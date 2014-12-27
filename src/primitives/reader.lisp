(in-package :clint)

(defclass cl-readtable ()
  ((chars    :initarg :chars :accessor readtable-chars    :initform (make-hash-table)
             :documentation "A table mapping from the single character reader macros.
                             to their corresponding procedure.")
   (dispatch :initarg :chars :accessor readtable-dispatch :initform (make-hash-table)
             :documentation "A table mapping from the first character of a dispatch
                             reader macro to the corresponding procedure."))
  (:documentation "A Clint readtable."))

(cl-defparameter *readtable* cl-*readtable* (make-instance 'cl-readtable)
  "The current readtable.")

(defun cl-set-macro-character (char fn &optional non-terminating (readtable cl-*readtable*))
  "This makes the reader call FN whenever CHAR is read."
  (declare (ignore non-terminating))
  (setf (gethash char (readtable-chars readtable)) fn)
  t)

(defun-cl make-dispatch-macro-character cl-make-dispatch-macro-character
    (char &optional non-terminating (readtable cl-*readtable*))
  "Create a dispatch-macro-character whose first character is CHAR."
  (declare (ignore non-terminating))
  (setf (gethash char (readtable-dispatch readtable)) (make-hash-table))
  (cl-set-macro-character char
    (lambda (stream c)
      (declare (ignore c))
      (let* ((next (read-char stream))
             (fn (gethash next (gethash char (readtable-dispatch readtable))
                          (lambda (stream c) (declare (ignore stream c))
                            (error "Undefined dispatch-macro-character ~A~A" char next)))))
        (funcall fn stream next)))
    readtable))

(defun cl-set-dispatch-macro-character (char subchar fn &optional (readtable cl-*readtable*))
  "Whenever the reader encounters CHAR followed by SUBCHAR, call FN."
  (setf (gethash subchar (gethash char (readtable-dispatch readtable))) fn)
  t)

(defun cl-get-macro-character (char &optional (readtable cl-*readtable*))
  "Returns the reader macro that corresponds to CHAR in READTABLE."
  (gethash char (readtable-chars readtable)))

(defun whitespace (char)
  "Is this character whitespace?"
  (member char '(#\tab #\space #\newline)))

(defun adjustable-string ()
  "Returns an adjustable string."
  (make-array 10 :element-type 'character :adjustable t :fill-pointer 0))

(defun-cl read cl-read (&optional (stream *standard-input*) (eof-error t) eof-val recur-p)
  "Reads an expression from STREAM."
  (declare (ignore recur-p))
  (loop with buffer = (adjustable-string)
        for char = (peek-char nil stream nil nil) do
    (cond ((not char)
           (if eof-error
               (error "Reached the end of the file.")
               (return eof-val)))
          ((whitespace char)
           (cond ((= (length buffer) 0) (read-char stream))
                 ((equalp buffer ".") (read-char stream) (throw 'atom (cl-read stream)))
                 (:else (return (string->num/sym buffer)))))
          ((cl-get-macro-character char)
           (if (= (length buffer) 0)
               (progn (read-char stream)
                      (return (funcall (cl-get-macro-character char)
                                       stream char)))
               (return (string->num/sym buffer))))
          (:else (vector-push-extend (read-char stream) buffer)))))

;; I'm going to have to rewrite this to accept separate package and
;; symbol parts. An issue comes up when using the | reader macro
;; where this still splits the symbol even though the colon should
;; be escaped.
(defun string->num/sym (str)
  "Converts a string to either a number or a Clint symbol."
  (if (every #'digit-char-p str)
      (parse-integer str)
      (let ((colon (position #\: str)))
	(if (not colon)
	    (cl-intern (string-upcase str))
	    (let* ((package (subseq str 0 colon))
		   (last-colon (position #\: str :from-end t))
		   (symbol (subseq str (+ last-colon 1))))
	      (cl-intern (string-upcase symbol)
			 (cl-find-package (string-upcase package))))))))

(defun read-list (stream char)
  "Reads in a list."
  (declare (ignore char))
  (let* ((list (list nil)) (tail list))
    (catch 'end-of-list
      (loop (setf tail
                  (setf (cdr tail)
                        (catch 'atom (list (cl-read stream)))))))
    (cdr list)))

(defun end-list (stream char)
  "This will signal that the end of a list has been reached to 
   read-list."
  (declare (ignore stream char))
  (throw 'end-of-list nil))

(defun quote-reader (stream char)
  "Reads in a quoted expression."
  (declare (ignore char))
  ^`(quote ,(cl-read stream)))

(defun sharp-quote-reader (stream char)
  "Reads in a sharp quoted expression."
  (declare (ignore char))
  ^`#',(cl-read stream))

(defun string-reader (stream char)
  "Reads in a string."
  (declare (ignore char))
  (loop with result = (adjustable-string)
        for char = (read-char stream)
        until (char= char #\")
        do (vector-push-extend char result)
        finally (return result)))

(defun comment-reader (stream char)
  "Ignores the rest of the line of input and returns the next form
   after that."
  (declare (ignore char))
  (read-line stream)
  (cl-read stream))

(defun array-reader (stream char)
  "Reads in an array. Currently only supports one-dimensional arrays."
  (declare (ignore char))
  (apply #'vector (read-list stream #\()))

(defun char-reader (stream char)
  "Reads in a char."
  (declare (ignore char))
  (read-char stream))

(defun symbol-reader (stream char)
  "Reads in an escaped symbol."
  (declare (ignore char))
  (loop with result = (adjustable-string)
        for char = (read-char stream)
        until (char= char #\|)
        do (vector-push-extend char result)
        finally (return (string->num/sym result))))

(cl-set-macro-character #\( 'read-list)
(cl-set-macro-character #\) 'end-list)
(cl-set-macro-character #\' 'quote-reader)
(cl-set-macro-character #\" 'string-reader)
(cl-set-macro-character #\; 'comment-reader)
(cl-set-macro-character #\| 'symbol-reader)

(cl-make-dispatch-macro-character #\#)
(cl-set-dispatch-macro-character #\# #\' 'sharp-quote-reader)
(cl-set-dispatch-macro-character #\# #\( 'array-reader)
(cl-set-dispatch-macro-character #\# #\\ 'char-reader)

(defun eval-string (str)
  "Read an expression from STR and evaluate it."
  (with-input-from-string (stream str)
    (cl-eval (cl-read stream) *env* *fenv*)))

(defun-cl load cl-load (file)
  "Evaluates all of the expressions in a file."
  (with-open-file (in file :direction :input)
    (let ((g (gensym)))
      (loop for exp = (cl-read in nil g)
            until (eq g exp)
            do (cl-eval exp *env* *fenv*)))))

(defun repl ()
  "A REPL for Clint."
  (catch 'exit ;; For exit to throw to exit the Clint repl.
    (loop
      (format t "~&=> ")
      (with-simple-restart (cl-repl "Return to Clint's repl.")
        (format t "~&~A" (cl-eval (cl-read) *env* *fenv*))))))
