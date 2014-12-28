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

(defun empty (str)
  "Is this string the empty string?"
  (= (length str) 0))
(proclaim '(inline empty))

(defun-cl read cl-read (&optional (stream *standard-input*) (eof-error t) eof-val recur-p)
  "Reads an expression from STREAM."
  (declare (ignore recur-p))
  (loop with str1 = (adjustable-string)
        with str2 = (adjustable-string)
        with read-colon = nil
        with escaped    = nil
        for char = (peek-char nil stream nil nil) do
    (cond ((not char)
           (if eof-error
               (error "Reached the end of the file.")
               (return eof-val)))
	  ((eql char #\|)
	   (read-char stream)
	   (setf escaped (not escaped)))
	  (escaped
	   (if read-colon
	       (vector-push-extend (read-char stream) str2)
	       (vector-push-extend (read-char stream) str1)))
	  ((eql char #\:)
	   (read-char stream)
	   (setf read-colon t))
          ((whitespace char)
           (cond ((and (empty str1) (empty str2))
 		  (read-char stream))
                 ((equal str1 ".")
		  (read-char stream)
		  (throw 'atom (cl-read stream)))
                 (read-colon
		  (return (strings->num/sym str2 str1)))
		 (:else
		  (return (strings->num/sym str1)))))
          ((cl-get-macro-character char)
           (cond ((and (empty str1) (empty str2))
		  (read-char stream)
		  (return (funcall (cl-get-macro-character char)
				   stream char)))
		 (read-colon
		  (return (strings->num/sym str2 str1)))
		 (:else
		  (return (strings->num/sym str1)))))
          (read-colon
	   (vector-push-extend (read-char stream) str2))
	  (:else
	   (vector-push-extend (read-char stream) str1)))))

(defun strings->num/sym (name &optional (package cl-*package*))
  "Converts a string to either a number or a Clint symbol."
  (if (every #'digit-char-p name)
      (parse-integer name)
      (cl-intern (string-upcase name) (if (stringp package)
					  (string-upcase package)
					  package))))

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

(cl-set-macro-character #\( 'read-list)
(cl-set-macro-character #\) 'end-list)
(cl-set-macro-character #\' 'quote-reader)
(cl-set-macro-character #\" 'string-reader)
(cl-set-macro-character #\; 'comment-reader)

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
