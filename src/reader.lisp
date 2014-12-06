(in-package :clint)

(defclass cl-readtable ()
  ((chars :initarg :chars :accessor readtable-chars :initform (make-hash-table))))

(setf (global-var ^'*readtable*) (make-instance 'cl-readtable))

(defun cl-set-macro-character (char fn &optional non-terminating (readtable (global-var ^'*readtable*)))
  "Has the reader call FN whenever CHAR is read."
  (declare (ignore non-terminating))
  (setf (gethash char (readtable-chars readtable)) fn)
  t)

(defun cl-get-macro-character (char &optional (readtable (global-var ^'*readtable*)))
  "Returns the macro-character for the given char."
  (gethash char (readtable-chars readtable)))

(defun whitespace (char)
  "Is this character whitespace?"
  (member char '(#\tab #\space #\newline)))

(defun adjustable-string ()
  "Returns an adjustable string."
  (make-array 10 :element-type 'character :adjustable t :fill-pointer 0))

(defun cl-read (&optional (stream *standard-input*) eof-error eof-val recur-p)
  "Reads an expression from STREAM."
  (declare (ignore recur-p))
  (loop with buffer = (adjustable-string)
        for char = (peek-char nil stream nil nil) do
        (cond ((not char)
               (if eof-error
                   (error "EOF")
                   (return eof-val)))
              ((whitespace char)
               (if (= (length buffer) 0)
                   (read-char stream)
                   (return (string->num/sym buffer))))
              ((cl-get-macro-character char)
               (if (= (length buffer) 0)
                   (progn (read-char stream)
                          (return (funcall (cl-get-macro-character char)
                                           stream char)))
                   (return (string->num/sym buffer))))
              (:else (vector-push-extend (read-char stream) buffer)))))

(defun string->num/sym (str)
  "Converts a string to either a number or a cl-symbol."
  (if (every #'digit-char-p str)
      (parse-integer str)
      (cl-intern str)))

(defun read-list (stream char)
  "Reads in a list."
  (declare (ignore char))
  (let* ((list (list nil)) (tail list))
    (catch 'end-of-list
      (loop (setf tail
                  (setf (cdr tail)
                        (list (cl-read stream))))))
    (cdr list)))

(defun end-list (stream char)
  "Signals the end of a list."
  (declare (ignore stream char))
  (throw 'end-of-list nil))

(defun quote-reader (stream char)
  "Reads in a quote."
  (declare (ignore char))
  ^`(quote ,(cl-read stream)))

(defun string-reader (stream char)
  "Reads in a string."
  (declare (ignore char))
  (loop with result = (adjustable-string)
        for char = (read-char stream)
        until (char= char #\")
        do (vector-push-extend char result)
        finally (return result)))

(defun comment-reader (stream char)
  "Ignores the rest of the line of input and returns the next thing 
   after that."
  (declare (ignore char))
  (read-line stream)
  (cl-read stream))

(cl-set-macro-character #\( 'read-list)
(cl-set-macro-character #\) 'end-list)
(cl-set-macro-character #\' 'quote-reader)
(cl-set-macro-character #\" 'string-reader)
(cl-set-macro-character #\; 'comment-reader)

(defun eval-string (str)
  "Evaluate an expression from the given string."
  (with-input-from-string (stream str)
    (cl-eval (cl-read stream) *env* *fenv*)))

(defun cl-repl ()
  "A REPL for the interpreter."
  (loop (format t "~&=> ")
        (format t "~&~A" (cl-eval (cl-read) *env* *fenv*))))
