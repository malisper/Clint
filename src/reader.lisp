(in-package :clint)

(defclass cl-readtable ()
  ((chars :initarg :chars :accessor readtable-chars :initform (make-hash-table))))

(defparameter *buffer* (make-array 10
                                   :element-type 'character
                                   :adjustable t
                                   :fill-pointer 0)
  "A buffer which contains what has been read so far for an atom.")

(pushnew (list ^*readtable* (make-instance 'cl-readtable)) *env*
         :test (lambda (x y) (eq (car x) (car y))))

(defparameter *default-character-handler*
  (lambda (stream char)
    (declare (ignore stream))
    (vector-push-extend char *buffer*))
  "The default handler to call when reading a character.")

(defun cl-set-character-handler (char fn &optional non-terminating (readtable (get-val ^*readtable* *env*)))
  "Has the reader call FN whenever CHAR is read. The result is
   ignored. It is possible to have the result of the read be a value 
   by throwing the symbol 'read-result'. This is meant for characters
   such as whitespace which should only do something if they are
   preceded by an object."
  (declare (ignore non-terminating))
  (setf (gethash char (readtable-chars readtable))
        fn)
  t)

(defun cl-set-macro-character (char fn &optional non-terminating (readtable (get-val ^*readtable* *env*)))
  "Sets a macro-character to the given function in the given readtable in the interpreter."
  (cl-set-character-handler
    char
    (lambda (&rest args) (throw 'read-result (apply fn args)))
    non-terminating
    readtable))

(defun cl-get-macro-character (char &optional (readtable (get-val ^*readtable* *env*)))
  "Returns the macro-character for the given char."
  (gethash char (readtable-chars readtable)))

(defun cl-read (&optional (stream *standard-input*) eof-error eof-val recur-p)
  "Read in an expression."
  (declare (ignore recur-p))
  (setf (fill-pointer *buffer*) 0)
  (catch 'read-result
    (loop for char = (read-char stream nil nil)
          unless (and eof-error char)
            do (return-from cl-read eof-val)
          do (funcall (or (cl-get-macro-character char) *default-character-handler*)
                      stream char))))

(defun handle-whitespace (&rest args)
  "Handle a whitespace."
  (declare (ignore args))
  ;; We want to return only when the buffer has a value.
  (let ((buffer (process-buffer)))
    (when buffer
        (throw 'read-result buffer))))

(defun process-buffer ()
  "Take whatever is in the buffer and return it."
  (when (> (length *buffer*) 0)
    (prog1 (if (find-if #'digit-char-p *buffer*)
               (parse-integer *buffer*)
               (cl-intern (string-upcase (copy-seq *buffer*)) (get-val ^*package* *env*)))
      (setf (fill-pointer *buffer*) 0))))

(defun read-list (stream char)
  "Reads in a list."
  (declare (ignore char))
  (let ((result '()))
    (catch 'end-list
      (loop (push (cl-read stream) result)))
    (let ((buffer (process-buffer)))
      (when buffer
        (push buffer result)))
    (nreverse result)))

(defun end-list (stream char)
  "Handles the end of a list."
  (declare (ignore stream char))
  (throw 'end-list nil))

(defun quote-reader (stream char)
  "Reads in a quote."
  (declare (ignore char))
  `(,^quote ,(cl-read stream)))

(cl-set-macro-character #\( 'read-list)
(cl-set-macro-character #\' 'quote-reader)

(cl-set-character-handler #\) 'end-list)
(cl-set-character-handler #\space 'return-process-buffer)
(cl-set-character-handler #\newline 'return-process-buffer)

(defun cl-repl ()
  "A REPL for the interpreter."
  (loop (format t "~&=> ")
        (format t "~&~A" (cl-eval (cl-read) *env* *fenv*))))
