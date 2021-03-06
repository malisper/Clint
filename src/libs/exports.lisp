;;;; This is where all of the symbols are exported.

(export '(funcall apply eval package-name find-package intern
          make-package package-nicknames documentation fdefinition
          macro-function set-macro-character
          set-dispatch-macro-character get make-hash-table
          symbol-name symbol-package export import use-package
          unintern + - * / exp expt sin cos tan eq eql cons rplacd
          rplaca evenp oddp zerop plusp minusp null not < > <= >= =
          gethash remhash atom))

(export '(let let* cond lambda setf push pop defun defmacro and or
	  when unless prog1 prog2 incf decf))

(export '(member-if member find-if find maplist mapcar mapc mapl list
	  list* nthcdr equal length append last butlast nconc
	  identity constantly))
