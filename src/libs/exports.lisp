;;;; This is where all of the symbols are exported.

(export '(funcall apply eval package-name find-package intern
          make-package package-nicknames documentation fdefinition
          macro-function set-macro-character
          set-dispatch-macro-character get make-hash-table
          symbol-name symbol-package export import use-package
          unintern + - * / exp expt sin cos tan eq eql cons rplacd
          rplaca evenp oddp zerop plusp minusp null not < > <= >= =
          gethash remhash atom))
