Clint
=====

Common Lisp INTerpreter

This is a Common Lisp interpreter that I am piecing together.

Definitions: Clint refers to the interpreter level Common Lisp while
ICL refers to the implementation Common Lisp.

Conventions: Things whose name begins with cl: is defined at the ICL
level, is equivalent to the ICL's version without the cl prepended,
but instead acts at the Clint level. For example, cl-intern is an ICL
procedure, but will intern a symbol into Clint instead of the ICL.
There are a few exceptions. The procedure cl-boundp acts a little
different than the ICL procedure boundp. The procedures cl-eval-all
and cl-eval-if do not have ICL equivalents. The procedure cl-doc is
equivalent to the ICL procedure, documentation, I just truncated the
name.

Core: The core of the interpreter is in the src/core/ directory. This
includes the implementation of eval, apply, intern, special forms, and
several other primitives at the foundation of the interpreter.

Primitives: All of the primitives that are not at the heart of Clint
are in the src/primitives/ directory. It also contains all of the
basic variable definitions.

Non-primitives: All of the non-primitives are in the src/libs/ folder.
Currently there are two files. One that contains several higher order
functions and another that contains several utilities for working with
lists.

Evaluating Code: There are currently two wasy to evaluate code with
Clint. There is the procedure "repl", which will start a repl. And
there is eval-string, which takes a string of the code to be evaluated
as an argument.

Warnings: Currently the Clint symbol "nil" and the empty list are not
the same thing. Always use the empty list for now.
