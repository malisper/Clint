Clint
=====

Common Lisp INTerpreter

This is a Common Lisp interpreter that I am piecing together.

Definitions: Clint refers to the interpreter level Common Lisp while
ICL refers to the implementation Common Lisp.

Conventions: Anything whose name begins with cl: is defined at the ICL
level, is equivalent to the ICL's version without the cl prepended,
but instead acts at the Clint level. For example, cl-intern is an ICL
procedure, but will intern a symbol into Clint instead of the ICL.

Primitives: Special forms are defined as part of cl-eval, the core of
the interpreter, in the interp.lisp file. All of the primitive
procedures are defined in the primitive-fns.lisp file and all of
primitive macros are defined in the primitive-macros.lisp file.

Non-primitives: Currently the only non-primitive Clint code is in the
higher-order-fns.lisp file. Unfortunately I have not yet been able to
figure out how to have it load with the rest of Clint (all of the
primitives will be loaded). To load it manually, use either cl-load
in the ICL or load from within Clint.

Evaluating Code: There are currently two wasy to evaluate code with
Clint. There is the procedure "repl", which will start a repl. And
there is eval-string, which takes a string of the code to be evaluated
as an argument.

Warnings: Currently the Clint symbol "nil" and the empty list are not
the same thing. Always use the empty list for now.
