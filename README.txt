Clint
=====

Common Lisp INTerpreter

As the name suggests this is a Common Lisp interpreter that I am
piecing together.


Getting Started:

After loading Clint, the 'repl' procedure will start a repl in which
you can type expressions and have them be evaluated by Clint. There
are several files of code meant to be loaded by the interpreter. To do
this either use (cl-load ".../Clint/src/libs/FILE") from outside the
Clint repl or (load "...") from within. All of the files in the
src/libs/ directory need to be loaded this way. I am currently trying
to find a way to automate this.


Definitions:

The name 'Clint' refers to the interpreter level Common Lisp while
'ICL' refers to the implementation Common Lisp. This is useful for
distinguishing whether a procedure is at the interpreter level or the
implementation level.


Conventions:

A call to a procedure (from the ICL level) with 'cl-' prepended is
roughly the same as calling the procedure without the prefix from
within Clint. For example, calling 'cl-intern' from the ICL is
equivalent to calling 'intern' from the Clint repl.

There are currently three exceptions to this convention. The procedure
'cl-eval-all' for which there is no analog, the procedure 'cl-boundp'
which behaves differently than 'boundp' does, and 'cl-doc' which is
the analog of 'documentation'.


Core:

The core of the interpreter is in the src/core/ directory. The
procedures cl-eval, cl-apply, and cl-intern are all implemented there.


Primitives:

Primitives are all defined in the src/primitves/ directory.

Primitive procedures are defined in one of three ways. The simplest
way is the procedure add-prim. It takes a symbol, uses that as the
name of the primitive, and also calls the ICL function of that name to
implement the primitive. This makes calling that primitve from within
Clint the same as calling the ICL procedure of the same name. It is
useful for procedures taken directly from the ICL such as the
arithmetic operators.

The second option is to use the macro defprimitive-fn. It associates a
Clint primitive with an ICL closure through syntax that is like
regular defun. The closure will be invoked whenever the Clint
primitive is called.

The last option is the macro defun-cl. It is similar to
defprimitive-fn, but it defines both a Clint primitive and an ICL
procedure at the same time.

There is a macro analog of defprimitive-fn called
defprimitive-macro. It associates an ICL closure with a macro and will
call the macro on Clint code to macroexpand the Clint code.

See the primitives/definitions.lisp file for the implementation of the
above procedure and macros.

The macro cl-defparameter, makes it possible to define Clint variables
from the ICL. Not only does it act like a defparameter form evaluated
in Clint, it also defines a symbol-macro that allows one to access the
variable from the ICL.


Non-primitives:

All of the other code is implemented in the src/libs/ directory. It
contains Clint level code that acts as a sort of library. This are
things that can easily be implemented in terms of the primitives that
are already defined.
