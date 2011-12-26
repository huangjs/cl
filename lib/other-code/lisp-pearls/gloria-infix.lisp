#|
infix.lisp
by AmkG

Title		:	Infix notation macro
Filename	:	infix.lisp
Written by	:	Alan Manuel K. Gloria <almkglor@gmail.com>

Copyright (c) 2006 Alan Manuel K. Gloria

Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the
Software without restriction, including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so, subject to the
following conditions:

The above copyright notice and this permission notice shall be included in all copies
or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

tested on:
 clisp
 gcl
 sbcl
 ecl
|#

#|
infix notation:
Lisp has traditionally used prefix notation for all formulae: in this
notation, the operation to be performed is specified before the
operands it is to be performed on.  This simple notation gives a
consistent, regular syntax which greatly facilitates
metaprogramming.

However, one weakness of prefix notation is that some simple
algebraic/arithmetic operations do not and cannot follow the
traditional syntax taught to us during elementary - that is, the
notation we are used to in arithmetic is not prefix, but infix
notation:
   1 + 2 - 4 * 5
In Lisp this would be:
  (- (+ 1 2) (* 4 5)))
Despite the inherent advantages of prefix notation, sometimes
it's just much easier to write mathematical formulae a little more
like the way we actually write them on paper.

This file contains a macro, 'nfx, which allows for such an infix
notation.  There are limitations: you need to put a LOT of spaces
in the formula:
	(nfx 1+2-3) ;would confuse the Lisp parser
	(nfx 1 + 2 - 3) ;would be understood

You can't use certain symbols as variable names:
	(setf + 42)
	(nfx (max + 3)) ; would look awfully like +(max,3), not max(+,3)

...but you can embed prefix notation in it!
	(nfx 1 + (- x 100)) ;it's valid!
	(nfx 1 + (- x (3 * 3))) ;it's ALSO valid!
	(nfx 1 + (- x 3 * 3)) ;err... this can give you unexpected behavior

...also, you can define your own infix symbols using definfix:
	(definfix my-infix-symbol
		:precedence 40
		:function-name +)
	(nfx 1 my-infix-symbol 2)
		=> 3

send any feedback, bugreports, bugfixes, and cute girls to:
	almkglor@gmail.com
|#

#|
This file defines a macro of the form (nfx ...) whose
parameters are a stream of LISP "tokens" (either symbols,
constants, or lists).  The stream of tokens is interpreted
in infix notation.  The macro then expands to the prefix
form equivalent to the infix form
Ex.
(macroexpand-1 '(nfx foo = 32))
	=> (setf foo 32)
(macroexpand-1 '(nfx bar == (3 + foo) * quux ))
	=> (equal bar (* (+ 3 foo) quux))

NOTE: detection of function calls
function call forms should be supported:
(macroexpand-1 '(nfx foo = (max (bar + 32) quux niaw) ))
	=> (setf foo (max (+ bar 32) quux niaw))
(macroexpand-1 '(nfx (cdr foo) = (cons (qux + 1) nil)))
	=> (setf (cdr foo) (cons (+ qux 1) nil))

If any infix notation is in a function call within an
nfx form, it should be within a parentheses:
(macroexpand-1 '(nfx (max (bar + foo) (quux + quuux)) ))
	=> (max (+ bar foo) (+ quux quuux))
(macroexpand-1 '(nfx (max bar + foo quux + quuux) ))
	=> (max bar + foo quux + quuux)

function calls are detected in the following manner:
if nfx detects a list in the input stream,
	if the second item is not a registered infix,
		function call, for each element recurse and replace the element
		not a function call, recurse on the list as a new stream

this allows us to use prefix operators (such as -) as-is:
(macroexpand-1 '(nfx (- bar) == (/ (foo + 1)) ))
	=> (equal (- bar) (/ (+ foo 1)))

|#

#|
while macro
(gosh, very sorry, but I never figured out the 'loop thingie...
lol how funny that I know 'do but not 'loop?!? I guess
it's too user friendly!)
|#
(defmacro while (cond &rest body)
	`(do
		()
		((null ,cond))
		,@body ))

#|
(nfx-operator-base <action> ...)
Handles the operator database
...erk.  my schemeishness is showing.  Gomennasai!
|#
(let	(
		(data
			(make-hash-table :test 'eq) ))
	(labels
		(
			(getval (sym)
				(gethash sym data) )
			(add (params)
				(setf (gethash (car params) data) (apply #'vector params)) ))
		(defun nfx-operator-base (action &rest params)
			(case action
				(:get (getval (car params)))
				(:add (add params)) ))))

#|
the nfx macro
|#
(defmacro nfx (&rest s)
	(let
		(
			opstack
			curop
			tmp
			(bldg (cons nil nil)) )
		(labels
			(
				#|
				gosh.  what's tconc in CL?!? You know, that
				tail concatenate with pointers to both head and
				tail?
				|#
				(tconc (l v)
					(car
						(if (car l)
							(setf (cdr l) (setf (cddr l) (cons v nil)))
							(setf (car l) (setf (cdr l) (cons v nil))) )))
				#|top-of-stack query|#
				(top-opstack ()
					(caaar opstack))
				#|handles any sub-lists|#
				(enlist (l)
					(if l
						(if (and (listp (cdr l)) (getop (cadr l)))
							(cons 'nfx l)
							(if (proper-list-p l)
								(mapcar
									#'(lambda (o)
										(if (listp o) (enlist o) o) )
									l )
								l))))
				#|
				pushes an operation onto the stack
				- used when the currently being built
				  operation is of lower precedence than
				  the operation being considered
				|#
				(push-oper ()
					(push bldg opstack)
					(setf bldg (cons nil nil)) )
				#|
				pops off an operation from the stack,
				- used before collapsing the call
				|#
				(pop-oper ()
					(let
						(
							(tmp (pop opstack)) )
						(tconc tmp (car bldg))
						(setf bldg tmp) ))
				#|sub-expression handling code|#
				(expr (o)
					(if (listp o)
						(enlist o)
						o ))
				#|
				determine if the specified operation has precedence
				over the operation currently being built
				|#
				(precedes? (op1 op2)
					(or
						(< (precedence op1) (precedence op2))
						(and
							(= (precedence op1) (precedence op2))
							(eq (associativity op1) :right) )))
				#|funges bldg: collapses fungible operations into one form|#
				(fungebldg (bldg)
					(mapcan
						#'(lambda (o)
							(if (listp o)
								(progn
									(setf o (fungebldg o))
									(if (and (fungible (car bldg)) (eq (car o) (car bldg)))
										(cdr o)
										(cons o nil)))
								(cons o nil)))
						bldg ))
				#|fixes bldg: changes operation objects to their functions|#
				(fixbldg (bldg)
					(when (vectorp (car bldg))
						(setf (car bldg) (function-name (car bldg))) )
					(mapc
						#'(lambda (o)
							(when (listp o)
								(fixbldg o) ))
						bldg ))
				#|error-handling function|#
				(err (l) (print 'error-improper-expression) (print l) (error 'error))
				#|accessor functions|#
				(precedence (op) (if op (aref op 1) 99999))
				(associativity (op) (aref op 2))
				(function-name (op) (aref op 3))
				(fungible (op) (when (vectorp op) (aref op 4)))
				#|get the data for a (presumed) infix operator|#
				(getop (op)
					(nfx-operator-base :get op) ))
			(if (proper-list-p s)
				(case (length s)
					(1 (expr (car s)))
					(2 (err s))
					(t
						(do
							(#|variable and step list|#
								(oL s (cddr oL)) )
							(#|termination condition|#
								(null (cddr oL))
								(tconc bldg (expr (car oL)))
								(when (cdr oL)
									(err oL) )
								(while opstack (pop-oper))
								;(print bldg)
								(fixbldg (fungebldg (caar bldg))))
							#|do code|#
							;(print oL)
							;(print opstack)
							;(print (car bldg))
							(if (setf curop (getop (cadr oL)))
								(progn
									(if (precedes? curop (caar bldg))
										(progn
											(push-oper)
											(tconc bldg curop)
											(tconc bldg (expr (car oL))) )
										(progn
											(tconc bldg (expr (car oL)))
											#|collapse while the stacktop is precedent over the current op|#
											(while (precedes? (top-opstack) curop) (pop-oper))
											(setf tmp (car bldg))
											(setf bldg (cons nil nil))
											(tconc bldg curop)
											(tconc bldg tmp) )))
								(err oL) ))))
				s))))

#|
(definfix <operator>
	:precedence <integer>
	[:associativity <:left | :right>]
	[:function-name <actual function>]
	[:fungible <t | nil>])
- defines an infix operator with the symbol <operator>,
  with a precedence of <integer>.  The smaller the precedence
  number, the more precedence it has: * and / have smaller
  precedence number than + and -.
- associativity defaults to :left, which means that if two
  operators of the same precedence are encountered, the first
  one resolves first: 1 x 2 x 3 becomes ((1 x 2) x 3).  This
  is appropriate for most maths.  :right associativity means
  that 1 x 2 x 3 becomes (1 x (2 x 3)).  This is appropriate
  for assignment.  IMPORTANT: operators with the same
  precedence must have the same associativities!
- function-name defaults to the same symbol as the operator.
  For example, the function-name of = is setf.
- fungible means that if the same operator is encountered
  several times, then all inputs are funged into one function
  call.  For example, 1 + 2 + 3 becomes (+ 1 2 3), 1 < 2 < 3
  becomes (< 1 2 3).  This is not true for assignment:
  x = y = z should become (setf x (setf y z)), not (setf x y z)
- fungible defaults to t because I noticed that nearly every
  single darned operator was fungible.  Except assignment.
  Saved some dozen lines of code too.
|#
(defmacro definfix (
		operator
		&key
		precedence
		(associativity :left)
		function-name
		(fungible t))
	`(nfx-operator-base :add
		(quote ,operator)
		,precedence
		,associativity
		(quote
			,(if function-name
				function-name
				operator ))
		,fungible ))

#|
Predefined operators
Note: precedences are divisibles of 10, in case you
want to insert precedence levels between levels.
|#
(definfix ->
	:precedence 10
	:function-name slot-value-q
	:fungible nil)
#|
since we expect infix notation only (not infix-postfix),
our array accessor is a single infix @
|#
(definfix @
	:precedence 10
	:function-name aref)
(definfix **
	:precedence 20
	:function-name expt)
(definfix *
	:precedence 30)
(definfix /
	:precedence 30)
(definfix %
	:precedence 30
	:function-name mod)
(definfix +
	:precedence 40)
(definfix -
	:precedence 40)
(definfix <=
	:precedence 60)
(definfix <
	:precedence 60)
(definfix >=
	:precedence 60)
(definfix >
	:precedence 60)
(definfix ===
	:precedence 70
	:function-name eq
	:fungible nil)
(definfix ==
	:precedence 70
	:function-name equal
	:fungible nil)
(definfix !=
	:precedence 70)
(definfix &
	:precedence 80
	:function-name logand)
(definfix ^
	:precedence 90
	:function-name logxor)
#|
100 should be for the bitwise operator |, but
there's a mini-problem: | is not a valid symbol!
|#
(definfix &&
	:precedence 110
	:function-name and)
(definfix ||
	:precedence 120
	:function-name or)
(definfix :
	:precedence 130
	:function-name values)
(definfix =
	:precedence 140
	:associativity :right
	:function-name setf
	:fungible nil)
(definfix +=
	:precedence 140
	:associativity :right
	:fungible nil)
(definfix -=
	:precedence 140
	:associativity :right
	:fungible nil)
(definfix *=
	:precedence 140
	:associativity :right
	:fungible nil)
(definfix /=
	:precedence 140
	:associativity :right
	:function-name div=
	:fungible nil)
(definfix %=
	:precedence 140
	:associativity :right
	:function-name mod=
	:fungible nil)

#|
a few strategic macros
|#
(defmacro nfx-assign-maker (s)
	(let	(
			(str (symbol-name s)))
		`(defmacro ,(intern (concatenate 'string str "=")) (l r)
			`(setf ,l (,',s ,l ,r)))))
(nfx-assign-maker +)
(nfx-assign-maker -)
(nfx-assign-maker *)
(nfx-assign-maker mod)
#|this is done because the default of /= is "not equal"|#
(defmacro div= (l r)
	`(setf ,l (/ ,l ,r)))
(defmacro != (&rest l)
	`(not (equal ,@l)))
(defmacro slot-value-q (o s)
	`(slot-value ,o (quote ,s)))
(set-dispatch-macro-character #\# #\n
	#'(lambda (stream c1 c2)
		(let
			(
				(rd (read stream t nil t)))
			(if (listp rd)
				`(nfx ,@rd)
				(progn
					(print 'invalid-#n-usage)
					(print rd)
					(error 'error) )))))

#|
okay, tell you what. I'll prove to you just how schemeish
I am.  I kept getting the following error:
*** - EVAL: undefined function DEFINE
<igs!>
|#
