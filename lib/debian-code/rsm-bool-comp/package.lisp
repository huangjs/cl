;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package Definition For Boolean Equivalence.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.2 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.bool-comp
  (:use #:cl)
  (:documentation
   "DESCRIPTION: This file contains a Boolean solver that allows a user 
              to find a non-zero value (if it exists) of 
              a Boolean expression of the form f(x1,x2,...,xn). 
              Such a solver can be used, for instance, to 
              determine if two Boolean functions, f and g, are the 
              same by seeking to find a solution 
              of the Boolean function f ^ g (that is f XOR g). 
              The solver and related functions use a representation of 
              the function f as a \"term list\" which represents a Boolean
              function as an XOR sum of terms, where each term is a 
              vector which represents the AND'ing of
              variables, their negation, or 1. This representation is a 
              non-canonical one which, like a Boolean
              expression, can be a relatively compact way to represent a 
              Boolean function. However, without 
              a canonical form, we must use simplification to compare 
              a given function with the zero function.

              WORKING WITH THE REPRESENTATION: 
                Example of the representation: The \"term list\" 
                '(#(1 0 2) #(0 1 2)) represents the Boolean 
                function (X1 * X2') ^ (X1' * X2). We use the 
                following notation: \"*\" means AND ; \"^\" means XOR; 
                \"'\" appended to a variable, X, means NOT X; ~ <expression> 
                means NOT <expression>; and \"+\" means OR.
                The expression is interpreted by XORing each Boolean 
                expression derived from each term. Each term is decoded 
                by doing the following: For each index in the array 
                (1 based index), examine the number at that index. 
                If the number is 1 take the corresponding variable and 
                AND it with with the result so far. If the number is 0 
                take the corresponding variable, negate it, and AND it 
                with the result so far.
                Finally, if the number is 2, AND 1 with the result so far. 
                Example: So, for the term #(1 0 2), take the first 
                element of the array (at index 1) which is 1.
                Take the variable X1 and AND it with the next. 
                The next is X2 negated since the second element of  
                the array is 0 and we have X1 * X2'. Finally, with an 
                index of 3, the value of the array is 2 which means we 
                AND X1 * X2' with 1. The result is: X1 * X2'.

                Example: The \"term list\" '(#(1 0 2) #(0 1 2)) is 
                interpreted as \"X1 * X2' ^ X1' * X2\".
                NOTE: Boolean expressions here and in the code are 
                      interpreted with the following choice of 
                      operator symbols and their precedence:
                      NOT -> ~ or ', AND -> *, XOR -> ^, OR -> +.
                Therefore, X1 + X2*X3 means to AND X2 with X3 and 
                then OR that with X1.
              ANOTHER INTERPRETATION OF THE REPRESENTATION: 
                Boolean functions of n variables can be represented by 
                bit-strings of length 2^n.
                This representation is canonical but grows exponentially. 
                However, it is possible to 
                compactly represent certain Boolean functions as a 
                \"tensor product\" of binary pairs; and,
                represent an arbitrary Boolean function as an XOR 
                sum of such terms.
                One can translate a \"term\" in a \"term list\" to a 
                bit-string that represents the Boolean function 
                of that \"term\". This can be done as follows: 
                Interpret each of the 0, 1, 2 in the term array as 
                K, H, E respectively, where K = #(1 0), H = #(0 1), 
                E = #(1 1). Now take the tensor product of 
                each of these K, H, and E's. Example: #(1 0 2) 
                translates to H x K x E (here x means tensor product).
                That is, #(0 1) x #(1 0) x #(1 1) = #(0 1 0 0) x #(1 1) 
                = #(0 1 0 0 0 1 0 0).
                #(0 1 0 0 0 1 0 0) is the bit string representation of 
                the Boolean function of three variables 
                represented by #(1 0 2) namely, X1*X2'. 
                The bit-string is actually a listing of the values of the 
                truth table of the Boolean function X1*X2':
                 X1 X2 X3 -> X1*X2'
                 0  0  0     0
                 1  0  0     1
                 0  1  0     0
                 1  1  0     0
                 0  0  1     0
                 1  0  1     1
                 0  1  1     0
                 1  1  1     0
                Notice that the values under X1*X2' in the table read top 
                to bottom are the same as the array #(0 1 0 0 0 1 0 0).
                In fact, one can show that the functions X1, X2, X3 
                can be represented as H X E x E, E x H x E, and E X E X H 
                respectively. This view of Boolean functions as bit-strings
                motivated the algorithms below.
                NOTE: When there is a small number of variables, 
                      a bit-string representation is used rather than the
                      \"tensor product\" term list representation. In the 
                      functions below the bit-string representation is 
                      used if a Boolean function uses 12 or fewer variables.
 
              SYMMETRY: If a Boolean function of n variables is completely 
                        symmetric, it can be determined 
                        to be the zero function in at most n+1 evaluations. 
                        The solver function is-function-non-zero? takes 
                        advantage of symmetry information, as it finds it,
                        to simplify the complexity of the representation.

Export Summary:


              is-function-non-zero? is a low-level function that is used 
              to find a non-zero value of a Boolean function.
              It is low-level in the sense that it takes as input a 
              \"term list\" (defined above).
              The return value is NIL, if the function *is* the 
              zero function; otherwise, it returns a vector of zero's 
              and one's that indicate the values that each variable should
              take in order to get a non-zero value from the 
              Boolean function.
              Example: (is-function-non-zero? 
                         '(#(0 1 0 2) #(0 1 2 0) #(0 1 0 1) #(0 1 1 0)) 4)
                       nil
              This means this example has no solution.

              Example: (is-function-non-zero? 
                        '(#(2 2 1 2 1 0 1 2 0 1 1 1) 
                          #(2 2 1 0 1 1 0 1 2 2 1 1) 
                          #(2 2 2 2 1 1 0 2 1 1 1 2)) 12)
                       #(1 1 1 1 1 1 0 1 1 1 1 1)
              This means that setting each variable to the successive 
              values in the array #(1 1 1 1 1 1 0 1 1 1 1 1) and 
              evaluating the Boolean function represented by the term 
              list will yield 1.

              bool-solve is higher level version of is-function-non-zero? 
              but takes as input an ordinary Boolean expression.

              Example: (bool-solve \"X1 + X2*X3\")
               #(0 1 1)
              This means that one way to get a non-zero value of the 
              expression \"X1 + X2*X3\" is to 
              evaluate this function with X1=0, X2=1, and X3=1.

              Example: (bool-solve \"(X1' * X2 + X1 * X2') ^ 
                                     ( (X1 + X2) * ( X1' + X2') )\")
               NIL
              This means that there is no input that will yield a non-zero
              value; that is, the expression is really the zero function.

              bool-solve-from-file is like bool-solve but reads a 
              Boolean expression from a file.

              Example: (bool-solve-from-file file-name)
                If file-name is a file with contents X1 + X2*X3, this 
                function will return  #(0 1 1)

              rpn-eqn-tree->term-list takes an RPN Boolean expression 
              (a tree) and converts it to a tree of \"terms\".

              Example: (rpn-eqn-tree->term-list '(+ x1 x2 (* x3 x4) x5) 5)
               (+ #(1 2 2 2 2) #(2 1 2 2 2) (* #(2 2 1 2 2) 
                  #(2 2 2 1 2)) #(2 2 2 2 1))

              convert-tree->xor takes an RPN Boolean
              equation (a tree) of \"terms\" and converts it to an 
              XOR \"term list\". 

              Example: (convert-tree->xor 
                '(+ #(1 1 2) #(0 2 1) (* #(1 2 1) #(2 0 2))) 3)
               (#(0 2 1) #(1 1 2) #(1 0 1)

              formula->rep takes a Boolean expression and converts it 
              to a \"term list\". The user need not worry about inserting 
              white space between operators or variables.
              The function will also determine the number of variables.
              Example: (formula->rep \"X1 + X3\")
              (#(1 2 2) #(0 2 1))

              rep-from-file reads a Boolean expression from a file and 
              converts it to a \"term list\".
              The user need not worry about inserting white space 
              between operators or variables. 
              The function will also determine the number of variables. 

              list->eqn takes a \"term list\" and converts it to a 
              Boolean expression.

              Example: (list->eqn '(#(1 0 2) #(0 1 2)))
               X1*X2' ^ X1'*X2 

          NOTE: In the code below we use the word \"orthogonal\". 
                By the phrase \"term t1 is orthogonal to term t2\" 
                we mean that t1 ANDed with t2 is zero. 
                In bit-string terms, this means that the \"dot-product\"
                of the two bit-strings is zero.
")
  (:export #:is-function-non-zero?
           #:bool-solve
           #:bool-solve-from-file
           #:prn-eqn-tree->term-list
           #:convert-tree->xor
           #:formula->rep
           #:rep-from-file
           #:list->eqn))

