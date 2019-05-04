---
layout: post
title: Lisp Cheat Sheet
tags: lisp
excerpt_separator: <!--more-->
---

A simple Lisp Cheat Sheet

<!--more-->

## Lists
Lists in lisp are recursively defined single linked lists. That means that a list is actually an element cons a list or an element cons nill 

`()`							|Nil
`cons 1 nil`					|A list with a single element, \[1\]
`cons 1 '(2 3)`					|A list with three elements, \[1,2,3\]
`(list 1 2 3)`					|returns the list (1 2 3)
`(length '(1 2 3))`				|Length of the list
`(first '(1 2 3))`				|first element of list, 1
`(car '(1 2 3))`				|first element of list, 1
`(rest '(1 2 3))`				|the original list without the first element, \[2,3\]
`(cdr '(1 2 3))`				|the original list without the first element, \[2,3\]
`(nth 2 '(23 34 45))`			|returns the third element of the list (lists start on 0)
`(append '(1 2 )` <br>`  '(3 4))`  |concatenates two lists
`(reverse '(1 2 3))`        	|reverses a list, \[3,2,1\]

## Functions

`(defun sum (a b)` <br>` (+ a b))`			|Defines a function that takes two arguments `a` and `b`, and returns the sum of them
`(defun dice ()` <br>` (+ 1 (random 6)))`	|A function that takes no arguments

## Variables

`(defparameter foo 1)`        	|Global variable named foo whose value is 1
`(setf foo 2)`               	|Sets the value of foo to 2
`(let* ((var 5)) body)`			|Local variable called var, valued 5, when `body` is evaluated. 

## Strings

`""`                        	|Empty string
`"Hello World!"`            	|A string that says "Hello World!"
`"this is a double quote \""` 	|Double quotes can be scaped inside a string
`(length "Lisp")`            	|length(number of characters) of the string "Lisp"
`(reverse "dog")`            	|reverses the word "dog"
`(concatenate "hello" " world")` |Concatenates two or more strings
`(subseq "hello" 1 3)`     		|Returns a substring from the character 1 to the character 3. "el"
`(string a)`					|Converts an object to a string. Useful for concatenating a character to a string.

## Comparison 

`(eq 'fred 'fred)`				|Check if two objects are identical. Returns either `NIL` or `T`
`(= 2 2)` <br>`(> 3 3)` <br>`(/= 3 3)` <br>`(>= 3 3)`			|Numerical comparison 
`(evenp 17)` <br>`(oddp 17)`		|Checks if even or odd
`(string= "cat" "cat")`			|Compares strings
`(numberp 2)`					|Checks if something is a number
`(listp '(1 2 3))`				|Checks if something is a list
`(null nil)`					|Checks if something is a `NIL`

## Logic Operators

`(and A B)`						|Takes any number of argumenst. If all arguments evaluate to non-nil, then the value of the last argument is returned
`(or A B)`						|The arguments are evaluated left to right until one evaluates to non-nil, in such case the argument value is returned, otherwise it returns nil.
`(not A)`						|It takes one argument and returns t if the argument evaluates to nil.

## Conditionals

`(if (evenp a)` <br>`  (print "Answer is even")` <br>`  (print "Answer is odd"))` |`if` takes three parameters. It evaluates the first parameter; if it returns a non-nil value, it evaluates and returns the second parameter; otherwise it evaluates and returns the third parameter. 
`(when test A B ... )`	|first evaluates `test`. If the result is nil no form is evaluated and nil is returned. Else, the forms are evaluated as a progn.
`(unles test A B ... )`	|Equivalent to <br> `(when (not test) A B ... )`
`(cond` <br>`  (test-1 A B ...)` <br>`  (test-2)` <br>`  (test-3 C ...)` <br>`  ... )`  |The first clause whose test evaluates to non-nil is selected; all other clauses are ignored, and the consequents are evaluated as an implicit `progn`. Like guards in other programming languages
`(case keyform` <br>`(keylist-1 A B ...)` <br>`(keylist-2 C ...)` <br>`...)` |Evaluate the form `keyform`. If it is in the keylist of a clause, the consequents of that clause are evaluated as an implicit progn.

## Misc

`(print 123)`					|Prints something in console
`(format t "val: ~a" 2)`		|Prints "val: 2" to terminal
`(format nil "val: ~a" 2)`		|returns "val: 2" 
`(max 1 3)`						|returns the max of two or more values
`(min 3 2)`						|returns the min of two or more values
`(progn` <br>`  (print "Answer is even")` <br>` 0)`|`progn`  allows you to wrap several procedures into one bracketed list. Evaluates the expressions in order. Returns the value of the last expression
`(dolist (item list)` <br>`body) `|Performs a operation `body` on every item `item` in a `list`.


