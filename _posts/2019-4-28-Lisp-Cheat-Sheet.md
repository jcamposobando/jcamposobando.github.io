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

`()`                        |Nil
`cons 1 nil`                |A list with a single element, \[1\]
`cons 1 '(2 3)`                |A list with three elements, \[1,2,3\]
`(list 1 2 3)`                |returns the list (1 2 3)
`(length '(1 2 3))`            |Length of the list
`(first '(1 2 3))`            |first element of list, 1
`(car '(1 2 3))`            |first element of list, 1
`(rest '(1 2 3))`            |the original list without the first element, \[2,3\]
`(cdr '(1 2 3))`            |the original list without the first element, \[2,3\]
`(nth 2 '(23 34 45))`        |returns the third element of the list (lists start on 0)
`(append '(1 2 ) '(3 4))`    |concatenates two lists
`(reverse '(1 2 3 4))`        |reverses a list, \[4,3,2,1\]

## Functions

`(defun sum (a b) (+ a b))` |Defines a function that takes two arguments `a` and `b`, and returns the sum of them
`(defun dice () (+ 1 (random 6)))` |A function that takes no arguments

## Variables

`(defparameter foo 1)`         |Global variable named foo whose value is 1
`(setf foo 2)`                |Sets the value of foo to 2
`(let* ((var 5)) body)`        |Local variable called var, valued 5, when `body` is evaluated. 

## Strings

`""`                        |Empty string
`"Hello World!"`            |A string that says "Hello World!"
`"this is a double quote \""` |Double quotes can be scaped inside a string
`(length "Lisp")`            |length(number of characters) of the string "Lisp"
`(reverse "dog")`            |reverses the word "dog"
`(concatenate "hello" " world")` |Concatenates two or more strings
`(subseq "hello" 1 3)`     	|Returns a substring from the character 1 to the character 3. "el"
`(string a)`				|Converts an object to a string. Useful for concatenating a character to a string.
