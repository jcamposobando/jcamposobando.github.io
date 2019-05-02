---
layout: post
title: Lisp in Small Parts (Solutions)
tags: Lisp 
excerpt_separator: <!--more-->
---
I'm learning lisp with several tutorials. These are the solutions to some of the exercises proposed on [Lisp in Small Parts](http://lisp.plasticki.com/show?14F). I am learning, so these are not necessarily good or efficient solutions. If you have a suggestion please leave it in the comments =D

If you need a quick and simple way to test them, the best I found is this [online lisp interpreter by TutorialsPoint](https://www.tutorialspoint.com/execute_lisp_online.php).
<!--more-->

##[Manipulating Lists](http://lisp.plasticki.com/show?2EE)

###Swaping the first two elements of a list
It stores the first element of the list in `a`. Then, it stores the first element of the rest of the list in `b`. Then it stores the `rest` of the `rest` of the list in `c`
Finally, it `cons` all the parts in the correct order.
```
(defun swap (lst) 
    (let* (
        (a (first lst))
        (b (first (rest lst)))
        (c (rest (rest lst)))
    ) (cons b (cons a c))))
```

###Duplicate the first item in a list
It stores the first element of the list in `a`, then it attaches it to the front of the list.
```
(defun dup (lst)
    (let* ( (a (first lst)))
    (cons a lst)))
```

###Return a random item from a list
First, it stores the `length` of the list in a local variable `ln`. Then it generates a `random` number between 0 and `ln`. Finally, it takes the `nth` element, where the `n` is the randomly generated number.
```
(defun random-elt (lst)
    (let* ( (ln (length lst)))
    (nth (random ln) lst)))
```
###Return the last item in a list
First, it stores the `length` of the list in a local variable `ln`, then it takes the element in the position ln - 1. (There must be a more efficient way of doing it using recursion)
```
(defun last-elt (lst)
    (let* ( (ln (length lst)))
    (nth (- 1 ln) lst)))
```

##[Strings](http://lisp.plasticki.com/show?HL)

###Reverse the middle letters of a word
Keeps the first and the last letter of the word in place and reverses the rest.
(midverse "retinues") => "reunites"

This solution works but looks awful. In the last line, the `string` part is needed to convert `frst`, which is a single character, into a string. That's because only strings can be concatenated.

```
(defun midverse (str)
    (let* (
        (ln (length str))
        (frst (elt str 0))
        (midd (reverse (subseq str 1 (- ln 1 ))))
        (last (subseq str (- ln 1 ) ln )))
    (concatenate 'string (string frst) midd last)))
```
