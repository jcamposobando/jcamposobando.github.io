---
layout: post
title: Lisp in Small Parts (Solutions)
tags: lisp 
excerpt_separator: <!--more-->
---
I'm learning lisp with several tutorials. These are the solutions to some of the exercises proposed on [Lisp in Small Parts](http://lisp.plasticki.com/show?14F). I am learning, so these are not necessarily good or efficient solutions. If you have a suggestion please leave it in the comments =D

If you need a quick and simple way to test them, the best I found is this [online lisp interpreter by TutorialsPoint](https://www.tutorialspoint.com/execute_lisp_online.php).
<!--more-->

## Manipulating Lists
Find the excersices [here](http://lisp.plasticki.com/show?2EE)

### Swaping the first two elements of a list
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

### Duplicate the first item in a list
It stores the first element of the list in `a`, then it attaches it to the front of the list.
```
(defun dup (lst)
    (let* ( (a (first lst)))
    (cons a lst)))
```

### Return a random item from a list
First, it stores the `length` of the list in a local variable `ln`. Then it generates a `random` number between 0 and `ln`. Finally, it takes the `nth` element, where the `n` is the randomly generated number.
```
(defun random-elt (lst)
    (let* ( (ln (length lst)))
    (nth (random ln) lst)))
```
### Return the last item in a list
First, it stores the `length` of the list in a local variable `ln`, then it takes the element in the position ln - 1. (There must be a more efficient way of doing it using recursion)
```
(defun last-elt (lst)
    (let* ( (ln (length lst)))
    (nth (- 1 ln) lst)))
```

## Strings
Find the excersices [here](http://lisp.plasticki.com/show?HL)

### Reverse the middle letters of a word
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

### Rotate a string n places to the left
```
(defun rotate (str pos)
    (let* (
        (ln (length str))
        (fst (subseq str pos))
        (lst (subseq str 0 pos))
	) (concatenate 'string fst lst)))
```

## Printing
Find the excersices [here](http://lisp.plasticki.com/show?2EF)

### Use format to write a story-writing program. 
The procedure story should take a name, food, and colour; for example: `(story "Lisa" "cheese" "green")`
(this solution is not mine, I took it from [here](http://disq.us/p/1a38jr1) because I thought it was really funny.
```
(defun story (name food colour) 
  (format
    nil 
    "There once was a princess called ~a, who liked ~a. One day ~a found some ~a ~a and ate so much that she died. The end."
    name food name colour food))
```

## Testing a Result
Find the excersices [here](http://lisp.plasticki.com/show?1QF)

### Test whether a string is a palindrome
```
(defun palindrome? (str)
    (let* (
        (md (/ (length str) 2))
        (fst (subseq str (ceiling md)))
        (lst (reverse (subseq str 0 (floor md))))
    ) (string= fst lst)))
```

### Test whether an object is a list of two numbers
```
(defun numpair? (lst) 
    (and (numberp (car lst)) (numberp (cadr lst))))
```

## Processing Items in a List
Find the excersices [here](http://lisp.plasticki.com/show?HF)

### Count the number of elements in a list
```
(defun count-list (lst) 
    (if (null lst) 
        0
        (+ 1 (count-list(cdr lst )))))
```

### Reverse each string in a list of strings
```
(defun reverse-list (lst)
   (if (not (null lst))
       (append (reverse-list(cdr lst)) 
               (list (car lst)))))
```

### Find whether each number in a list is even or odd
```
(defun evenplist (lst)
    (if (null lst)
        nil
        (cons   (if (evenp (car lst)) t nil) 
                (evenplist (cdr lst)))))
```

### Find the maximum element of a list
```
(defun max-list (lst)
    (if (= (length lst) 1) 
        (car lst)
        (let* (
            (a (car lst))
            (b (max-list(cdr lst)))
            ) (if (> a b) a b))))
```

### Duplicate each element in a list
```
(defun dupli (lst)
    (if (null lst)
        nil
        (cons (car lst) 
              (cons (car lst) 
		    (dupli (cdr lst))))))
```

### Eliminate consecutive duplicates in a list

```
(defun compress (lst)
    (if (> (length lst) 1)
        (if (= (cadr lst) (car lst))
            (compress (cdr lst))
            (cons (car lst) (compress (cdr lst))))
        lst ))
```

### Interleave two lists

```
(defun interleave (l1 l2)
    (if (not(or (null l1) (null l2)))
        (cons(car l1) 
             (cons(car l2) 
                  (interleave (cdr l1) (cdr l2))))
        nil))
```

### Look up an entry in an association list

```
(defparameter *type* '(
        (cat mammal) 
        (dog mammal) 
        (lizard reptile)
        (salmon fish)))

(defun look (lst val)
    (if (and (not (null lst)) (eq (caar lst) val))
        (cdar lst)
        (look (cdr lst) val)))
```
