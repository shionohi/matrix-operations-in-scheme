#lang racket

;;; Title : numsch.rkt
;;; Author : Hiroyuki Shiono
;;; Date : 10/28/2021
;;; Explanation : This code provides the basic matrix operations which is applicable to m x n dimensional array.
;;; You can implement the m x n dimensional array using the structure of (list (list ... ) (list ... ) ...).
;;; as examples below suggested. 
;;; All lists in the list of matrix have must be same lengths and all elements in the matrix must be numbers.

(provide matrix.add
         matrix.dot
         matrix.mean
         matrix.subtract
         matrix.transpose
         matrix.shape
         matrix.zeroes)

;;; Sample matrices : 
;;; (define a (list (list 1 2) (list 3 4)))
;;; (define b (list (list 1 3 4 5) (list 1 5 6 7)))

;;; proc : matrix.shape
;;; input :
;;; m : a matrix, a list of lists
;;; output :
;;; a pair of y and x, where y is the number of m's row (integer)
;;; and x is the number of m's column (integer)

(define matrix.shape
  (lambda (m)
    (cons (length m) (length (car m)))))

;;; example : 
;;; (matrix.shape a)
;;; '(2 . 2)
;;; (matrix.shape b)
;;; '(2 . 4)

;;; proc : nth-vertical
;;; input :
;;; m : a matrix, a list of lists
;;; n : an integer, where 0 <= n < (cdr (matrix.shape m))
;;; output :
;;; a list, which contains all elements in m[][n]
;;;
;;; example :
;;; 2 x 2 dimensional matrix
;;;     2 | 4                               
;;; m = - - - -> slice-by-vertical(m, 0) -> (list 2 1)
;;;     1 | 3                               

(define nth-vertical
  (lambda (m n)
    (map (lambda (l)
           (list-ref l n))
         m)))

;;; proc : each-line
;;; input :
;;; a : a list of numbers
;;; b : a list of lists
;;; output : a list of lists, initially given as an empty list
;;; index : an integer, 0 <= index < (cdr (matrix.shape m))
;;; 
;;; output :
;;; a list, which contains the dot product of a and b

(define each-line
  (lambda (a b output index)
    (if (equal? (cdr (matrix.shape b)) index)
        output
        (each-line a b (append output (list (apply + (map (lambda (num1 num2)
               (* num1 num2))
              (nth-vertical b index)
              a)))) (+ index 1)))))


;;; proc : matrix.dot-helper
;;; input :
;;; m1 : a matrix, a list of lists
;;; m2 : a matrix, a list of lists
;;; output : a list of lists, initially defined as an empty list
;;;
;;; output :
;;; a matrix, a list of lists which is a dot product of m1 and m2

(define matrix.dot-helper
  (lambda (m1 m2 output)
    (if (null? m1)
        output
        (matrix.dot-helper (cdr m1) m2 (append output (list (each-line (car m1) m2 '() 0)))))))

;;; proc : matrix.dot
;;; input :
;;; m1 : a matrix, a list of lists
;;; m2 : a matrix, a list of lists
;;; output :
;;; a matrix, a list of lists which is a dot product of m1 and m2

(define matrix.dot
  (lambda (m1 m2)
    (if (not (equal? (cdr (matrix.shape m1))
                     (car (matrix.shape m2))))
        #f
        (matrix.dot-helper m1 m2 '()))))

;;; example : 
;;; (matrix.dot a b)
;;; '((3 13 16 19) (7 29 36 43))

;;; proc : matrix.mean-helper
;;; input :
;;; size : an integer, the size of the m
;;; sum : a number, initially given as a 0
;;; m : a list of lists
;;; output :
;;; a number, the mean of m

(define matrix.mean-helper
  (lambda (size sum m)
    (if (null? m)
        (/ sum size)
        (matrix.mean-helper size (+ sum (apply + (car m))) (cdr m)))))

;;; proc : matrix.mean
;;; input :
;;; m : a list of lists
;;; output :
;;; a number, the mean of m

(define matrix.mean
  (lambda (m)
    (if (empty? m)
        #f
        (matrix.mean-helper (* (car (matrix.shape m))
                                 (cdr (matrix.shape m))) 0 m))))

;;; example :
;;; (matrix.mean a)
;;; 2+1/2
;;; (matrix.mean b)
;;; 4

;;; proc : matrix.add-helper
;;; input :
;;; m : a list of lists
;;; val : a number
;;; output : a list of lists, initially given as an empty list
;;; 
;;; output :
;;; a list of lists, where all elements in the output are m + val

(define matrix.add-helper
  (lambda (m val output)
    (if (null? m)
        output
        (matrix.add-helper (cdr m) val (append output (list (map (lambda (num)
                                                                  (+ num val))
                                                                (car m))))))))

;;; proc : matrix.add
;;; input :
;;; m : a list of lists
;;; val : a number
;;; output :
;;; a list of lists, where all elements in the output are m + val

(define matrix.add
  (lambda (m val)
    (matrix.add-helper m val '())))

;;; example
;;; (matrix.add a 2)
;;; '((3 4) (5 6))
;;; (matrix.add b -3)
;;; '((-2 0 1 2) (-2 2 3 4))

;;; proc : matrix.subtract-helper
;;; input :
;;; m : a list of lists
;;; val : a number
;;; output : a list of lists, initially given as an empty list
;;; 
;;; output :
;;; a list of lists, where all elements in the output are m - val

(define matrix.subtract-helper
  (lambda (m val output)
    (if (null? m)
        output
        (matrix.subtract-helper (cdr m) val (append output (list (map (lambda (num)
                                                                  (- num val))
                                                                (car m))))))))

;;; proc : matrix.subtract
;;; input :
;;; m : a list of lists
;;; val : a number
;;; output :
;;; a list of lists, where all elements in the output are m - val
        
(define matrix.subtract
  (lambda (m val)
    (matrix.subtract-helper m val '())))

;;; example
;;; (matrix.subtract a 2)
;;; '((-1 0) (1 2))
;;; (matrix.subtract b -3)
;;; '((4 6 7 8) (4 8 9 10))

;;; proc : matrix.transpose-helper
;;; input :
;;; m : a list of lists
;;; y : an integer, the size of the column of m
;;; output: a list of lists, intially given as an empty list
;;; index : an integer, 0 <= index < y
;;; 
;;; output :
;;; a list of lists, where all elements in the output tranpose(m)

(define matrix.transpose-helper
  (lambda (m y output index)
    (if (equal? y index)
        output
        (matrix.transpose-helper m y
                                 (append output
                                             (list (nth-vertical m index)))
                                 (+ 1 index)))))

;;; proc : matrix.transpose
;;; input :
;;; m : a list of lists
;;; 
;;; output :
;;; a list of lists, where all elements in the output tranpose(m)

(define matrix.transpose
  (lambda (m)
    (matrix.transpose-helper m (cdr (matrix.shape m)) '() 0)))

;;; example
;;; (matrix.transpose a)
;;; '((1 3) (2 4))
;;; (matrix.transpose b)
;;; '((1 1) (3 5) (4 6) (5 7))

;;; proc : matrix.zeroes-helper
;;; input :
;;; x : an integer
;;; y : an integer
;;; output : a list of lists, initially given as an empty list
;;; 
;;; output :
;;; a list of lists, x by y dimensional matrix where all values are zeroes.

(define matrix.zeroes-helper
  (lambda (x y output)
    (if (equal? x 0)
        output
        (matrix.zeroes-helper (- x 1) y (append output (list (make-list y 0)))))))

;;; proc : matrix.zeroes
;;; input :
;;; x : an integer
;;; y : an integer
;;; output :
;;; a list of lists, x by y dimensional matrix where all values are zeroes.

(define matrix.zeroes
  (lambda (x y)
    (matrix.zeroes-helper x y '())))

;;; example : 
;;; (matrix.zeroes 3 0)
;;; '(() () ())
;;; (matrix.zeroes 7 2)
;;; '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0))
