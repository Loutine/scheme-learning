#lang racket
(define array
  (lambda (x p)
    (/ (+ x (/ p x)) 2)))
(define abs
  (lambda (a)
    (cond
      ((< a 0) (- 0 a))
      ((>= a 0) a))))
(define root
  (lambda (a g)
    (cond
      ((< (abs (- g (array g a))) 0.00001)
       (array g a))
      (else (root a (array g a))))))