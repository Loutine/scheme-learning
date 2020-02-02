#lang racket
(define exerct
  (lambda (mat)
    (cond
      ((null? mat) '())
      (else
       (cons (car (car mat)) (exerct (cdr mat)))))))
(define remain
  (lambda (mat)
    (cond
      ((null? mat) '())
      (else
       (cons (cdr (car mat)) (remain (cdr mat)))))))
(define transpose
  (lambda (mat)
    (cond
      ((null? mat) '())
      ((null? (car mat)) '())
      (else
       (cons (exerct mat) (transpose (remain mat)))))))
(transpose '((9 0 0)
             (1 3 4)
             (1 2 3)
             (8 9 3)
             (1 3 5)
             (8 7 6)
             (3 1 4)
             (9 1 2)
             (12 44 5))
           )
(define vecplus
  (lambda (char add vec)
    (cond
      [(null? vec) '()]
      [else
       (cons (char add (car vec))
             (vecplus char add (cdr vec)))]
      )))
(vecplus + 3 '(3 5 6 1))
