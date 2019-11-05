#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b))
       (= a b))
      (else
       (eq? a b)))))
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2))
       #t)
      ((or (null? l1) (null? l2))
       #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and
        (eqan? (car l1) (car l2))
        (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2)))
       #f)
      (else
       (and
        (eqlist? (car l1) (car l2))
        (eqlist? (cdr l1) (cdr l2)))))))
(define rember+
  (lambda (s l)
    (cond
      ((null? l) '())
      ((and (atom? s) (atom? (car l)))
       (cond
         ((eqan? s (car l)) (cdr l))
         (else (cons (car l) (rember+ s (cdr l))))))
      ((atom? s)
       (cond
         ((eqlist? (car l) (rember+ s (car l)))
          (cons (rember+ (car l)) (cdr l)))
         (else (cons (car l) (rember+ s (cdr l))))))
      ((atom? (car l))
       (cons (car l) (rember+ s (cdr l))))
      (else
       (cond
         ((eqlist? s (car l)) (cdr l))
         ((eqlist? (car l) (rember+ s (car l)))
          (cons (car l) (rember+ s (cdr l))))
         (else (cons (rember+ s (car l)) (cdr l))))))))
      
