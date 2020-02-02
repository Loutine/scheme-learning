#lang scheme
(require "refresh.rkt")
(require "refresh2.rkt")
(provide (all-defined-out))
;;define some char
(define o^
  (lambda (n m)
    (cond
     [(zero? m) 1]
     [else
      (* n (o^ n (pred m)))])))
(define o+
  (lambda (n m)
    (cond
      [(zero? n) m]
      [else
       (o+ (pred n) (succ m))])))
(define o-
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else
       (o- (pred n) (pred m))])))
(define o*
  (lambda (n m)
    (cond
      [(zero? n) 0]
      [(zero? (pred n)) m]
      [else
       (o* (pred n) (o+ m m))])))
(define o/
  (lambda (n m)
    (cond
      [(< n m) 0]
      [else
       (succ (o/ (- n m) m))])))
;;deal all the arithmetic expression as S conj P 
(define numbered?
  (lambda (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))])))
;;but it is not strict I found another way to do so
(define numbered?-s
  (lambda (aexp)
    (define arithmetic-char
      '(+ * o^))
    (cond
      [(atom? aexp) (number? aexp)]
      [else
       (and (numbered?-s (car aexp))
            (member? (car (cdr aexp)) arithmetic-char)
            (numbered?-s (car (cdr (cdr aexp)))))])))
;; value:
;; 1st-sub exp
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))
;; operator
(define operator
  (lambda (aexp)
    (car  aexp)))
;;2nd-sub-exp
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))
;; value function
(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [else
       (cond
         [(eq? (operator nexp) (quote +))
          (o+ (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))]
         [(eq? (operator nexp) (quote *))
          (o* (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))]
         [else
          (o^ (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))])])))
;; a problem about representation
;; only the truth-value and number we use representation
;; here is a kind of way to represent the number
(define (trans num)
  (cond
    [(null? num) 0]
    [else
     (succ (trans (cdr num)))]))
;; 0 '() 1 '(()) 2 '(()())
;;zero sero
(define (sero? num)
  (null? num))
;;add1 edd1
(define (edd1 num)
  (cons '() num))
;;sub1 zub1
(define (zub1 num)
  (cdr num))
;;but what about lat?
;;if (1 4 5) is lat? in representation it is not
(define o-add
  (lambda (n m)
    (cond
      [(sero? n) m]
      [else
       (o-add (zub1 n) (edd1 m))])))
;;add in representation
