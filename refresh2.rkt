#lang racket
(require "refresh.rkt")
(provide (all-defined-out))
(define rember*
  (lambda (a l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eqan? (car l) a)
          (rember* a (cdr l))]
         [else (cons (car l) (rember* a (cdr l)))])]
      [else
       (cons (rember* a (car l))
             (rember* a (cdr l)))])))
;;rember-star
(define insertR*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eqan? (car l) old)
          (cons old (cons new (insertR* new old (cdr l))))]
         [else
          (cons (car l) (insertR* new old (cdr l)))])]
      [else
       (cons (insertR* new old (car l))
             (insertR* new old (cdr l)))])))
;;insertR*
(define insertL*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eqan? (car l) old)
          (cons new (cons old (insertL* new old l)))]
         [else
          (cons (car l) (insertL* new old l))])]
      [else
       (cons (insertL* new old (car l))
             (insertL* new old (cdr l)))])))
;;insertL-star
(define occur*
  (lambda (a l)
    (cond
      [(null? l) 0]
      [(atom? (car l))
       (cond
         [(eqan? (car l) a)
          (succ (occur* a (cdr l)))]
         [else
          (occur* a (cdr l))])]
      [else
       (+ (occur* a (car l)) (occur* a (cdr l)))])))
;;occur-star
(define subst*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eqan? old (car l))
          (cons new (subst* new old (cdr l)))]
         [else
          (cons (car l) (subst* new old l))])]
      [else
       (cons
        (subst* new old (car l))
        (subst* new old (cdr l)))])))
;;subst*
(define member*
  (lambda (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l))
       (or (eqan? (car l) a)
           (member* a (cdr l)))]
      [else
       (or (member* a (car l))
           (member* a (cdr l)))])))
;;member*
(define leftmost
  (lambda (l)
    (cond
      [(null? l) (print "No element")]
      [(not (pair? (car l))) (car l)]
      [else
       (leftmost (car l))])))
;;leftmost
(define reverse
  (lambda (l)
    (define core
      (lambda (lat acc)
        (cond
          ((null? lat) acc)
          (else (core (cdr lat) (cons (car lat) acc))))))
    (core l '())))
(define reverse*
  (lambda (l)
    (define core
      (lambda (s acc)
        (cond
          [(null? s) acc]
          [(atom? (car s))
           (core (cdr s) (cons (car s) acc))]
          [else (core (cdr s) (cons (core (car s) '()) acc))])))
    (core l '())))
(define rightmost
  (lambda (l)
    (leftmost (reverse* l))))
;;rightmost
;;it is not easy to build a rightmost like this but there is a more confused way but not so complicate

;;right-most
(define right-most
  (lambda (l)
    (cond
      [(null? l) '()]
      [(null? (cdr l))
       (cond
         [(atom? (car l))
          (car l)]
         [else (right-most (car l))])]
      [else (right-most (cdr l))] )))
;;eqlist?
(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2) #f)]
      [(and (atom? (car l1)) (atom? (car l2))
            (and (eqan? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2))))]
      [(or (atom? (car l1)) (atom? (car l2))) #f]
      [else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))])))
;;equal?
(define equal?
  (lambda (s1 s2)
    (cond
      [(and (pair? s1) (pair? s2))
       (eqlist?-my s1 s2)]
      [else
       (eqan? s1 s2)])))
;;it is too long on TLS use pair? is more efficent
(define eqlist?-my
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or  (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist?-my (cdr l1) (cdr l2))))))) 
;;IT JUST LIKE MAGIC!!!!!!!!!!!!!!!!!
(define rember+
  (lambda (s l)
    (cond 
      [(null? l) '()]
      [(equal? (car l) s)
       (rember+ s (cdr l))]
      [else
       (cons (car l) (rember+ s (cdr l)))])))

