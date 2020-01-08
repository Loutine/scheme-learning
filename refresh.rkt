#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
;; if normal pair? f null? f all t
;; if not    pair? t null? f all f
;; if '()    pair? f null? t all f
(define lat?
  (lambda (x)
    (cond
      [(null? x) #t]
      [(atom? (car x)) (lat? (cdr x))]
      [else #f])))
;;defination of lat? use for list not for S-expression
(define member?
  (lambda (a lat)
    (and
     (not (null? lat))
     (or (eq? (car lat) a) (member? a (cdr lat))))))
;;member? look up if element a in lat
;;or you can use a easy way to define it
(define rember?
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? a (car lat)) (cdr lat)]
      [else (cons (car lat) (rember? a (cdr lat)))])))
;; remove the first member
(define firsts
  (lambda (l)
    (cond
      [(null? l) '()]
      [(atom? (car l)) (firsts (cdr l))]
      [else (cons (car (car l)) (firsts (cdr l)))])))
;;take the ever first of all element
(define insertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons (cons old new) (cdr lat))]
      [else (cons (car lat) (insertR new old (cdr lat)))])))
;;insertR once in a list
(define insertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car (cdr lat))) (cons (cons (car lat) new) (cdr lat))]
      [else (cons (car lat) (insertL new old (cdr lat)))])))
;;insertL once in a list
(define (subst new old lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) old) (cons new (cdr lat))]
   [else (cons (car lat) (subst new old (cdr lat)))]))
;;subst use for  substitute a element in list
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) o1) (cons new (cdr lat))]
      [(eq? (car lat) o2) (cons new (cdr lat))]
      [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))])))
;;subst2 have 2 choices but only substitute for once
(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? a (car lat))
        (multirember a (cdr lat))]
      [else
       (cons
        (car lat)
        (multirember a (cdr lat)))])))
;;multirember rember the same element a in a list for several times
(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons old
             (cons new
                   (multiinsertR new old (cdr lat))))]
      [else (cons (car lat)
                  (multiinsertR new old (cdr lat)))])))
;;multiinsertR
(define multiinsertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car (cdr lat)) old)
       (cons (car lat) (cons new (multiinsertL new old (cdr lat))))]
      [else (cons (car lat)
                  (multiinsertL new old (cdr lat)))])))
;;multiinsertL
(define multisubst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons new (multisubst new old (cdr lat)))]
      [else (cons (car lat)
                  (multisubst new old (cdr lat)))])))
;;multisubst
(define tup+
  (lambda (tup1 tup2)
    (cond
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))])))
;;a function for tup added
(define succ
  (lambda (x)
    (+ x 1)))
(define pred
  (lambda (x)
    (- x 1)))
;;successor and predecessor
(define pow
  (lambda (n m)
    (cond
      [(zero? m) 1]
      [else (* n (pow n (pred m)))])))
;;a function for pow
(define divide
  (lambda (n m)
    (cond
      [(zero? n) 0 (print "lasdf")]
      [(< n m) 0]
      [else (succ (divide (- n m) m))])))
;;a not perfect demo of divide
