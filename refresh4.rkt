#lang racket
(require "refresh.rkt")
(require "refresh2.rkt")
(require "refresh3.rkt")
(provide (all-defined-out))
;;set?-my maybe too slow
(define set?-my
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [else
       (and (not (member (car lat) (cdr lat)))
            (set?-my (cdr lat)))])))
;;the standard set?
(define set?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [(member? (car lat) (cdr lat))
       #f]
      [else (set? (cdr lat))])))
;; and the make set
(define makeset
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [else
       (cons
        (car lat)
        (makeset
         (multirember (car lat)
                      (cdr lat))))])))
;;multirember return a lat without (car lat)
;;my way to define subset?
(define subset?-my
  (lambda (set1 set2)
    (cond
      [(null? set1) #t]
      [else
       (and
        (member? (car set1) set2)
        (subset? (cdr set1) set2))])))
;;too slow it will check all the element in a lat
;;in fact it is the way book mentioned after
(define subset?
  (lambda (set1 set2)
    (cond
      [(null? set1) #t]
      [(member? (car set1) set2)
       (subset? (cdr set1) set2)]
      [else #f])))
;;eqset?
;;if two sets are equal they are the subset of each other
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))
;;intersect?
(define intersect?
  (lambda (set1 set2)
    (cond
      [(null? set1) #f]
      [(member? (car set1) set2) #t]
      [else
       (intersect? (car (cdr set1) set2))])))
(define intersect?-my
  (lambda (set1 set2)
    (cond
      [(null? set1) #f]
      [else
       (or (member? (car set) set2)
           (intersect?-my (cdr set1) set2))])))
;;union
(define union
  (lambda (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2)
       (union (cdr set1) set2)]
      [else
       (cons (car set1)
             (union (cdr set1) set2))])))
(define diff
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2)
       (diff (cdr set1) set2)]
      [else
       (cons (car set1)
             (diff (cdr set1) set2))])))
;;intersectall ****
;;;help function
;;1 same
(define intersect
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2))]
      [else
       (intersect (cdr set1) set2)])))
(define intersectall
  (lambda (l-set)
    (cond
      [(null? l-set) '()]
      [(null? (cdr l-set)) (car l-set)]
      [else
       (intersect (car l-set) (intersectall (cdr l-set)))])))
;;so funny that I never write intersect but write a same have the same function (too careless)
;;a-pair?
(define a-pair?
  (lambda (x)
    (cond
      [(atom? x) #f]
      [(null? x) #f]
      [(null? (cdr x)) #f]
      [(null? (cdr (cdr x))) #t]
      [else #f])))
;;some tools for pair
(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car (cdr p))))
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))
(define third
  (lambda (l)
    (car (cdr (cdr l)))))
;;all-pairs
(define fun?
  (lambda (rel)
    (set? (firsts rel))))
;;revrel
(define revrel
  (lambda (rel)
    (cond
      [(null? rel) '()]
      [else
       (cons (build (second (car rel)) (first (car rel)))
             (revrel (cdr rel)))])))
;;seconds
(define seconds
  (lambda (rel)
    (cond
      [(null? rel) '()]
      [else
       (cons (car (cdr (car rel)))
             (seconds (cdr rel)))])))
;;fullfun?
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
