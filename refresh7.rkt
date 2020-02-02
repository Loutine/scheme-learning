#lang scheme
(require "refresh.rkt")
(require "refresh2.rkt")
(require "refresh3.rkt")
(require "refresh4.rkt")
(require "refresh5.rkt")
(provide (all-defined-out))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names))
       (car values))
      (else
       (lookup-in-entry-help name
                             (cdr names)
                             (cdr values)
                             entry-f)))))
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))
;;use help-function and make it easy

;;extend-table
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      [(null? table) (table-f table)]
      [else
       (lookup-in-entry )])))
;;extend-table
(define extend-table cons)

(define atom-to-action-new
  (lambda (e)
    (cond
      [(number? e) *const]
      [(or (eq? e #f)
           (eq? e #t)
           (eq? e 'cons)
           (eq? e 'car)
           (eq? e 'cdr)
           (eq? e 'null?)
           (eq? e 'eq?)
           (eq? e 'atom?)
           (eq? e 'zero?)
           (eq? e 'succ)
           (eq? e 'pred)
           (eq? e 'number?)) *const]
      [else *identifier])))
