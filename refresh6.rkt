#lang scheme
(require "refresh.rkt")
(require "refresh2.rkt")
(require "refresh3.rkt")
(require "refresh4.rkt")
(require "refresh5.rkt")
(provide (all-defined-out))

(define eternity (lambda (x) (eternity x)))
(define l1 '(apples))
;;about Y combinator
(((lambda (mk-length)
    (lambda (x)
      ((mk-length mk-length) x)))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (succ
               ((mk-length eternity)
                (cdr l))))))))
 l1)

;;((lambda (mk-length)
;;   (lambda (l)
;;     (cond
;;       ((null? l) 0)
;;       (else (succ
;;              ((mk-length eternity)
;;               (cdr l)))))))
;; (lambda (mk-length)
;;   (lambda (l)
;;     (cond
;;       ((null? l) 0)
;;       (else (succ
;;              ((mk-length eternity)
;;               (cdr l))))))))
;;((lambda (l)
;;   (cond
;;     ((null? l) 0)
;;     (else (succ
;;            (((lambda (mk-length)
;;                (lambda (l)
;;                  (cond
;;                    ((null? l) 0)
;;                    (else (succ
;;                           ((mk-length eternity)
;;                            (cdr l))))))) eternity)
;;             (cdr l)))))))
;;((lambda (l)
;;   (cond
;;     ((null? l) 0)
;;     (else (succ
;;            ((lambda (l)
;;               (cond
;;                 ((null? l) 0)
;;                 (else (succ
;;                        ((eternity eternity)
;;                         (cdr l))))))
;;             (cdr l)))))))
(((lambda (f)
     ((lambda (mk-length)
        (mk-length mk-length))
      (lambda (mk-length)
         (f (lambda (x)
              ((mk-length mk-length) x))))))
  (lambda (length)
    (lambda (l)
      (cond
        [(null? l) 0]
        [else
         (succ
          (length (cdr l)))]))))
 l1)
