#lang scheme
(require "refresh.rkt")
(require "refresh2.rkt")
(require "refresh3.rkt")
(require "refresh4.rkt")
(provide (all-defined-out))
;;rember-f
(define rember-f-my
  (lambda (test? a l)
    (cond
      [(null? l) '()]
      [(test? a (car l)) (cdr l)]
      [else
       (cons (car l) (rember-f-my test? a (cdr l)))])))
;;curring (haskell curring)
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) '()]
        [(test? a (car l)) (cdr l)]
        [else
         (cons (car l) ((rember-f test?) a (cdr l)))]))))

;;a general structure
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        [(null? l) '()]
        [(eq? old (car l))
         (seq new old (cdr l))]
        [else
         (cons (car l) ((insert-g seq) new old l))]))))
;;MAGIC!!
(define seqrem
  (lambda (new old l)
    l))
(define rember-mag
  (lambda (a l)
    ((insert-g seqrem) #f a l)))
;;#f is a empty space!!!
(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) o+)
      ((eq? x (quote *)) o*)
      (else o^))))
;;to turn a value to a procedure!
(define value-magic
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [else
       ((atom-to-function (operator nexp))
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))])))
;;general structure for multirember
(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) '()]
        [(test? a (car l))
         ((multirember-f test?) a (cdr l))]
        [else
         (cons (car l)
               ((multirember-f test?) a (cdr l)))]))))

;;really magic
;;componet
(define eq-a?
  (eq?-c 'a))
(define multiremberT
  (lambda (test? lat)
    (cond
      [(null? lat) '()]
      [(test? (car lat))
       (multiremberT test? (cdr lat))]
      [else
       (cons (car lat)
             (multiremberT test? (cdr lat)))])))
(define multirember-magic
  (lambda (lat)
    (multiremberT eq-a? lat)))
;;cps!
(define multirember&co
  (lambda (a lat col)
    (cond
      [(null? lat)
       (col '() '())]
      [(eq? (car lat) a)
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen))))]
      [else
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))])))
(define a-friend
  (lambda (x y)
    (null? y)))
;;multiinsertLR
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) oldL)
       (cons new (cons oldL
                       (multiinsertLR new oldL oldR (cdr lat))))]
      [(eq? (car lat) oldR)
       (cons oldR (cons new
                        (multiinsertLR new oldL oldR (cdr lat))))]
      [else
       (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))])))
;;multiinsertLR&co
;;col function
(define help
  (lambda (a b c)
    (print (cons b c))
    (print a)))
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      [(null? lat)
       (col '() 0 0)]
      [(eq? (car lat) oldL)
       (multiinsertLR&co
        new oldL oldR (cdr lat)
        (lambda (newlat L R)
          (col (cons new (cons oldL newlat))
               (succ L)
               R)))]
      [(eq? (car lat) oldR)
       (multiinsertLR&co
        new oldL oldR (cdr lat)
        (lambda (newlat L R)
          (col (cons oldR (cons new newlat))
               L
               (succ R))))]
      [else
       (multiinsertLR&co
        new oldL oldR (cdr lat)
        (lambda (newlat L R)
          (col (cons (car lat) newlat) L R)
          ))])))
;;even?
(define even?
  (lambda (n)
    (= (o* (o/ n 2) 2) n)))
;;evens-only
(define evens-only*
  (lambda (l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(even? (car l)) (evens-only* (cdr l))]
         [else
          (cons (car l) (evens-only* (cdr l)))])]
      [else
       (cons
        (evens-only* (car l))
        (evens-only* (cdr l)))])))
;; evens-only*&co
(define evens-only*&co
  (lambda (l col)
    (cond
      [(null? l) (col '() 1 0)]
      [(atom? (car l))
       (cond
         [(even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newlat multi add)
                            (col
                             (cons (car l) newlat)
                             (o* (car l) multi)
                             add)))]
         [else
          (evens-only*&co (cdr l)
                          (lambda (newlat multi add)
                            (col
                             newlat
                             multi
                             (o+ (car l) add)
                             )))])]
      [else
       (evens-only*&co (car l)
                       (lambda (newlat multi add)
                         (evens-only*&co (cdr l)
                                         (lambda (a b c)
                                           (col
                                            (cons newlat a)
                                            (o* multi b)
                                            (o+ add c))))))])))
(define the-last-friend
  (lambda (a b c)
    (print a)
    (print b)
    (print "_")
    (print c)))
;;;looking
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))
(define keep-looking
  (lambda (a sorn lat)
    (cond
      [(numbered? sorn)
       (keep-looking a (pick sorn lat) lat)]
      [else
       (eq? sorn a)])))
;;shift
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))
(define align
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(a-pair? (first pora))
       (align (shift pora))]
      [else
       (build (first pora)
              (align (second pora)))])))
;;length*
(define length*
  (lambda (l)
    (cond
      [(null? l) 0]
      [(atom? (car l))
       (succ (length* (cdr l)))]
      [else
       (o+ (length* (car l)) (length* (cdr l)))])

    ))
(define weight*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else
       (o+ (o* 2 (weight* (first pora)))
          (weight* (second pora)))])))
;;Collatz list
(define C
  (lambda (n)
    (cond
      [(one? n) 1]
      [else
       (cond
         [(even? n) (C (o/ n 2))]
         [else
          (C (succ (o* 3 n)))])])))
;;if this is a partial function?
;;Ackermann function
(define A
  (lambda (n m)
    (cond
      [(zero? n) (succ m)]
      [(zero? m) (A (pred n) 1)]
      [else (A (pred n) (A n (pred m)))])))
(define l '(apples))
(define eternity (lambda (x) (eternity x)))
