;#lang racket

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (foldr op acc lst)
  (if (null? lst)
      acc
      (op (car lst) (foldr op acc (cdr lst)))))

(define (foldl op acc lst)
  (if (null? lst)
    acc
    (foldl op (op acc (car lst)) (cdr lst))))

(define (filter p lst)
  (if (null? lst)
      (list)
      (if (p (car lst))
          (cons (car lst) (filter p (cdr lst)))
          (filter p (cdr lst)))))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
          (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

(define (1+ x) (+ x 1))
(define (id x) x)

;(apply + '(1 2 3))
;(map (lambda (x) (+ x 1)) '(1 2 3))
;(filter (lambda (x) (< x 5)) '(1 2 3 4 5 6 7))
;(foldl (lambda (x acc) (- x acc)) 0 '(1 2 3 4) )
;(foldr (lambda (x acc) (- x acc)) 0 '(1 2 3 4) )

