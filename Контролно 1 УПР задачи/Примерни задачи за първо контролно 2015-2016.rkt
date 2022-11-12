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

;(apply + '(1 2 3))
;(map (lambda (x) (+ x 1)) '(1 2 3))
;(filter (lambda (x) (< x 5)) '(1 2 3 4 5 6 7))
;(foldl (lambda (x acc) (- x acc)) 0 '(1 2 3 4) )
;(foldr (lambda (x acc) (- x acc)) 0 '(1 2 3 4) )

;TASK 1 - TEMA 1

(define (id x) x)
(define (1+ x) (+ 1 x))

(define (min-sum-digit a b k)
  (define (get-sum-digits num)
    (if (= num 0)
        num
        (+ (remainder num 10) (get-sum-digits (quotient num 10)))))

  (if (> a b)
      '()
      (if (= (remainder (get-sum-digits a) k) 0) a
      (min-sum-digit (+ a 1) b k))))

;(min-sum-digit 10 35 6)
;(min-sum-digit 10 15 20)

;TASK 2 - TEMA 1

(define (average f g)
  (lambda (x) (/ (+ (f x) (g x)) 2)))

(define (calcprod f n)
  (accumulate-i
   (lambda (acc x) (lambda (num) (* (acc num) (x num)))) 
   (lambda (num) 1) 1 n
   (lambda (i)
     (average f (lambda (el) (expt i el))))
   1+))


;((average 1+ (lambda (x) (expt 3 x))) 2) ;-> 6
;((calcprod 1+ 3) 1) ;-> 7.5
;(2 + 1) / 2 * (2 + 2) / 2 * (2 + 3) / 2

;((calcprod 1+ 3) 2) ;-> 42
;(3 + 1) / 2 * (3 + 4) / 2 * (3 + 9) / 2

;((calcprod 1+ 3) 3) ;-> 232.5
;(4 + 1) / 2 * (4 + 8) / 2 * (4 + 27) / 2

;TASK 3 - TEMA 1

(define (occurrences l1 l2)
  (foldr
   (lambda (x acc)
     (cons
      (foldl
       (lambda (acc1 x1)
         (if (equal? x1 x)
             (1+ acc1)
             acc1))
       0 l2) acc))
     '() l1))

;(occurrences '(1 2 3) '( 1 2 4 1 )) ;-> (2 1 0)
;(occurrences '(1 2 3) '( 1 2 2 1 3 3 2 3 3)) ;-> (2 3 4)
;(occurrences '() '( 1 2 2 1 3 3 2 3 3)) ;-> ()
;(occurrences '(1 2 3 '()) '('())) ;-> (0 0 0 1)

;TASK 4 - TEMA 1

(define (match-lengths? l3 l4)
  (define length-dif (- (length (car l3)) (length (car l4))))

  (define (match-lengths?-helper l1 l2)
    (cond
    ((not (= (length l1) (length l2))) #f)
    ((null? l1) #t)
    (else (and (= (- (length (car l1)) (length (car l2))) length-dif) (match-lengths?-helper (cdr l1) (cdr l2))))))

  (match-lengths?-helper l3 l4))


;(match-lengths? '( () (1 2) (3 4 5)) '( (1) (2 3 4) (5 6 7 8))) ;-> #t,

;(match-lengths? '( () (1 2) (3 4 5)) '( () (2 3 4) (5 6 7))) ;-> #f

;(match-lengths? '( () (1 2) (3 4 5)) '( (1) (2 3 4) (5 6 7))) ;-> #f

;(match-lengths? '( () (1 2) (3 4 5)) '( (1) (2 3) (5 6 7 8))) ;-> #f

;(match-lengths? '( () (1 2) (3 4 5)) '( () (1 2) (3 4 5))) ;-> #t

;TASK 1 - TEMA 2

(define (prod-sum-div a b k)
  (define (sum-denominators x)
    (accumulate-i
     (lambda (acc y)
       (if (= (remainder x y) 0)
           (+ acc y)
           acc))
     0 1 x id 1+))

  (accumulate-i
   (lambda (acc x)
     (if (= (remainder (sum-denominators x) k) 0)
         (* acc x)
         acc))
     1 a b id 1+))

;(prod-sum-div 1 10 2) ;->3*5*6*7*10 = 6300

;TASK 2 - TEMA 2

(define (average-new f g)
  (lambda (x)
    (sqrt (* (f x) (g x)))))

(define (calcsum f n)
  (accumulate-i
   (lambda (acc x)
     (lambda (el) (+ (acc el) (x el))))
   (lambda (x) 0) 1 n
   (lambda (i)
     (average-new f (lambda (x) (expt x i)))) 1+))

;((calcsum 1+ 2) 1) ;-> 2*sqrt(2)
;sqrt(2) + sqrt(2)

;((calcsum 1+ 2) 2) ;-> sqrt(6) + sqrt(12)
;sqrt(3*2) + sqrt(3*4)

;TASK 3 - TEMA 2

(define (duplicates l1 l2)
  (foldr
   (lambda (x acc)
     (if (< 1
            (foldl
             (lambda (acc1 x1)
               (if (equal? x1 x)
                   (1+ acc1)
                   acc1))
             0 l2))
         (cons x acc)
         acc))
   '() l1))

;(duplicates '(1 2 3) '(1 2 1 3 2)) ;-> (1 2)
;(duplicates '(1 4 3 2) '(4 1 2 1 3 2 3)) ;-> (1 3 2)
;(duplicates '() '(4 1 2 1 3 2 3)) ;-> ()
;(duplicates '(1 2 3) '(1 2 3)) ;-> ()
;(duplicates '(1 2 3) '()) ;-> ()
;(duplicates '(1 () 2 3 5) '(1 2 5 () 6 () 1 2)) ;-> (1 () 2)

;TASK 4 - TEMA 2

(define (image? l1 l2)
  (define diff (- (car l1) (car l2)))

  (define (image?-helper l3 l4)
    (cond
    ((not (= (length l3) (length l4))) #f)
    ((null? l3) #t)
    (else (and (= diff (- (car l3) (car l4))) (image?-helper (cdr l3) (cdr l4))))))

  (image?-helper l1 l2))

;(image? '(1 2 3) '(4 5 6)) ;-> #t
;(image? '(1 2 3) '(1 2 2 )) ;-> #f
;(image? '(1 2) '(1 2 2 )) ;-> #f
;(image? '(1 2 3) '(0 2 3)) ;-> #f
;(image? '(1 2 3) '(1 1 3)) ;-> #f