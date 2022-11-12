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

;TASK 1 - VARIANT A

(define (product-digits x)
  (define (product-digits-helper x)
    (if (= x 0)
      1
      (* (remainder x 10) (product-digits-helper (quotient x 10)))))

  (if (= x 0) 0
      (product-digits-helper x)))

;(product-digits 5)
;(product-digits 25)
;(product-digits 0)
;(product-digits 1)

(define (largest-diff a b)
  (define (product-digits-diff x)
    (- x (product-digits x)))
  
  (accumulate-i
   (lambda (acc x)
     (if (< acc x)
         x
         acc))
   -1 a b
   (lambda (x)
     (accumulate-i
             (lambda (acc1 x1)
               (if (< acc1 x1)
                   x1
                   acc1))
             -1 x b
             (lambda (x1)
               (abs (- (product-digits-diff x) (product-digits-diff x1))))
             1+))
   1+))

;(largest-diff 28 35) ;→ 19  = {30} – {29} = (30 – 0) – (29 – 18))

;TASK 2 - VARIANT A

(define (max-metric ml ll)
  (cdr (foldl
   (lambda (acc m)
     (let ((m-value (apply + (map m ll))))
       (if (< (car acc) m-value)
         (cons m-value m)
         acc)))
   (cons (apply + (map (car ml) ll)) (car ml))
   ml)))


(define (prod l) (apply * l))        (define (sum l) (apply + l)) 

;(max-metric (list sum prod) '((0 1 2) (3 4 5) (1337 0))) ;→ <sum>
;(max-metric (list car sum)  '((1000 -1000) (29 1) (42))) ;→ <car>

;TASK 3 - VARIANT A

(define (deep-repeat lst)
  (define atom?
    (lambda (x)
      (and (not (pair? x)) (not (null? x)))))
  
  (define (append lst1 lst2)
    (if (null? lst1)
        (if (null? lst2)
            '()
            (cons (car lst2) (append lst1 (cdr lst2))))
        (cons (car lst1) (append (cdr lst1) lst2))))
  
  (define (repeat-atom atom level)
    (if (< level 1)
        '()
        (cons atom (repeat-atom atom (- level 1)))))
  
  (define (deep-repeat-helper lst newLst level)
    (if (null? lst) newLst
        (if (atom? (car lst))
            (deep-repeat-helper (cdr lst) (append newLst (repeat-atom (car lst) level) ) level)
            (append (append newLst (list (deep-repeat-helper (car lst) '() (+ 1 level)))) (deep-repeat-helper (cdr lst) '() level)))))

  (deep-repeat-helper lst '() 1))
    
;(deep-repeat '(1 2)) ;-> (1 2)
;(deep-repeat '(1 (2))) ;-> (1 (2 2))
;(deep-repeat '(1 (2 3) 4 (5 (6)))) ;→ (1 (2 2 3 3) 4 (5 5 (6 6 6)))

;TASK 1 - VARIANT B

(define (sum-digit-divisors x)
  (define originalNum x)

  (define (sum-digit-divisors-helper x1)
   (if (= x1 0)
      0
      (if (and (not (= (remainder x1 10) 0)) (= (remainder originalNum (remainder x1 10)) 0))
          (+ (remainder x1 10) (sum-digit-divisors-helper (quotient x1 10)))
          (sum-digit-divisors-helper (quotient x1 10)))))

  (sum-digit-divisors-helper x))

;(sum-digit-divisors 1234) ;->3
;(sum-digit-divisors 36) ;->9
;(sum-digit-divisors 10) ;->1
;(sum-digit-divisors 0) ;->1
;(sum-digit-divisors 11) ;->2

(define (same-sum a b)
  (accumulate-i
   (lambda (acc x)
      (+ acc (accumulate-i
       (lambda (acc1 x1)
         (if (= (sum-digit-divisors x) (sum-digit-divisors x1))
             (1+ acc1)
             acc1))
       0 (+ x 1) b id 1+)))
   0 a b id 1+))

;(same-sum 28 35) ;→ 2
;(same-sum 0 10) ;→ 1

;TASK 2 - VARIANT B

(define (best-metric? ml ll)
  (define (bigger-consecutive? list1 list2)
    (if (null? list1) #t
        (and (> (car list1) (car list2)) (bigger-consecutive? (cdr list1) (cdr list2)))))
  
  (foldr
   (lambda (m acc)
     (if
      (foldl
       (lambda (acc1 x)
         (if (not (equal? x m))
            (if (bigger-consecutive? (map m ll) (map x ll))
                (and acc1 #t)
                #f)
            acc1))
       #t ml)
         #t
         acc))
   #f ml))

;(define (prod l) (apply * l))        (define (sum l) (apply + l)) 
;(best-metric? (list sum prod) '((0 1 2) (3 -4 5) (1337 0))) ;→ #t
;(best-metric? (list car sum)  '((100 -100) (29 1) (42)))    ;→ #f

;TASK 3 - VARIANT B

(define (deep-delete lst)
  (define (atom? x)
    (and (not (pair? x)) (not (null? x))))

  (define (append lst1 lst2)
    (if (null? lst1)
        (if (null? lst2)
            '()
            (cons (car lst2) (append lst1 (cdr lst2))))
        (cons (car lst1) (append (cdr lst1) lst2))))

  (define (deep-delete-helper lst1 newLst level)
    (if (null? lst1) newLst
        (if (atom? (car lst1))
            (if (not (number? (car lst1)))
                (deep-delete-helper (cdr lst1) (append newLst (list (car lst1)) level))
                (if (< (car lst1) level)
                    (deep-delete-helper (cdr lst1) newLst level)
                    (deep-delete-helper (cdr lst1) (append newLst (list (car lst1))) level)))
            (append (append newLst (list (deep-delete-helper (car lst1) '() (1+ level)))) (deep-delete-helper (cdr lst1) '() (1+ level))))))

  (deep-delete-helper lst '() 1))


;(deep-delete '(1 (1 (3 4) 1))) ;→ (1 ((3 4))
;(deep-delete '(1 (2 (2 4) 1) 0 (3 (1)))) ;→ (1 (2 (4)) (3 ()))
