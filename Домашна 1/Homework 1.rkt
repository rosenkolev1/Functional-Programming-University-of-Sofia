(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))
(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

;Task 1
(define (id x) x)
(define (1+ x) (+ x 1))

(define (argmin f a b)
  (accumulate-i
   (lambda (x y)
     (if (< (f y) (f x))
         y
         x))
   a
   a
   b
   id
   1+))

;(define (mod7 x) (modulo x 7))
;(argmin mod7 45 50);-> 49
;(argmin mod7 50 56)
;(argmin (lambda (x) (- x 5)) 55 60);-> 55

;Task 2
(define (best-pair a b)
  (define (countOfDivisors x)
    (accumulate-i
     (lambda (acc y)
       (if (= (remainder x y) 0)
           (+ acc 1)
           acc))
     0
     1
     x
     id
     1+))
  
  (define (getSumOfPair pair) (+ (car pair) (cdr pair)))
  
  (accumulate-i
   (lambda (acc y)
     (if (< (countOfDivisors (getSumOfPair acc)) (countOfDivisors y))
         (if (odd? y)
             (cons (quotient y 2) (1+ (quotient y 2)))
             (cons (- (/ y 2) 1) (1+ (/ y 2))))                  
         acc))
   (cons a (1+ a))
   (+ a a 1)
   (+ b b -1)
   id
   1+))

(best-pair 16 20) ;-> (16 . 20) ; или (17 . 19), но не и (18 . 18) :)

;TASK 3

