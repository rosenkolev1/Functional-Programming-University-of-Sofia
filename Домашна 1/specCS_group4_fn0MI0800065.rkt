#lang racket

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

(define (mod7 x) (modulo x 7))
(argmin mod7 45 50);-> 49
(argmin mod7 50 56); -> 56
(argmin (lambda (x) (- x 5)) 55 60);-> 55

;Task 2
(define (best-pair a b)
  (define (countOfDivisors x)
    (accumulate-i
     (lambda (acc y)
       (if (= (remainder x y) 0)
           (+ acc 1)
           acc))
     0 1 x id 1+))
  
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
(best-pair 10 16) ;-> (11 . 13)

;TASK 3
(define (integrate2 f a b c d dx dy)
  (accumulate-i
   (lambda (acc y)
     (+ acc (accumulate-i
             (lambda (acc2 x)
               (+ acc2 x))
             0 a b
             (lambda (x)
                     (* (f x y) dx dy))
             (lambda (x) (+ x dx)))))
   0 c d
   id
   (lambda (y) (+ y dx))))

;(lambda (y) (* y dy))

(define pi 3.14159265359)
(define (someFunc x y) (+ x (sin y) 1))
(let ((res (integrate2 someFunc 0 2 (- pi) pi 0.01 0.01)))
  (/ res pi)) ;-> приблизително 8; забележете по-високите стойности за dx и dy

;TASK 4
(define (n-rooks taken n)
  (define allRowsHaveOneFig
    (accumulate-i
   (lambda (acc x)
     (and acc
          (= 1
             (accumulate-i
               (lambda (acc2 cell)
                 (if (member cell taken)
                   (+ acc2 1)
                   acc2))
               0 0 (- n 1)
               (lambda (y) (cons x y))
               (lambda (y) (+ y 1))))))
   #t 0 (- n 1) id (lambda (x) (+ x 1))))
  
  (define allColsHaveOneFig
    (accumulate-i
   (lambda (acc x)
     (and acc
          (= 1
             (accumulate-i
               (lambda (acc2 cell)
                 (if (member cell taken)
                   (+ acc2 1)
                   acc2))
               0 0 (- n 1)
               (lambda (y) (cons y x))
               (lambda (y) (+ y 1))))))
   #t 0 (- n 1) id (lambda (x) (+ x 1))))
  (if (< (length taken) n) #f
      (and allRowsHaveOneFig allColsHaveOneFig)))


(define board1 (list (cons 0 0) (cons 1 1) (cons 2 2) (cons 3 3) (cons 4 4)))
(define board2 (list (cons 0 0) (cons 1 1) (cons 1 2) (cons 3 3) (cons 4 4)))
(define board3 (list (cons 0 0) (cons 1 1) (cons 2 1) (cons 3 3) (cons 4 4)))
(define board4 (list (cons 0 2) (cons 1 3) (cons 2 4) (cons 3 0) (cons 4 1)))
(define board5 (list (cons 0 0) (cons 1 1) (cons 2 2) (cons 3 3)))

(n-rooks board1 5); -> #t
(n-rooks board2 5); -> #f
(n-rooks board3 5); -> #f
(n-rooks board4 5); -> #t
(n-rooks board5 5); -> #f