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

;TASK 1

(define (prime? n)
  (accumulate-i
   (lambda (acc x)
     (if (= (remainder n x) 0)
         #f
         acc))
   #t 2 (quotient n 2) id 1+))

(define (check-prime-divisor? num x)
  (and (= (remainder num x) 0) (prime? x)))

(define (grow n)
  (accumulate-i
   (lambda (acc x)
     (if (check-prime-divisor? n x)
         (* acc x)
         acc))
   n 2 (quotient n 2) id 1+))

(grow 20) ;-> 200
(grow 15) ;-> 15*3*5 = 225
(grow 0) ;->0
(grow 1) ;->1
(grow 2) ;->2

;TASK 2

(define (member? x lst)
  (foldl
   (lambda (acc el)
     (if (equal? x el)
         #t
         acc))
   #f lst))

;(member? 1 '(1 2 3)) ;->#t
;(member? '() '(1 () 3)) ;->#t
;(member? 0 '(1 2 3)) ;->#f

(define (maxUnitary n)
  (define (getPrimeDivisors num)
    (accumulate-i
     (lambda (acc x)
       (if (check-prime-divisor? num x)
           (cons x acc)
           acc))
     '() 2 (quotient n 2) id 1+))
  
  (define (isUnitary? num k)
    (define primeDivisorsK (getPrimeDivisors k))
    (define primeDivisorsDivRes (getPrimeDivisors (/ num k)))

    (foldl
     (lambda (acc x)
       (if (member? x primeDivisorsK)
           #f
           acc))
     #t primeDivisorsDivRes) )
  
  (accumulate-i
   (lambda (acc x)
     (if (and (= (remainder n x) 0) (isUnitary? n x) (< x n) (> x acc))
         x
         acc))
   -1 2 (quotient n 2) id 1+))

(maxUnitary 60) ;->20

(maxUnitary 50) ;->25 

;TASK 3

(define (selectiveMap f a b)
  (define (apply-lastRule ai bi lastRule)
    (if (= lastRule 1) (f ai) (f bi)))
  
  (define (selectiveMap-helper f aEls bEls lastRule)    
    (if (null? aEls)
        '()
        (let ((ai (car aEls)) (bi (car bEls)))
          (cond
          ((and (< (f ai) (f bi)) (< (f bi) (min ai bi))) (cons (f ai) (selectiveMap-helper f (cdr aEls) (cdr bEls) 1)))
          ((and (> (f bi) (f ai)) (> (f ai) (max ai bi))) (cons (f bi) (selectiveMap-helper f (cdr aEls) (cdr bEls) 2)))
          (else (cons (apply-lastRule ai bi lastRule) (selectiveMap-helper f (cdr aEls) (cdr bEls) lastRule)))))))

  (cons (apply-lastRule (car a) (car b) 1) (selectiveMap-helper f (cdr a) (cdr b) 1)))

(selectiveMap (lambda (x) (- (* x x) 2)) '(2 -1 -2 -1 4 0 1 -4) '(10 2 -3 2 -1 1 3 5)) ;-> (2 -1 7 2 -1 -2 -1 23)

;(length '(1 2 3))

;TASK 4

(define (connection-busses network phone)
  (foldr
   (lambda (phoneX acc)
     (if (member? phoneX network)
         (cons phoneX acc)
         acc))
   '() phone))

(define (preferredDevice network phones)
  (define networkBusCount (length network))
  
  (cdr (foldr
   (lambda (phone acc)
     (let* ((shared-busses (connection-busses network phone))
           (shared-busses-count (length shared-busses))
           (connection-quality (/ shared-busses-count networkBusCount)))
       (if (null? shared-busses)
           acc
           (if (< (car acc) connection-quality)
               (cons connection-quality shared-busses)
               acc))))
   (cons -1 '()) phones)))

(preferredDevice '(2 4 5 17 30) '((1 3 5 7 9 20) (1 2 3 4 5 7 12 14 30) (2 4 17))) ;-> (2 4 5 30)
(preferredDevice '(2 4 5 17 30) '((1 13 12 7 9 20) (1 2 3 4 5 7 12 14 30) (2 4 17) (2 4 17 30 28 5 60))) ;-> (2 4 17 30 5)

;TASK 4 BONUS - NOT DONE - THROWS ERROR

(define (getAllConnected networks phone)
  (foldr
   (lambda (network acc)
     (let* (
            (shared-busses (connection-busses network phone)))
       (if (not (null? shared-busses))
           (cons network acc)
           acc)))
   '() networks))

;(length (cdr '((list) (list))))
;(length '((1 2 3 4 5 7 12 14 30) ((2 4 5 17 30) (14 2 4))))

(define (getPreferredDevicesBySharedBusses networks phones)
  (foldr
   (lambda (phone acc)
     (let* ((allConnected (getAllConnected networks phone))
            (isConnected (not (null? allConnected)))
            (connectedCount (length allConnected))
            (currentMaxConnectedCount (if (not acc) 0 (length (car (cdr acc))))))
       (cond 
         ((and isConnected (= connectedCount currentMaxConnectedCount)) (cons (list (cons phone (list allConnected))) (list acc)))
         ((and isConnected (> connectedCount currentMaxConnectedCount)) (cons phone (list allConnected)))
         (else acc))))
   #f phones))

(define (getTotalPhoneServiceQuality phone networksConnected)
    (apply + (map
              (lambda (networkConnected)
                (let* ((shared-busses (connection-busses networkConnected phone))
                       (shared-busses-count (length shared-busses))
                       (networkBusCount (length networkConnected)))
                  (/ shared-busses-count networkBusCount))) networksConnected)))

(define (getPreferredDevicesByConnectionQuality phoneNetworkPairs)
    (foldr
     (lambda (phoneNetworkPair acc)
       (let* ((phone (car phoneNetworkPair))
              (networksConnected (cdr phoneNetworkPair))
              (totalPhoneServiceQuality (getTotalPhoneServiceQuality networksConnected phone)))
         (if (or (not acc) (< (getTotalPhoneServiceQuality (cdr acc) (car acc)) totalPhoneServiceQuality))
             (cons phone (list networksConnected))
             acc)))
     #f phoneNetworkPairs))

(define (preferredDeviceForAll networks phones)

  (let* ((bestPhoneNetworksPair (getPreferredDevicesByConnectionQuality (car (getPreferredDevicesBySharedBusses networks phones)))))
  (connection-busses (car (cdr bestPhoneNetworksPair)) (car bestPhoneNetworksPair))))

;(list '() '())
;(cons '(1 2) (list '((1 2 3) (3 2 1))))
;(cons '((1 2) ((1 2 3) (3 2 1))) (list '((4 5) ((4 5 6) (6 5 4)))))  

;(getPreferredDevicesBySharedBusses '((2 4 5 17 30) (14 2 4)) '((1 13 12 7 9 20) (2 4 17) (2 4 17 30 28 5 60) (1 2 3 4 5 7 12 14 30)))
;(preferredDeviceForAll '((2 4 5 17 30) (14 2 4)) '((1 13 12 7 9 20) (2 4 17) (2 4 17 30 28 5 60) (1 2 3 4 5 7 12 14 30))) ;-> (2 4 5 30)


  

  