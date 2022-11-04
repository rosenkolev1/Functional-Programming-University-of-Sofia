; 1. Намира броя на цифрите на дадено естествено число n.
; Реализирайте я рекурсивно.
(define (count-digits n)
  (if (and (< n 10) (> n -1))
      1
      (+ 1 (count-digits (quotient n 10)))))


;(count-digits 2)
;(count-digits 34)
;(count-digits 567)
;(count-digits 0)

; 2. За дадени цяло число x и естествено число n връща x^n.
; Реализирайте я рекурсивно.
(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

;(pow 10 3)
;(pow 7 3)
;(pow 0 3)
;(pow 3 1)
;(pow 8 0)

; 3. За дадени числа a и b (a < b)
; намира сумата на целите числа в интервала [a,b]
; Реализирайте я рекурсивно.
(define (interval-sum a b)
  (if (> a b)
      0
      (+ a (interval-sum (+ a 1) b))))

;(interval-sum 1 10)
;(interval-sum 10 15)
;(interval-sum 1 1)
;(interval-sum 1 0)

; 4. За дадени цели числа x и n връща x^n.
; Реализирайте я чрез линейна рекурсия (итерация).
(define (pow x n)
  (define (pow-iter x n product)
    (if (= n 0)
        product
        (pow-iter x (- n 1) (* product x))))
  (pow-iter x n 1))

;(pow 10 3)
;(pow 7 3)
;(pow 0 3)
;(pow 3 1)
;(pow 8 0)

; 5. Намира броя на цифрите на дадено цяло число n.
; Реализирайте я чрез линейна рекурсия (итерация).

(define (count-digits n)
  (define (count-digits-iter n count)
    (cond ((and (= n 0) (= count 0)) 1)
          ((= n 0) count)
          (else (count-digits-iter (quotient n 10) (+ count 1)))))
  (count-digits-iter n 0))

;(count-digits 2)
;(count-digits 34)
;(count-digits 567)
;(count-digits 0)

; 6. За дадени цели числа a и b
; намира сумата на целите числа в интервала [a,b].
; Трябва да работи и за a > b.
; Реализирайте я чрез линейна рекурсия (итерация).
(define (interval-sum-i a b)
  (define (interval-sum-iter a b sum)
    (if (> a b)
        sum
        (interval-sum-iter (+ a 1) b (+ sum a))))
  (interval-sum-iter a b 0))

;(interval-sum 1 10)
;(interval-sum 10 15)
;(interval-sum 1 1)
;(interval-sum 1 0)

; 7. За дадено цяло число n връща число,
; чийто цифри са в обратен ред.
; Реализирайте го чрез линейна рекурсия (итерация).
(define (reverse-digits-i n)
  (define (reverse-digits-i-iter n revNum)
    (if (= n 0)
        (quotient revNum 10)
        (reverse-digits-i-iter (quotient n 10) (* (+ revNum (remainder n 10)) 10))))
  (reverse-digits-i-iter n 0))

;(reverse-digits-i 123)
;(reverse-digits-i 12)
;(reverse-digits-i 3567)
;(reverse-digits-i 1)
;(reverse-digits-i 0)

; 8. За дадени цели числа x и n връща x^n,
; но ако n е четно, то x^n = (x^(n/2))^2
; Реализирайте я чрез линейна рекурсия (итерация).
(define (fast-pow x n)
  (define (fast-pow-iter x n product)
    (cond ((= n 0) product)
      ((= (remainder n 2) 1) (fast-pow-iter x (- n 1) (* product x)))
      ((= (remainder n 2) 0) (fast-pow-iter (* x x) (/ n 2) product))
          ))
  (fast-pow-iter x n 1))

;(fast-pow 10 3)
;(fast-pow 7 3)
;(fast-pow 0 3)
;(fast-pow 3 1)
;(fast-pow 8 0)
;(fast-pow 10 4)
;(fast-pow 10 3)