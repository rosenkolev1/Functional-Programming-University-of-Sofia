;#lang racket

; THROWBACK:
;-----------
; - рекурсия и итерация
; - функциите в които всички рекурсивни извиквания са
;   опашкови, генерират итеративен рекурсивен процес
; - не всички функции могат да бъдат изчислени с итерация

; Видяхме че можем да пишем итеративни функции, като
; пазим резултата в допълнителна променлива.
;(define (interval-sum-i a b)
;  (define (iter i j acc)
;    (if (= i j)
;      (+ acc i)
;      (iter (+ i 1)
;            j
;            (+ acc i))))
;    (iter a b 0))

; Какво ако искаме да умножим числата в интервала?

; Можем да се абстрахираме от операцията и да напишем
; функция, която може да използва произволна операция
; за горното изчисление.

; Обаче ако искаме да умножаваме числата,
; освен замяната на (+) с (*),
; ще трябва и да извикаме (iter a b 1),
; защото неутралния елемент спрямо умножението е 1, а не 0.

; Затова просто ще напишем итеративна функция без вложена
; дефиниция, и ще оставим потребителя да подаде начална
; стойност.

; В замяна получаваме по-обща функция, която можем да
; ползваме за повече неща.

(define (interval-op from to op acc)
  (if (> from to)
    acc
    (interval-op
      (+ from 1)
      to
      op
      (op acc from))))


;===============;=========================================
;; З А Д А Ч И ;;
;;;;;;;;;;;;;;;;;

; Идентитета не е част от racket и R5RS, но ще ви потрябва
(define (id x) x)

; 1. Да се напише итеративна функция, която за дадени:
; - [from, to] - интервал от числа
; - term - едноаргументна функция над цели числа
; - op - бинарна операция над acc и term(x)
; - acc - начална стойност
; Пресмята натрупаната стойност в acc, получена чрез
; обхождане на интервала [from, to] с операция op
; над стойностите получени чрез term(x)
(define (accumulate from to term op acc)
  (if (> from to)
      acc
      (accumulate (+ 1 from) to term op (op acc (term from)))))

;(accumulate 1 10 id (lambda (a b) (+ a b)) 0)

; Реализирайте следните функции чрез accumulate:
;-----------------------------------------------
; 2. Факториел.
(define (fact n) (accumulate 1 n id (lambda (a b) (* a b)) 1))

;(fact 4)

; 3. Проверява дали даден предикат е верен за всички числа
; в даден интервал.
; Hint: вместо да ползвате директно and и or -
; (define (and2 x y) (and x y))
(define (for-all? from to p?)
  (accumulate from to p? (lambda (x y) (and x y)) #t))

;(for-all? 1 5 (lambda (a) (< a 10)))
;(for-all? 1 5 (lambda (a) (< a 5)))
;(for-all? 1 5 (lambda (a) (<= a 5)))

; 4. Проверява дали някое число в даден интервал
; изпълнява даден предикат.
(define (exists? from to p?)
  (accumulate from to p? (lambda (x y) (or y x)) #f))

;(exists? 1 5 (lambda (a) (< a 10)))
;(exists? 1 5 (lambda (a) (> a 5)))
;(exists? 1 5 (lambda (a) (>= a 5)))

; 5. Намира броя на целите числа в интервал,
; които изпълняват даден предикат.
(define (count-p from to p?)
  (accumulate from to
              (lambda (a)
                (if (p? a) 1 0))
              (lambda (x y) (+ x y)) 0))

;(count-p 1 5 (lambda (a) (< a 10))) ;5
;(count-p 1 5 (lambda (a) (> a 5))) ;0
;(count-p 1 5 (lambda (a) (>= a 5))) ;1

; 6. Проверява дали в целочисления интервал [a,b]
; съществуват две различни цели числа x и y, такива че:
; f(x) = g(x) и f(y) = g(y)
(define (meet-twice? f g a b)
  (>= (accumulate a b
              (lambda (arg)
                (if (= (f arg) (g arg))
                    1
                    0))
              (lambda (x y) (+ x y)) 0) 2))

; Примери:
;(meet-twice? id (lambda (x) (- x)) -3 1) ;-> #f
;(meet-twice? id sqrt 0 5) ;-> #t (за 0 и 1)

; 7. Обръща записа на дадено естествено число
; Hint: (count-digits n) <=> (+ 1 (floor (log n 10)))
(define (reverse-digits n)
  (define digitCount
    (if (not(= n 0))
        (+ 1 (floor (log n 10)))
        1))
  (define (getDigit num pos currentPos)
    (if (= currentPos pos)
        (remainder num 10)
        (getDigit (quotient num 10) pos (+ currentPos 1))))
  (accumulate 1 digitCount
              id
              (lambda (ac arg) (+ ac (* (getDigit n arg 1) (expt 10 (- digitCount arg))))) 0))

;(reverse-digits 1)
;(reverse-digits 123)
;(reverse-digits 123987)
;(reverse-digits 0)

; 8. Намира броя на палиндромите в интервала [a,b]
(define (count-palindromes a b)
  (accumulate a b
              (lambda (arg)
                (if (= (reverse-digits arg) arg)
                    1
                    0))
              (lambda (ac arg) (+ ac arg)) 0))

;(count-palindromes 1 10) ;9
;(count-palindromes 1 11) ;10
;(count-palindromes 90 98) ;0
;(count-palindromes 998 1001) ;2

; 9. Намира средната цифра на записа на дадено число.
; Ако n има четен брой цифри, функцията връща -1.
(define (middle-digit n)
  (let* ((digitCount
    (if (not(= n 0))
        (+ 1 (floor (log n 10)))
        1))
    (middlePos (+ (floor (/ digitCount 2)) 1)))
    (if (even? digitCount)
      -1
      (accumulate 1 middlePos
                  id
                  (lambda (ac arg)
                    (cond
                      ((< arg middlePos) (quotient ac 10))
                      ((= arg middlePos) (remainder ac 10))
                      (else ac))) n))))

; Примери:
(middle-digit 452) ;-> 5
(middle-digit 45267) ;-> 2
(middle-digit 4712) ;-> -1