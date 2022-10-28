;#lang racket

; Наредени двойки:
;-----------------
; cons създава наредена 2-ка
;(cons 1 #\a) ; (1 . #\a)
; Тук точката индикира че това е наредена 2-ка

;(define some-pair (cons "first" "second"))

; Функции с които достъпваме елементите на наредена двойка:

; Първия елемент (Contents of the Address Register)
;(car some-pair)   ; "hello"

; Втория елемент (Contents of the Data Register)
;(cdr some-pair)   ; "world"


; Има съкратен синтаксис за композиция на car и cdr:
; caar - първия елемент на първия елемент
; cdar - втория елемент на първия елемент
; и т.н. до общо 4 срещания на "a" и "d"

;(define deep-list
;  (list (list 1 2)
;        (list (list 3 4) 5)))

; Може да ги четете отзад напред за по-лесно
;(caar deep-list)  ; 1
;(cdar deep-list)  ; (2)


; Списъци:
;---------
; 1. '() е списък
; 2. (head . tail) е списък <=> tail е списък

;(list 1 2 3 4) ; построява списък от подадените аргументи

; Ясно се вижда че списъците са наредени 2ки,
; но наредените 2ки не са списъци.
;'(1 . (2 . (3 . (4 . 5))))  ; '(1 2 3 4 . 5)
;'(1 . (2 . (3 . (4 . ())))) ; '(1 2 3 4)

;(pair? '()) ; #f
;(list? '()) ; #t
;(null? '()) ; #t


; Списък може да се конструира и с quote
;(quote (1 2 3 4))

; Но обикновено се използва съкратен синтаксис:
;'(1 2 3 4)

; Каква е разликата дали ползваме list или quote?

; quote е специална форма и не оценява аргументите си,
; а се прилага "рекурсивно" върху тях
;'(1 2 3)
; е същото като
;(list '1 '2 '3)

; съответно ако има какво да се оцени
;'(+ 1 2)  ; тук + е само символ
          ; (immutable string)

;(list + 1 2)  ; тук + е процедура
; или ако на мястото на някой елемент сложим променлива
; quote ще вземе буквално името й като символ, а list
; ще я оцени

; Сравнения:
;-----------
; (= a b) работи само за числа

; (eq? a b) проверява дали a и b са един и същ обект
; (на едно и също място в паметта)

; (eqv? a b) е като eq? с изключение за някои типове
; на аргументите - като char и integer

; (equal? a b) e като eqv? с изключение за много типове.
; Работи за списъци и може да сравнява покомпонентно.


;===============;=========================================
;; З А Д А Ч И ;;
;;;;;;;;;;;;;;;;;

; NOTE: Суфикса * е защото съществуват вградени процедури
;       със същите имена в racket или r5rs

; 0. Намира дължина на списък
(define (length* lst)
  (if (null? lst)
      0
      (+ 1 (length* (cdr lst)))))

;(length* (list 1 2 3))
;(length* (list 1))
;(length* (list))

; 1. Връща списък от първите n елемента
(define (take* n lst)
  (define (take*-helper oldLst n)
   (if (or (null? oldLst) (<= n 0))
      (list)
      (cons (car oldLst) (take*-helper (cdr oldLst) (- n 1)))))
  (take*-helper lst n))

;(take* 0 (list 1 2 3))
;(take* 1 (list 1 2 3))
;(take* 2 (list 1 2 3))
;(take* 3 (list 1 2 3))
;(take* 4 (list 1 2 3))
;(take* 2 (list))

; 2. Връща списък като lst, но без първите n елемента
(define (drop* n lst)
  (if (null? lst)
      (list)
      (if (> n 0)
          (drop* (- n 1) (cdr lst))
          lst)))

;(drop* -1 (list 1 2 3))
;(drop* 0 (list 1 2 3))
;(drop* 1 (list 1 2 3))
;(drop* 2 (list 1 2 3))
;(drop* 3 (list 1 2 3))
;(drop* 4 (list 1 2 3))


; 3. Генерира списък от целите числа в интервала [a,b]
(define (from-to a b)
  (if (> a b)
      (list)
      (if (= a b)
      (cons a (list))
      (cons a (from-to (+ a 1) b)))))

;(from-to 5 10)
;(from-to 2 2)
;(from-to 3 2)

; 4. По даден списък от числа - намира сумата им.
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

;(sum (list 5 10 15))
;(sum (list 2 2))
;(sum (list 3 2))
;(sum (list))

; 5. Връща последния елемент на списъка lst.
(define (last* lst)
  (if (null? lst)
      'null
      (if (null? (cdr lst)) 
      (car lst)
      (last* (cdr lst)))))

;(last* (list 5 10 15))
;(last* (list 2 7))
;(last* (list 2 3))
;(last* (list))

; 6. Връща n-тия елемент на списъка lst.
(define (nth n lst)
  (if (or (null? lst) (< n 0))
      'null
      (if (= n 1)
          (car lst)
          (nth (- n 1) (cdr lst)))))

;(nth 1 (list 5 10 15))
;(nth 2 (list 5 10 15))
;(nth 3 (list 5 10 15))
;(nth 4 (list 5 10 15))
;(nth -1 (list 5 10 15))
;(nth 0 (list))

; 7. Залепя l1 и l2 в списък от наредени 2ки.
; Може списъците да са с различна дължина.
; Пример: (zip '(1 2 3) '(4 5)) -> '((1 . 4) (2 . 5))
(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      (list)
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

;(zip (list 1 2 3) (list 4 5))
;(zip (list 10 11) (list 20 21))
;(zip (list 10 11 12) (list 20 21 22))
;(zip (list 3 4) (list))
;(zip (list) (list 3 4))
;(zip (list) (list))

; 8. Връща конкатенацията на lst1 и lst2.
; Реализирайте с рекурсия
(define (append* lst1 lst2)
  (if (null? lst1)
      (if (null? lst2)
          (list)
          (cons (car lst2) (append* lst1 (cdr lst2))))
      (cons (car lst1) (append* (cdr lst1) lst2))))

;(append* (list 1 2) (list 10 13 14))
;(append* (list) (list 10 13 14))
;(append* (list 10 13 14) (list))
;(append* (list) (list))

; 9. Връща lst след прилагане на f върху всеки елемент.
(define (map* f lst)
  (if (null? lst)
      (list)
      (cons (f (car lst)) (map f (cdr lst)))))

;(map* (lambda (x) (+ x 1)) (list 10 13 14))
;(map* (lambda (x) (+ x 1)) (list 10))
;(map* (lambda (x) (+ x 1)) (list))

; 10. Връща списък от елементите на lst,
; за които предиката p е верен
(define (filter* p lst)
  (if (null? lst)
      (list)
      (if (p (car lst))
          (cons (car lst) (filter* p (cdr lst)))
          (filter* p (cdr lst)))))

;(filter* (lambda (x) (> x 5)) (list 1 5 6))
;(filter* (lambda (x) (> x 5)) (list 1 5 5))
;(filter* (lambda (x) (> x 5)) (list 1))
;(filter* (lambda (x) (> x 5)) (list))
;(filter* (lambda (x) (> x 5)) (list 1 10 2 9 3 8 4 7 5 6))

; 11. Като функцията accumulate, но за списъци
; Пример: (foldl* - 0 '(1 2 3 4)) -> -10
(define (foldl* op acc lst)
  (if (null? lst)
      acc
      (foldl* op (op acc (car lst)) (cdr lst))))

;(foldl* - 0 (list 1 2 3 4)); -> -10
;(foldl* - 0 (list 1))
;(foldl* - 0 (list))
;(foldl* + 5 (list 1 2 3 4))
