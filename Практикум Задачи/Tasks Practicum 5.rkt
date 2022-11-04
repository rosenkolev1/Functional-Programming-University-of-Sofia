#lang racket

; Предния път работихме със списъци и реализирахме
; функцията accumulate

; В racket (и Haskell) има 2 различни но много близки
; помежду си имплементации:

;(foldr + 0 '(1 2 3 4)) ; 10
;(foldl + 0 '(1 2 3 4)) ; 10

; Каква е разликата тогава?

; foldr сгъва списъка от дясно наляво
;------------------------------------
; (+ 1 (+ 2 (+ 3 (+ 4 0))))
; foldr генерира рекурсивен процес и тези операции са
; отложени

; т.е. можете да се възползвате от това,
; ако искате операциите ви да се изпълнят в обратен ред

; foldl сгъва списъка от ляво надясно
;------------------------------------
; (+ 4 (+ 3 (+ 2 (+ 1 0))))
; foldl генерира итеративен процес и операциите се
; изчисляват в аргумента.

; NOTE: Нормалния foldl (както работи и в Haskell)
;       има по-различна имплементация:
;       (+ (+ (+ (+ 0 1) 2) 3) 4)
; Разликата е в реда на аргументите на операцията.

; В Racket:
;(foldl - 0 '(1 2 3 4)) ;-> 2

; В Haskell:
; (foldl - 0 '(1 2 3 4)) -> -10


; В scheme има функции, които могат да приемат произволен
; брой аргументи.
;----------------
; map на много аргументи
;(map + '(1 2 3) '(4 5 6)) ; '(5 7 9)
; списъците трябва да имат еднаква дължина

; apply прилага функция над списък от аргументи
;(apply + '(1 2 3 4 5)) ; 15
;(apply max '(1 2 3 11 4 5)) ; 11

; apply има вида: (apply proc v1 ... vn lst kw-arg ...)
; Засега не се интересуваме от kw-arg.
; Освен подадения списък, apply може да приема и
; допълнителни аргументи към подадената процедура proc.
; Това са онези v1 ... vn

; Тук 2 е допълнителен аргумент към *
;(apply * 2 '(1 2 3)) ; 12
; същото като:
;(* 2 1 2 3)

; Тук + е допълнителен аргумент към map:
;(apply map + '((1 2 3) (4 5 6))) ; '(5 7 9)
; същото като:
;(map + '(1 2 3) '(4 5 6))

; Ето и един пример с транспониране на матрица
(define (transpose m)
  (apply map list m))

; Разписваме transpose:
;
; (define m '((1 2 3)
;             (4 5 6)
;             (7 8 9)))
;
; (apply map list '((1 2 3) (4 5 6) (7 8 9)))
; <=>
; (map list '(1 2 3) '(4 5 6) '(7 8 9))
; <=>
; (list (list 1 4 7)
;       (list 2 5 8)
;       (list 3 6 9))

; Можем да правим функции на произволен брой аргументи
; (lambda (<args> . <opt-args>) <body>)
; Където <args> са задължителните параметри,
; а opt-args е списък с допълнителни (optional) аргументи.
;(lambda (x . lst) (apply + (cons x lst)))

; Вече знаем че define е синтактична захар за свързване
; на име с ламбда.
; Съответно дефинираме такива функции с define така:
;(define (sum x . l) (foldl + 0 (cons x l)))


;===============;=========================================
;; З А Д А Ч И ;;
;;;;;;;;;;;;;;;;;

;(map + '(2 3) '(3 4) '(7 8))

(define (foldl* op acc lst)
  (if (null? lst)
    acc
    (foldl* op (op (car lst) acc) (cdr lst))))

; 0. Като foldl но рекурсивно
; Пример: (foldr* - 0 '(1 2 3 4)) -> -2
(define (foldr* op acc lst)
  (if (null? lst)
      acc
      (op (car lst) (foldr* op acc (cdr lst)))))

;(foldr* + 0 '(1 2 3 4)) ;-> 10
;(foldr* - 0 '(1 2 3 4)) ;-> -2

; Използвайте foldl или foldr:
;-----------------------------
;(define foldr foldr*)
;(define foldl foldl*)


; 1. Намира дължина на списък
(define (length* lst)
  (foldr (lambda (x acc) (+ acc 1)) 0 lst))

;(length* '(1 2 3 4))

; 2. Премахва повторенията на елементи в lst
(define (uniques lst)
  (reverse(foldl
   (lambda (el acc)
     (if (not (member el acc))
         (cons el acc)
         acc)) '() lst)))

;(uniques '(1 2 3 2 4 5 1 7 4 3 0))
;(uniques '(1))
;(uniques '())

; 3. Проверява дали p? е верен за точно n елемента от lst
(define (sat-n? p? n lst)
  (= n (foldr
   (lambda (el acc)
     (if (p? el)
         (+ acc 1)
         acc))
         0 lst))
      )

;(sat-n? (lambda (x) (= x 5)) 3 '(1 2 3 4 5 5 5 6 7 8))
;(sat-n? (lambda (x) (= x 5)) 3 '(5 5 6 7 8))
;(sat-n? (lambda (x) (= x 5)) 2 '(5 5 6 7 8))
;(sat-n? (lambda (x) (= x 5)) 3 '(1))
;(sat-n? (lambda (x) (= x 5)) 3 '())

; 4. Връща списък с елементите на lst, но в обратен ред.
(define (reverse* lst)
  (foldl 
   (lambda (el acc)
     (cons el acc))
   '() lst))

;(reverse* '(1 2 3 4 5))
;(reverse* '())

; 5. Намира броя на елементите в дълбокия списък lst.
; Тоест lst може да има произволни нива на вложеност.
(define (count-atoms lst)
  (foldr
   (lambda (el acc)
     (if (list? el)
         (+ (count-atoms el) acc) 
         (if (not (eqv? el 'quote))
             (+ acc 1)
             acc)))
   0 lst))

;(count-atoms '())
;(count-atoms (list (list)))
;(count-atoms '('()))
;(count-atoms '(1 2 3))
;(count-atoms '('(1 2 3)))
;(count-atoms '('('(1 2 3))))
;(count-atoms '(1 2 3 '(1 2 3)))

; 6. Връща наредена двойка (fst . snd), където
; fst са елементите за които p? е истина
; и snd са тези за които p? е лъжа
(define (partition* p? lst)
  (list
   (foldr
    (lambda (el acc)
            (if (p? el)
                (cons el acc)
                acc))
    '() lst)
   (foldr
    (lambda (el acc)
            (if (not (p? el))
                (cons el acc)
                acc))
    '() lst)))

;(partition* (lambda (x) (< x 5)) '(1 2 3 4 5 6 7 8))
;(partition* (lambda (x) (< x 0)) '(1 2 3 4 5 6 7 8))

; Използвайте apply:
;-------------------
; 7. Средно аритметично на много аргументи
(define (avg h . t)
  (/ (apply + (cons h t)) (length (cons h t))))

;(avg 3 1 2 3)
;(avg 1)

; 8. Композиция на много едноаргументни функции

(define (1+ x) (+ x 1))

;(define (compose-all f . gs)
;  (if (null? gs)
;      f
;      (lambda (x)
;        (f ((apply compose gs) x)))))

;(define (compose f g)
;  (lambda (x) (f (g x))))

;(define (compose-all f . gs)
;  (if (null? gs)
;      f
;      (compose f (apply compose-all gs))))


;(compose-all 1+ 1+ 1+)
;((compose-all 1+ 1+ 1+) 10)
;((compose-all 1+ 1+ 1+ 1+) 10)

; 9. Конюнкция на много едноместни предикати
(define (conjoint-all p? . preds)
  (if (null? preds)
      p?
      (lambda (x)
        (and (p? x) ((apply conjoint-all preds) x)))))

;((conjoint-all (lambda (x) (> x 5)) (lambda (x) (< x 10))) 10)
;((conjoint-all (lambda (x) (> x 5)) (lambda (x) (< x 10))) 5)
;((conjoint-all (lambda (x) (> x 5)) (lambda (x) (< x 10))) 7)
;((conjoint-all (lambda (x) (< x 10))) 7)
;((conjoint-all (lambda (x) (> x 5)) (lambda (x) (< x 10)) (lambda (x) (> x 8))) 7)

; 10. Като map на много аргументи, но трябва да работи за
; списъци с различни дължини.
;(define (zipWith f . lsts)
;  (apply map f lsts))

(define (any? p? lst)
  (foldr (lambda (x acc) (or (p? x) acc)) #f lst))

(define (zipWith f . lsts)
  (if (or (null? lsts)
          (any? null? lsts))
      '()
      (cons (apply f (map car lsts))
            (apply zipWith f (map cdr lsts)))))

(map car '((1 2 3) (4 5 6)))
(zipWith + '(1 2 3) '(4 5))

; apply има вида: (apply proc v1 ... vn lst kw-arg ...)
; Засега не се интересуваме от kw-arg.
; Освен подадения списък, apply може да приема и
; допълнителни аргументи към подадената процедура proc.
; Това са онези v1 ... vn

; Тук 2 е допълнителен аргумент към *
;(apply * 2 '(1 2 3)) ; 12
; същото като:
;(* 2 1 2 3)

; Тук + е допълнителен аргумент към map:
;(apply map + '((1 2 3) (4 5 6))) ; '(5 7 9)
; същото като:
;(map + '(1 2 3) '(4 5 6))