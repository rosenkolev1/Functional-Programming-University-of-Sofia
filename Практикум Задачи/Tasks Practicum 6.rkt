#lang racket

; Двоични дървета:
;-----------------
; Двоично дърво дефинираме по следния начин:
; 1) '() е двоично дърво.
; 2) (root left right) е двоично дърво, точно когато
;   left и right са двоични дървета,
;   а root е просто стойността в корена.

; Дефинираме си няколко функции за работа с дървета:

(define root car)

(define left cadr)

(define right caddr)

; Проверяваме по дефиницията
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (left t))
           (tree? (right t)))))

; Конструктор
(define (make-tree root left right)
  (list root left right))

(define empty? null?)

; Едно дърво е листо ако има вида (root '() '()),
; тоест текущия връх няма наследници
(define (leaf? t)
  (and (empty? (left t))
       (empty? (right t))))


; Пример:
(define t
  '(1 (2 () ())
      (3 ()
         (4 () ()))))

; Изглежда така:
;
;   1
;  / \
; 2   3
;      \
;       4


; Асоциативни списъци:
;---------------------
; асоциативен списък ще наричаме списък от двойки
; от вида (key . value)

; още познато като map или dictionary

; ето и някой основни функции:

; По функция и списък от ключове, правим
; асоциативен списък с елементи от вида (key . fn(key))
(define (make-alist fn keys)
  (map (lambda (key)
         (cons key (fn key)))
       keys))

; Вече можем и да добавяме елементи в нашия списък
(define (add-assoc key value alist)
  (cons (cons key value)
        alist))

;(add-assoc 'purpose 42 '()) ; '((purpose . 42))

; Ще е хубаво да имаме и функции с които да вземем
; само ключовете или само стойностите на списъка
(define (alist-keys alist)
  (map car alist))

;===============;=========================================
;; З А Д А Ч И ;;
;;;;;;;;;;;;;;;;;
(define (1+ x) (+ 1 x))
(define (id x) x)

; За двоични дървета:
;--------------------
; 1. Намира броя на листата в tree.
(define (count-leaves tree)
  (if (empty? tree)
      0
      (if (leaf? tree)
          1
          (+ (count-leaves (left tree)) (count-leaves (right tree))))))

;(count-leaves '(1 (2 () ()) (3 (4 () ()) (7 () ()))))
;(count-leaves '(1 () ()))
;(count-leaves '())

; 2. Връща ново дърво, в което f е приложена над
; всеки връх от tree.
(define (map-tree f tree)
  (if (empty? tree)
      '()
      (list (f (root tree)) (map-tree f (left tree)) (map-tree f (right tree)))))

;(map-tree 1+ '(3 (4 () ()) (5 (0 () ()) (10 () ())))) ;-> (4 (5 () ()) (6 (1 () ()) (11 () ())))
;(map-tree 1+ '())
;(map-tree 1+ '(5 () ()))

; 3. Връща списък от всички върхове на разстояние n от
; корена на tree.
;(define (append lst1 lst2)
;  (if (empty? lst1)
;      lst2
;      (cons (car lst1) (append (cdr lst1) lst2))))

(define (level n tree)
  
  (if (and (> n 0) (not (empty? tree)))
      (append (level (- n 1) (left tree)) (level (- n 1) (right tree)))
      (if (and (<= n 0) (not (empty? tree)))
          (list (root tree))
          '())))

;(level 0 '(5 (4 () ()) (3 (2 () ()) (1 () ())))) ;-> (5)
;(level 1 '(5 (4 () ()) ())) ;-> (4)
;(level 1 '(5 (4 () ()) (3 (2 () ()) (1 () ())))) ;-> (4 3)
;(level 2 '(5 (4 (10 () ()) ()) (3 (2 () ()) (1 () ())))) ;-> (10 2 1)

; Обхождане на дърво, функциите да връщат списък
; от върховете в реда на обхождането им:
;---------------------------------------
; 4. корен-ляво-дясно
; (pre-order t) -> (1 2 3 4)
(define (pre-order t)
  (if (empty? t)
      '()
      (append (append (list (root t)) (pre-order (left t))) (pre-order (right t)))))

;(pre-order '(1 (2 () ()) (3 (4 () ()) ()))) ;-> (1 2 3 4)

; 5. ляво-корен-дясно
; (in-order t) -> (2 1 3 4)
(define (in-order t)
  (if (empty? t)
      '()
      (append (append (in-order (left t))  (list (root t))) (in-order (right t)))))

;(in-order '(1 (2 () ()) (3 (4 () ()) ()))) ;-> (2 1 4 3)

; 6. ляво-дясно-корен
; (post-order t) -> (2 4 3 1)
(define (post-order t)
  (if (empty? t)
      '()
      (append (append (post-order (left t)) (post-order (right t))) (list (root t)))))

;(post-order '(1 (2 () ()) (3 (4 () ()) ()))) ;-> (2 4 3 1)

; 7. Обръща 2ката от наследници на всеки връх
; (root left right) -> (root right left)
(define (flip-tree t)
  (if (empty? t)
      '()
      (list (root t) (flip-tree (right t)) (flip-tree (left t)))))

;(flip-tree '(1 (2 () ()) (3 (4 () ()) ()))) ;-> (1 (3 () (4 () ())) (2 () ()) )


; За асоциативни списъци:
;------------------------
; 8.Връща списък от стойностите на асоциативен списък
(define (alist-values alist)
  (map cdr alist))

;(alist-keys '(("first" . 1) ("second" . 2)))
;(alist-values '(("first" . 1) ("second" . 2)))
;(alist-keys '())
;(alist-values '())

; 9. По дадени ключ и асоциативен списък,
; връща стойността от първата намерена двойка с ключа
(define (alist-assoc key alist)
  (if (empty? alist)
      #f
      (if (equal? (car (car alist)) key)
          (cdr (car alist))
          (alist-assoc key (cdr alist)))))

;(alist-assoc "first" '(("first" . 1) ("second" . 2))) ;-> 1
;(alist-assoc "second" '(("first" . 1) ("second" . 2))) ;-> 1
;(alist-assoc "three" '(("first" . 1) ("second" . 2))) ;-> 1

; 10. По даден ключ изтрива първата съответстваща двойка
; със същия ключ
(define (del-assoc key alist)
  (if (null? alist)
      '()
      (if (equal? key (caar alist))
          (cdr alist)
          (cons (car alist) (del-assoc key (cdr alist))))))

;(del-assoc "first" '(("first" . 1) ("second" . 2))) ;-> '(("second" . 2))
;(del-assoc "second" '(("first" . 1) ("second" . 2))) ;-> '(("first" . 1))
;(del-assoc "third" '(("first" . 1) ("second" . 2))) ;-> '(("first" . 1) ("second" . 2))

; 11. Връща списък от списъци (result . args),
; където args са точно тези елементи x от lst,
; за които f(x) = result
(define (group-by* f lst)
  (foldr
   (lambda (el acc)
     (if (member (f el) (alist-keys acc))
         (foldl
          (lambda (kv acc1)
            (let* ((key (car kv))
                   (value (car (cdr kv)))
                   (newValue (list (append (list el) value))))
              (if (= (car kv) (f el))
                (append acc1 (list (cons key newValue)))
                (append acc1 (list kv)))))
          '() acc)
         (add-assoc (f el) (list (list el)) acc)))
     '() lst))

;(list (cons 1 (list '(10 20))) (cons 2 (list '(2 3))) (cons 3 (list '(4 5))))

(group-by* 1+ '(4 3 2 4 5 3 2 4 5 5 5)) ;-> ((5 . (4 4 4)) (4 . (3 3)) (3 . (2 2)) (6 . (5 5 5 5)))
  