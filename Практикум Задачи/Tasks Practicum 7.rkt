#lang racket

; Функции от миналия път за асоциативни списъци:
;-----------------------------------------------
(define (make-alist fn keys)
  (map (lambda (key)
         (cons key (fn key))) keys))

(define (add-assoc key value alist)
  (cons (cons key value)
        alist))

(define (alist-keys alist)
  (map car alist))

(define (alist-values alist)
  (map cdr alist))

(define (alist-assoc key alist)
  (cond ((null? alist) '())
        ((equal? (caar alist) key) (cdar alist))
        (else (alist-assoc key (cdr alist)))))

(define (del-assoc key alist)
  (filter (lambda (alist-pair)
            (not (equal? (car alist-pair) key)))
          alist))

;(add-assoc 2 10 (list (cons 2 15)))

; Графи:
;-------
; Граф ще представяме като списък на съседство
; Тоест списък от списъци от върхове.
; За връх v ще дефинираме списък (v v1 .. vn)
; Където v1 до v1 са върховете до които v има ребро
; За всеки връх ще пазим такъв списък.
; Списъка от тези списъци ще е нашият граф.
;
; 1 --> 2
; |     |
; v     v
; 3 --> 4 --> 5
;
; Горният граф бихме представили по следния начин:
;'((1 2 3) ; 1 има ребро до 2 и 3
;  (2 4)   ; 2 има ребро до 4
;  (3 4)   ; 3 има ребро до 4
;  (4 5)   ; 4 има ребро до 5
;  (5))    ; 5 няма ребра до други върхове

;'() ; празен граф
;'((1 2 3) (2))
; ^ Невалидно защото твърдим че 1 има ребро до 3,
; но не сме включили списък за реброто 3

; Как да си създадем граф?
; Може да го конструираме само с върхове и без ребра
(define (make-graph vs)
  (map list vs))

; Още няколко функции за работа с графи:

; Проверка дали граф е празен
(define empty-graph? null?)

; Върховете на граф g са точно първите елементи
; на подсписъците на g.
; Тост g е списък от двойки и върховете са първите елементи
(define vertices alist-keys)

; За да добавим нов връх - добавяме списък от върха v.
; Той първоначално няма ребра до други върхове.
(define (add-vertex v g)
  (cons (list v) g))


; Отложени операции:
;-------------------
; delay от даден израз прави отложена операция (promise)
;(delay (+ 1 2)) ; #<promise>

; force взима отложена операция и я оценява
;(force (delay (+ 1 2))) ; 3

; Можем да симулираме delay и force чрез lambda


; Потоци:
;--------
; С помощта на delay и force можем да си дефинираме потоци:
; 1) '() е поток
; 2) (h . t) е поток <=> t е promise за списък

(define empty-stream '())

; За да постигнем това операцията cons ще трябва да прави
; нещо повече - да прилага delay върху 2рия елемент
; (define (cons-stream h t)
;   (cons h (delay t)))

; Проблем - delay е специална форма, но cons-stream
; не е и за нея важи стриктно оценяване.

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))

; Пример за функция, генерираща безкраен поток:
(define (repeat v)
  (cons-stream v (repeat v)))
; Тук ако бяхме използвали първия вариант на cons-stream,
; програмата ни щеше да се опита да изпълни безкрайната
; рекурсия и щеше да забие.


; Ето и базовите функции за работа с потоци:
;-------------------------------------------
; Когато ни трябва елемент от потока просто го взимаме
; (той е първият)
(define head car)

; Когато искаме да вземем опашката на потока,
; я оценяваме с force
(define (tail s)
  (force (cdr s)))

; Доста често ще искаме да взимаме елементи от поток.
; Функцията take за потоци:
(define (stream-take* n s)
  (if (or (zero? n) (null? s))
    '()
    (cons (head s)
          (stream-take* (- n 1) (tail s)))))


;===============;=========================================
;; З А Д А Ч И ;;
;;;;;;;;;;;;;;;;;

(define (1+ x) (+ x 1))

; За потоци:
;-----------
; Генерира безкрайния поток x, f(x), f(f(x)), ...
(define (iterate f x)
  (cons-stream (f x) (iterate f (f x))))

;(stream-take* 5 (iterate 1+ 5))

; Генерира безкраен поток от елементите на lst.
; (cycle '(1 2 3)) -> 1, 2, 3, 1, 2, 3, 1 ...
(define (cycle lst)
  (define originalLst lst)
  
  (define (cycle-iter lst)
    (cons-stream (head lst) (cycle-iter (if (null? (tail lst)) originalLst (tail lst)))))

  (cycle-iter lst))

;(stream-take* 7 (cycle '(1 2 3)))

; Прилага едноаргументна функция f
; над елементите на безкраен поток s.
(define (stream-map* f s)
  (cons-stream (f (head s)) (stream-map* f (tail s))))

;(stream-take* 11 (stream-map* 1+ (cycle '(0 1 2 3 4))))

; Генерира безкраен поток от естествените числа
(define nats  
  (iterate 1+ 0))

;(stream-take* 5 nats)

; За работа с графи:
;-------------------
; Може да ползвате горните функции за асоциативни списъци
; Не забравяйте че списъците са двойки,
; тоест списък от списъци всъщност е асоциативен списък.
; (стига да няма празни списъци като елементи)

; Връща списък от всички ребра* на графа g.
; * т.е. двойки върхове (x . y),
; такива че има ребро от x към y
(define (edges g)
  (foldl
   (lambda (kv acc)
     (append acc (foldr
      (lambda (y acc2)
        (add-assoc (car kv) y acc2))
      '()
      (alist-assoc (car kv) g))))
   '() g))

;(edges '((1 2 3) (2 4) (3 4) (4 5) (5)))

; Проверява дали има ребро от върха u до върха v в g.
(define (edge? u v g)
  (foldr
   (lambda (x acc)
     (if (and (equal? (car x) u) (equal? (cdr x) v))
         #t
         acc))
   #f (edges g)))

;(edge? 2 4 '((1 2 3) (2 4) (3 4) (4 5) (5)))
;(edge? 1 4 '((1 2 3) (2 4) (3 4) (4 5) (5)))
;(edge? 3 1 '((1 2 3) (2 4) (3 4) (4 5) (5)))
;(edge? 5 1 '((1 2 3) (2 4) (3 4) (4 5) (5)))

; Връща списък от децата на върха v в g.
(define (children v g)
  (alist-assoc v g))

;(children 1 '((1 2 3) (2 4) (3 4) (4 5) (5)))
;(children 2 '((1 2 3) (2 4) (3 4) (4 5) (5)))

; Връща списък от прилаганията на функцията f
; върху децата на v в g.
(define (map-children v f g)
  (map f (children v g)))

;(map-children 1 1+ '((1 2 3) (2 4) (3 4) (4 5) (5)))
;(map-children 2 1+ '((1 2 3) (2 4) (3 4) (4 5) (5)))

; Връща първото дете на v в g, за което предиката p
; е верен.
(define (search-child v p g)
  (let ((filteredList (filter p (children v g))))
    (if (null? filteredList) 'null
        (car filteredList))))

;(search-child 1 (lambda (x) (< x 4)) '((1 2 3) (2 4) (3 4) (4 5) (5)))
;(search-child 1 (lambda (x) (< x 4)) '((1 3 2) (2 4) (3 4) (4 5) (5)))
;(search-child 2 (lambda (x) (< x 3)) '((1 2 3) (2 4) (3 4) (4 5) (5)))

; Премахване на върха v от графа g,
; заедно с ребрата до него.
(define (remove-vertex v g)
  (let ((removedVertexKeyLst (del-assoc v g)))
    (foldr
     (lambda (x acc)
       (add-assoc (car x) (filter
                           (lambda (el) (not (equal? el v)))
                           (cdr x)) acc))
     '() removedVertexKeyLst)))

;(remove-vertex 2 '((1 2 3) (2 4) (3 4) (4 5) (5))) ;-> ((1 3) (3 4) (4 5) (5))
;(remove-vertex 1 '((1 2 3) (2 4 1) (3 4) (4 5 1) (5))) ;-> ((2 4) (3 4) (4 5) (5))

(define (member? lst x)
  (foldr
   (lambda (el acc)
     (if (equal? el x)
         #t
         acc))
   #f lst))

;(member? '(1 2 3) 2)
;(member? '(1 2 3) 5)
;(member? '() 2)

; Добавяне на ребро от u до v в g.
(define (add-edge u v g)
  (define addNewPath (if (not (member? (alist-keys g) u))
      (add-assoc u (list v) g)
      (foldr
       (lambda (x acc)
         (if (equal? (car x) u)
             (add-assoc (car x) (cons v (cdr x)) acc)
             (add-assoc (car x) (cdr x) acc)))
       '() g)))

  (if (not (member? (alist-keys g) v))
      (add-assoc v (list) addNewPath)
      addNewPath))

;(add-edge 1 5 '((1 2 3) (2 4) (3 4) (4 5) (5))) ; -> ((1 2 3 5) (2 4) (3 4) (4 5) (5))
;(add-edge 5 4 '((1 2 3) (2 4) (3 4) (4 5) (5))) ; -> ((1 2 3) (2 4) (3 4) (4 5) (5 4))
;(add-edge 6 3 '((1 2 3) (2 4) (3 4) (4 5) (5))) ; -> ((1 2 3) (2 4) (3 4) (4 5) (5) (6 3))
;(add-edge 6 7 '((1 2 3) (2 4) (3 4) (4 5) (5))) ; -> ((1 2 3) (2 4) (3 4) (4 5) (5) (6 7))

; Премахване на ребро от u до v в g.
(define (remove-edge u v g)
  (foldr
   (lambda (x acc)
     (if (equal? u (car x))
         (add-assoc (car x) (filter
                             (lambda (el) (not (equal? el v)))
                             (cdr x)) acc)
         (add-assoc (car x) (cdr x) acc)))
   '() g))

;(remove-edge 1 2 '((1 2 3) (2 4) (3 4) (4 5) (5))) ;->((1 3) (2 4) (3 4) (4 5) (5))
;(remove-edge 2 1 '((1 2 3) (2 1 4) (3 4) (4 5) (5))) ;->((1 2 3) (2 4) (3 4) (4 5) (5))

; За графи:
;----------
; Връща степента на върха v в графа g.
(define (degree v g)
  (length (children v g)))

;(degree 1 '((1 2 3) (2 4) (3 4) (4 5) (5))) ;-> 2
;(degree 2 '((1 2 3) (2 4) (3 4) (4 5) (5))) ;-> 1
;(degree 5 '((1 2 3) (2 4) (3 4) (4 5) (5))) ;-> 0
;(degree 6 '((1 2 3) (2 4) (3 4) (4 5) (5))) ;-> 0

; Проверява дали графа g е симетричен.
(define (symmetric? g)
  (foldr
   (lambda (x acc)
         (if (foldr
              (lambda (x2 acc2)
                (if (and (equal? (car x2) (cdr x)) (equal? (car x) (cdr x2)))
                    #t
                    acc2))
              #f (edges g))
             acc
             #f))
   #t (edges g)))

;(symmetric? '((1 2 3) (2 4) (3 4))) ;-> f
;(symmetric? '((1 2 3) (2 1) (3 1))) ;-> t

; Инвертира графа g.
; Тоест за всяко ребро (u,v) в g новият граф
; ще има реброто (v,u).
(define (invert g)
  (append
   (foldr
    (lambda (x acc)
      (add-edge (cdr x) (car x) acc))
    '() (edges g))
   (filter
    (lambda (x)
      (null? (cdr x))) g)))

;(invert '((1 2 3) (2 4) (3 4) (4 5) (5))) ;-> ((1) (2 1) (3 1) (4 2 3) (5 4) (5))

; Проверява дали има път между върховете u и v в графа g.
(define (path? u v g)
  (or (member? (children u g) v) (member? (children v g) u)))

;(path? 1 2 '((1 2 3) (2 4) (3 4) (4 5) (5))) ;->t
;(path? 1 3 '((1 2 3) (2 4) (3 4) (4 5) (5))) ;->t
;(path? 3 1 '((1 2 3) (2 4) (3 4) (4 5) (5))) ;->t
;(path? 2 5 '((1 2 3) (2 4) (3 4) (4 5) (5))) ;->f

; Проверява дали графа g е ацикличен.
(define (acyclic? g)
  (define edgesG (edges g))
  
  (define (dfs from g visitedV)
    (if (null? (children from g))
        (list #t)
        (foldr
         (lambda (x acc)
           (if (member? visitedV x)
               (append (list #f) acc)
               (append (dfs x g (cons x visitedV)) acc)))
         '() (children from g))))

  (not (member? (dfs (caar g) g (list (caar g))) #f )))

; 1 --> 2
; |     |
; v     v
; 3 --> 4 --> 5
;(acyclic? '((1 2 3) (2 4) (3 4) (4 5) (5))) ;->t
;(acyclic? '((1 2 3) (2 4) (3 4) (4 5 6 1) (5) (6))) ;->f