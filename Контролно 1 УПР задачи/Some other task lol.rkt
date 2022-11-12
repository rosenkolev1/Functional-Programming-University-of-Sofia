;Some other task

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

;TASK

(define (maxDuplicate ll)
  (define (repeats-first x lst1)
    (if (null? lst1)
        #f
        (if (equal? x (car lst1))
          #t
          (repeats-first x (cdr lst1)))))

  (define (maxDuplicate-helper lst1 maxnum)
    (if (null? lst1)
      maxnum
      (if (not (list? (car lst1)))
          (if (and (repeats-first (car lst1) (cdr lst1)) (or (equal? maxnum #f) (> (car lst1) maxnum)))
              (maxDuplicate-helper (cdr lst1) (car lst1))
              (maxDuplicate-helper (cdr lst1) maxnum))
          (maxDuplicate-helper (cdr lst1) (maxDuplicate-helper (car lst1) maxnum)))))

  (maxDuplicate-helper ll #f))
  
;(repeats-first 3 '(2 3 4))
(maxDuplicate '(6 4 5 (1 2 3 2) (5 (5 3 3) 4) (-4 -4) (5))) ;-> 3
(maxDuplicate '((1 2 3 2) (-4 -4) (5))) ;-> 2
(maxDuplicate '((1 2 3) (-4 -5 -6) (1 2))) ;->f