; Problem 1
; 1)
(define lst1 (cons 'd (cons (cons (cons 'e (cons 'f '())) (cons (cons 'g '()) '())) '()))) ; '(d ((ef) (g)))
(cons 'a (cons (cons 'b (cons 'c '())) lst1)) ; '(a (b c) d ((ef) (g)))

; 2)
(cons * (cons 'a (cons '4 '())))

; Problem 2
; 1)
(define flatten
  (lambda (l)
  (cond ((null? l) '()) ; base case (list is empty)
        ((pair? (car l))
         (append (flatten (car l))
                 (flatten (cdr l))))
        (else (cons (car l) (flatten (cdr l)))))))
; 2)
(define rev
  (lambda (l)
    (if (null? l)
        '()
        (append (rev (cdr l)) (list (car l))))))
; 3)
(define delete
  (lambda (a l)
    (cond ((null? l) '())
          ((pair? (car l)) (cons (delete a (car l)) (delete a (cdr l))))
          ((equal? a (car l)) (delete a (cdr l)))
          (else (cons (car l) (delete a (cdr l)))))))
; 4)
(define (merge-sorted x y)
  (cond ((null? x) y)
        ((null? y) x)
        ((>= (car x) (car y))
         (cons (car y) (merge-sorted x (cdr y))))
        (else
         (cons (car x) (merge-sorted (cdr x) y)))))

; Problem 3
(define NewTable
  (lambda () '()))

(define InstertIntoTable
  (lambda (entry table)
    (cons entry table)))

(define LookupTable
  (lambda (entry table)
    (cond ((null? entry) '())
          ((null? table) '())
          ((pair? (car table))
                  (if (equal? entry (car (car table))) (car (cdr (car table))) (LookupTable entry (cdr table)))))))

(define table
  (InstertIntoTable '(b (2 4 5)) (InstertIntoTable '(a 7) (NewTable))))

;(write table)
(LookupTable 'a table)
(LookupTable 'b table)
(LookupTable 'c table)

; Problem 4
(define map
  (lambda (f l)
    (if (null? l)
        '()
        (cons (f (car l)) (map f (cdr l))) )))

(define reduce
  (lambda (op l id)
    (if (null? l)
        id
        (op (car l) (reduce op (cdr l) id)) )))

(define minSquareVal
  (lambda (l)
    (if (null? l) '()
    (reduce min (map (lambda (x) (* x x)) l) +inf.0))))

(define maxSquareVal
  (lambda (l)
    (if (null? l) '()
    (reduce max (map (lambda (x) (* x x)) l) -inf.0))))

(define min
  (lambda (x y)
    (if (<= x y) x y)))

(define max
  (lambda (x y)
    (if (>= x y) x y)))



