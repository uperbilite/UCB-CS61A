(define (cddr s) (cdr (cdr s)))

(define (cadr s) (car (cdr s)))

(define (caddr s) (car (cdr (cdr s))))

(define (ordered? s)
  (if (not (null? (cdr s)))
      (if (> (car s) (cadr s))
          #f
          (ordered? (cdr s)))
      #t))

(define (square x) (* x x))

(define (pow base exp)
  (cond 
    ((= exp 0)
     1)
    ((= exp 1)
     base)
    (else
     (if (even? exp)
         (square (pow base (/ exp 2)))
         (* (pow (* base) (- exp 1)) base)))))
