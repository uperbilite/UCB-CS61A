(define (split-at lst n)
  (define (first lst n)
    (if (= n 0)
        nil
        (cons (car lst) (first (cdr lst) (- n 1)))))
  (define (remain lst n)
    (if (= n 0)
        (if (= 0 (length lst))
            nil
            (cons (car lst) (remain (cdr lst) n)))
        (remain (cdr lst) (- n 1))))
  (if (> n (length lst))
      (cons lst nil)
      (cons (first lst n) (remain lst n))))

(define (compose-all funcs)
  (define x (lambda (x) x))
  (define (compose funcs x)
    (if (eq? funcs nil)
        x
        (if (eq? (cdr funcs) nil)
            ((car funcs) x)
            (compose (cdr funcs) ((car funcs) x)))))
  (define (procedure x) (compose funcs x))
  procedure)
