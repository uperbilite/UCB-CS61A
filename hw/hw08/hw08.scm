(define (my-filter func lst)
  (if (null? lst)
      ()
      (if (null? (cdr lst))
          (if (func (car lst))
              (cons (car lst) nil)
              nil)
          (if (func (car lst))
              (cons (car lst) (my-filter func (cdr lst)))
              (my-filter func (cdr lst))))))

(define (interleave s1 s2)
  (define (turn who)
    (if (= 1 who)
        2
        1))
  (define (_interleave s1 s2 who)
    (define next (turn who))
    (if (null? s1)
        (if (null? s2)
            () ; s1 and s2 are both null
            (if
             (null? (cdr s2)) ; s1 is null and s2 is not null
             (cons (car s2) nil)
             (cons (car s2) (_interleave s1 (cdr s2) next))))
        (if (null? s2)
            (if
             (null? (cdr s1)) ; s2 is not null and s2 is null
             (cons (car s1) nil)
             (cons (car s1) (_interleave (cdr s1) s2 next)))
            (if (= 1 next) ; s1 and s2 are not null
                (cons (car s1) (_interleave (cdr s1) s2 next))
                (cons (car s2) (_interleave s1 (cdr s2) next))))))
  (_interleave s1 s2 2) ; begin at s1
)

(define (accumulate merger start n term)
  (define (_accumulate _start)
    (if (= _start n)
        (term _start)
        (merger (term _start) (_accumulate (+ _start 1)))))
  (merger start (_accumulate 1)))

(define (no-repeats lst)
  (define (filter y) (lambda (x) (not (= x y))))
  (if (null? lst)
      ()
      (if (null? (cdr lst))
          lst
          (cons (car lst)
                (no-repeats (my-filter (filter (car lst)) lst))))))
