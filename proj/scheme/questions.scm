(define (caar x) (car (car x)))

(define (cadr x) (car (cdr x)))

(define (cdar x) (cdr (car x)))

(define (cddr x) (cdr (cdr x)))

; ; Problem 15
; ; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (_enumerate x s)
    (if (null? s)
        ()
        (cons (cons x (cons (car s) nil))
              (_enumerate (+ x 1) (cdr s)))))
  (_enumerate 0 s))

; END PROBLEM 15
; ; Problem 16
; ; Merge two lists LIST1 and LIST2 according to INORDER? and return
; ; the merged lists.
(define (merge inorder? list1 list2)
  ; BEGIN PROBLEM 16
  (if (null? list1)
      (if (null? list2)
          ()
          (if (null? (cdr list2))
              (cons (car list2) nil)
              (cons (car list2)
                    (merge inorder? list1 (cdr list2)))))
      (if (null? list2)
          (if (null? (car list1))
              (cons (car list1) nil)
              (cons (car list1)
                    (merge inorder? (cdr list1) list2)))
          (if (inorder? (car list1) (car list2))
              (cons (car list1)
                    (merge inorder? (cdr list1) list2))
              (cons (car list2)
                    (merge inorder? list1 (cdr list2)))))))

; END PROBLEM 16
; ; Optional Problem 1
; ; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))

(define define? (check-special 'define))

(define quoted? (check-special 'quote))

(define let? (check-special 'let))

; ; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond 
    ((atom? expr)
     ; BEGIN PROBLEM 17
     'replace-this-line
     ; END PROBLEM 17
    )
    ((quoted? expr)
     ; BEGIN PROBLEM 17
     'replace-this-line
     ; END PROBLEM 17
    )
    ((or (lambda? expr) (define? expr))
     (let ((form (car expr))
           (params (cadr expr))
           (body (cddr expr)))
       ; BEGIN PROBLEM 17
       'replace-this-line
       ; END PROBLEM 17
     ))
    ((let? expr)
     (let ((values (cadr expr))
           (body (cddr expr)))
       ; BEGIN PROBLEM 17
       'replace-this-line
       ; END PROBLEM 17
     ))
    (else
     ; BEGIN PROBLEM 17
     'replace-this-line
     ; END PROBLEM 17
    )))

; ; Problem 21 (optional)
; ; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'replace-this-line)

; END Question 21
