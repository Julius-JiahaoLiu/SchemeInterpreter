(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (x) (append (list first) x)) rests) ;(map <proc> <lst>) proc is a one-argument procedure!!!
)

;; Problem 16
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 16
  (define (helper lst i sofar)
    (if (null? lst) sofar
      (helper (cdr lst) (+ i 1) (append sofar (list (list i (car lst))))) ; tail recursion!!!
    )
  )
  (helper s 0 nil)
  )
  ; END PROBLEM 16

;; Problem 17
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 17
  (if (eq? total 0) (list nil)
    (if (or (< total 0) (null? denoms)) nil
      (if (< total (car denoms))
        (list-change total (cdr denoms))
        (append 
          (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) 
          (list-change total (cdr denoms))
        )
      )
    )
  )
)
  ; END PROBLEM 17

;; Problem 18
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

(define (zip pairs) ;only consider pairs with two elements.
  (define helper pairs first second
    (if (null? pairs) (list first second)
        (helper (cdr pairs) (append first (caar pairs)) (append second (cdar pairs)))
      )
    )
  (helper pairs nil nil)
)
;; Converts all let special forms in EXPR into equivalent forms using lambda
;; The structure of let-to-lambda is somewhat similar to that of scheme_eval--but in Scheme! 
(define (let-to-lambda expr)
  (cond ((atom? expr) ;atoms include numbers, booleans, nil, and symbols.
         ; BEGIN PROBLEM 18
        expr
         ; END PROBLEM 18
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 18
        expr 
         ; END PROBLEM 18
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr)) ; lambda or define 
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
          (cons form (cons params (map let-to-lambda body)))
           ; END PROBLEM 18
           ))
        ((let? expr) ; let-to-lambda
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
          (define t (zip values)) ; the pairs in the let defination only contain two elements!!!
          ; Since Scheme expressions are recursively nested, let-to-lambda must also be recursive!!!
          (append (cons (cons 'lambda (cons (car t) (map let-to-lambda body))) nil) (map let-to-lambda (cadr t)))
           ; END PROBLEM 18
          ))
        (else
         ; BEGIN PROBLEM 18
        (cons (car expr) (map let-to-lambda (cdr expr)))
         ; END PROBLEM 18
         )))