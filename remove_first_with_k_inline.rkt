#lang eopl

(define remove-first
  (lambda (s los)
    (if (null? los) '()
        (if (eq? s (car los))
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

(define remove-first1
  (lambda (s los)
    (remove-first/k s los (lambda (val) val))))

(define remove-first/k
  (lambda (s los k)
    (if (null? los) (k '())
        (if (eq? s (car los))
            (k (cdr los))
            (remove-first/k s (cdr los)
              (lambda (val)
                (k (cons (car los) val))))))))

; (remove-first 'a '(a b c))
; (b c)
; (remove-first1 'a '(a b c))
; (b c)
; (remove-first 'b '(e f g))
; (e f g)
; (remove-first1 'b '(e f g))
; (e f g)