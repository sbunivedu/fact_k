#lang eopl

(define end-cont
  (lambda ()
    (lambda (val)
      (begin
        (eopl:printf "End of computation.~%")
        (eopl:printf "This sentence should appear only once.~%")
        val))))

(define remove-first1-cont
  (lambda (s saved-cont)
    (lambda (val)
      (apply-cont saved-cont (cons s val)))))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define remove-first
  (lambda (s los)
    (if (null? los) '()
        (if (eq? s (car los))
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

(define remove-first1
  (lambda (s los)
    (remove-first/k s los (end-cont))))

(define remove-first/k
  (lambda (s los k)
    (if (null? los) (apply-cont k '())
        (if (eq? s (car los))
            (apply-cont k (cdr los))
            (remove-first/k s (cdr los)
              (remove-first1-cont (car los) k))))))

; (remove-first 'a '(a b c))
; (b c)
; (remove-first1 'a '(a b c))
; (b c)
; (remove-first 'b '(e f g))
; (e f g)
; (remove-first1 'b '(e f g))
; (e f g)
