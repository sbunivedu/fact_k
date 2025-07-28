#lang eopl

(define-datatype continuation continuation?
  (end-cont)
  (remove-first1-cont
   (s symbol?)
   (cont continuation?)))

(define s 'uninitialized)
(define los 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)

(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont () val)
      (remove-first1-cont (saved-s saved-cont)
                          (set! cont saved-cont)
                          (set! val (cons saved-s val))
                          (apply-cont)))))

(define remove-first
  (lambda (s los)
    (if (null? los) '()
        (if (eq? s (car los))
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

(define remove-first1
  (lambda (arg-s arg-los)
    (set! cont (end-cont))
    (set! s arg-s)
    (set! los arg-los)
    (remove-first/k)))

(define remove-first/k
  (lambda ()
    (if (null? los)
        (begin
          (set! val '())
          (apply-cont))
        (if (eq? s (car los))
            (begin
              (set! los (cdr los))
              (remove-first/k))
            (begin
              (set! cont (remove-first1-cont (car los) cont))
              (set! los (cdr los))
              (remove-first/k))))))

; (remove-first 'a '(a b c))
; (b c)
; (remove-first1 'a '(a b c))
; (b c)
; (remove-first 'b '(e f g))
; (e f g)
; (remove-first1 'b '(e f g))
; (e f g)
