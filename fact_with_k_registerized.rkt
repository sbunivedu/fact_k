#lang eopl

(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
   (n integer?)
   (cont continuation?)))

(define n 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)

(define fact
  (lambda (arg-n)
    (set! cont (end-cont))
    (set! n arg-n)
    (fact/k)))

(define fact/k
  (lambda ()
    (if (zero? n)
        (begin
          (set! val 1)
          (apply-cont))
        (begin
          (set! cont (fact1-cont n cont))
          (set! n (- n 1))
          (fact/k)))))

(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont () val)
      (fact1-cont (saved-n saved-cont)
                  (set! cont saved-cont)
                  (set! val (* saved-n val))
                  (apply-cont)))))

; (trace fact/k)
; (fact 5) => 120