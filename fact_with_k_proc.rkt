#lang eopl

; procedural representation of continuations

(define end-cont
  (lambda ()
    (lambda (val)
      (begin
        (eopl:printf "End of computation.~%")
        (eopl:printf "This sentence should appear only once.~%")
        val))))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define fact1-cont
  (lambda (n saved-cont)
    (lambda (val)
      (apply-cont saved-cont (* n val)))))

(define fact
  (lambda (n)
    (fact/k n (end-cont))))

(define fact/k
  (lambda (n cont)
    (if (zero? n)
        (apply-cont cont 1)
        (fact/k (- n 1) (fact1-cont n cont)))))

; (fact 5) => 120