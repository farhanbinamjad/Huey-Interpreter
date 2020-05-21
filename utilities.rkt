#lang racket
(provide list-of? member?)

;; ------------------------------------------------------------------------
;; list predicates
;; ------------------------------------------------------------------------

(define list-of?
  (lambda (arg)
    (cond ((number? arg)                   ; (list-of <length>)
              (lambda (x)
                (and (list? x)
                     (= arg (length x)))))
          ((procedure? arg)                ; (list-of <type?>)
              (lambda (x)
                (and (list? x)
                     (andmap arg x))))
          (else
              (error 'list-of? "invalid argument ~a" arg)))))

(define member?
  (lambda (x lst)
    (if (member x lst) #t #f)))

;; ------------------------------------------------------------------------

