;;  ------------------------------------------------------------------------
;; |   FILE           :  datatypes.rkt                                      |
;; |   AUTHOR         :  Farhan Amjad                                       |
;; |   CREATION DATE  :  20202/04/22                                         |
;; |   DESCRIPTION    :  Utility functions for Huey language interpreter.   |
;;  ------------------------------------------------------------------------

#lang racket

(provide (all-defined-out))
(require "utilities.rkt")
;; -------------------------------------------------------------------------
;; environment adt
;; -------------------------------------------------------------------------

(define make-bindings
  (lambda ()
    (list)))

(define bind
  (lambda (var val bindings)
    (cons (cons var val)
          bindings)))

(define look-up
  (lambda (var env)
    (let ((x (assoc var env)))
      (if x
          (cdr x)
          (error 'environment "undefined variable -- ~a" var)))))


(define var-exists?
  (lambda (var env)
    (if (assoc var env)
        #t #f)))

;; -------------------------------------------------------------------------
;; cell adt
;; -------------------------------------------------------------------------

(define cell
  (lambda (val)
    (lambda (arg)
      (case arg
        ('value val)
        ('set
         (lambda (new-val)
           (set! val new-val)
           new-val))
        (else
         (error "Unknown cell."))))))


(define cell-value
  (lambda (cell)
    (cell 'value)))

(define cell-set!
  (lambda (cell new-val)
    ((cell 'set) new-val)))

;; -------------------------------------------------------------------------