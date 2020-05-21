;;  ------------------------------------------------------------------------
;; |   FILE           :  tests.rkt                                          |
;; |   AUTHOR         :  Farhan Amjad                                       |
;; |   CREATION DATE  :  2020/04/30                                         |
;; |   DESCRIPTION    :  Tests for Huey language syntax procedures,         |
;; |                     interpreter, and utilities.  
;;  ------------------------------------------------------------------------
#lang racket


(require rackunit)

(require "syntax-procs.rkt")
(require "interpreter.rkt")
(require "utilities.rkt")
(require "datatypes.rkt")

;;  ------------------------------------------------------------------------
;; |                        syntax procedures tests                         |
;;  ------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; Huey type predicates
;; -------------------------------------------------------------------------

;; rgb

(check-equal? (color? '(rgb 5 11 122)) #t)
(check-equal? (color? '(rgb 25 111 22)) #t)
(check-equal? (color? '(rgb 34 1 132)) #t)
(check-equal? (color? '(rgb 13 11 555)) #t)

;; unary operations
(check-equal? (color? '(invert (rgb 5 11 122))) #t)
(check-equal? (color? '(darker (rgb 22 16 121))) #t)

;; 2color-operations
(check-equal? (color? '((rgb 5 11 12) + (rgb 5 11 122))) #t)
(check-equal? (color? '((rgb 23 11 132) - (rgb 5 11 122))) #t)
(check-equal? (color? '((rgb 89 21 131) mix (rgb 5 11 122))) #t)


;; color-in expressions
(check-equal? (color? '(color x = ((rgb 5 11 12) + (rgb 5 11 122)) in ((rgb 23 11 132) - (rgb 5 11 122)))) #t)
(check-equal? (color? '(color x = (rgb 25 111 22) in
                            (invert (rgb 5 11 122)))) #t)

;; 1color
(check-equal? (color? '((rgb 150 99 42) * 1.6)) #t)
(check-equal? (color? '((rgb 150 99 42) shift -50)) #t)

;; nested expressions
(check-equal? (color? '(((rgb 150 99 42) * 1.6) * 1.5)) #t)
(check-equal? (color? '(((rgb 5 11 122) shift 1.5) mix (((rgb 150 99 42) * 1.6) * 1.5))) #t)
(check-equal? (color? '((rgb 13 11 125) - ((rgb 89 21 131) * 1.5))) #t)
;

;;;  -------------------------------------------------------------------------
;;; |                         environment adt tests                           |
;;;  -------------------------------------------------------------------------

(check-equal? (bind 'foo 1 (bind 'bar 2 (make-bindings)))
              '((foo . 1) (bar . 2)))


(check-equal? (eval-exp 'white)
     '(255 255 255))

(check-equal? (eval-exp '(do (rgb 255 0 0))) '(255 0 0))

(check-equal? (eval-exp '(color c = (rgb 0 255 0) in
                       (do (c <= (c mix (rgb 0 0 255)))
                           (c <= (invert c))
                           (darker (c shift 5))))) '(127 66 66))

(check-equal? (eval-exp '(color c = (rgb 0 255 0) in
                     (color d = (rgb 0 0 255) in
                        (do (c <= (c mix d))
                            (d <= (c mix d))
                            ((c mix d) shift 5))))) '(5 99 163))


(check-equal? (preprocess '(do (rgb 255 0 0)))
     '(do (rgb 255 0 0)))

(check-equal? (preprocess '(color c = (rgb 0 255 0) in
                       (do (c <= (c mix (rgb 0 0 255)))
                           (c <= (invert c))
                           (darker (c shift 5)))))
     '(color c = (rgb 0 255 0) in
         (do (c <= ((c * 0.5) + ((rgb 0 0 255) * 0.5)))
             (c <= (invert c))
             ((c shift 5) * 0.5))))

(define foo (cell 0))
(check-equal?(cell-value foo)
     0)
(check-equal? (cell-set! foo 10)
     10)
(check-equal? (cell-value foo)
     10)
(check-equal? (cell-set! foo (+ (cell-value foo) 5))
     15)
(check-equal? (cell-value foo)
     15)