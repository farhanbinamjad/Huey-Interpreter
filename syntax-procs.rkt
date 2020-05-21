;;  ------------------------------------------------------------------------
;; |   FILE           :  syntax-procs.rkt                                   |
;; |   AUTHOR         :  Farhan Amjad                                       |
;; |   CREATION DATE  :  2020/04/29                                         |
;; |   DESCRIPTION    :  Syntax procedure functions for Huey language       |           
;;  ------------------------------------------------------------------------

#lang racket

(require "utilities.rkt")
(require "datatypes.rkt")
(provide color?  rgb? unary? 2color? 1color?
         2color->first-col  2color->second-col 2color->op make-2color
         1color->color 1color->number 1color->op make-1color
         rgb->byte1  rgb->byte2 rgb->byte3 make-rgb is-keyword? do?
         assignment?    do->color  do->assgn assgn->varref assgn->color
         make-do unary->color       unary->op           make-unary
         varref?            make-varref         get-varref     color-in?
         color-in->val      color-in->body      color-in->var  make-color-in
         make-assignment    make-do-in-fn       define-var?    define->varref
         define->body       make-define)

;; ------------------------------------------------------------------------
;;  This code works with the following grammar:
;
;     <color> ::= (rgb <byte> <byte> <byte> )
;               | <varref>
;               | ( <unary-op> <color> )
;               | ( <color> <2color-op> <color> )
;               | ( <color> <1color-op> <number> )
;               | ( color <var> = <color> in <color> )
;               | ( do <assignment>* <color> )          ; new
;
;<assignment> ::= ( <varref> <= <color> )               ; new
;
;  <unary-op> ::= invert | darker
; <2color-op> ::= + | - | mix
; <1color-op> ::= * | shift
;;
;; ------------------------------------------------------------------------

;; ------------------------------------------------------------------------
;; general type predicate
;; ------------------------------------------------------------------------

(define color?
  (lambda (exp)
    (or (rgb? exp)
        (varref? exp)
        (unary? exp)
        (do? exp)
        (2color? exp)
        (1color? exp)
        (define-var? exp) 
        (assignment? exp)  
        (color-in? exp))))

;;  ------------------------------------------------------------------------
;  rgb check
;;  ------------------------------------------------------------------------

(define rgb?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 4)
         (eq? (first exp) 'rgb)
         (number? (rgb->byte1 exp))
         (number? (rgb->byte2 exp))
         (number? (rgb->byte3 exp)))))

(define rgb->byte1 second)
(define rgb->byte2 third)
(define rgb->byte3 fourth)

(define make-rgb
  (lambda (byte1 byte2 byte3)
    (list 'rgb (check byte1)
          (check byte2)
          (check byte3))))

(define check 
  (lambda (n)       
    (cond ((> n 255) 255)
          ((< n 0) 0)
          (else (inexact->exact (truncate n))))))

;;  ------------------------------------------------------------------------
;  repl
;;  ------------------------------------------------------------------------

(define define-var?
  (lambda (exp)
    (and (list? exp)
         (= 4 (length exp))
         (eq? (first exp) 'define)
         (varref? (define->varref exp))
         (eq? (third exp) '<=)
         (color? (define->body exp)))))

(define define->varref second)
(define define->body fourth)

(define make-define
  (lambda (var body)
    (list 'define var '<= body)))


;;  ------------------------------------------------------------------------
;;  varrefs
;;  ------------------------------------------------------------------------

(define keywords '(color = in do <=))

(define varref?
  (lambda (exp)
    (if (symbol? exp)
        (if (member exp keywords)
            #f
            #t)
        #f)))

(define make-varref
  (lambda (exp)
    exp))

(define get-varref
  (lambda (exp)
    exp))

(define is-keyword?
  (lambda (var)
    (if (member var keywords)
        #t
        #f)))


;; ------------------------------------------------------------------------
;; unary expressions
;; ------------------------------------------------------------------------

(define unary?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 2)
         (or (eq? (unary->op exp) 'invert)
             (eq? (unary->op exp) 'darker))
         (color? (unary->color exp)))))

(define unary->color second)
(define unary->op first)

(define make-unary
  (lambda (unary-op color)
    (list unary-op color)))

;; ------------------------------------------------------------------------
;; 2-color expressions
;; ------------------------------------------------------------------------

(define 2color?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 3)
         (color? (2color->first-col exp))
         (or (eq? (2color->op exp) '+)
             (eq? (2color->op exp) '-)
             (eq? (2color->op exp) 'mix))
         (color? (2color->second-col exp)))))

(define 2color->first-col first)
(define 2color->second-col third)
(define 2color->op second)

(define make-2color
  (lambda (color1 op color2)
    (list color1 op color2)))

;; ------------------------------------------------------------------------
;; 1-color expressions
;; ------------------------------------------------------------------------

(define 1color?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 3)
         (color? (1color->color exp))
         (or (eq? (1color->op exp) '*)
             (eq? (1color->op exp) 'shift))
         (number? (1color->number exp)))))

(define 1color->color first)
(define 1color->number third)
(define 1color->op second)

(define make-1color
  (lambda (color op numb)
    (list color op numb)))

;; ------------------------------------------------------------------------
;;  color-in expressions
;;  -----------------------------------------------------------------------

(define color-in?
  (lambda (exp)
    (and ((list-of? 6) exp)
         (eq? 'color (first exp))
         (symbol? (second exp))
         (eq? '= (third exp))
         (color? (fourth exp))
         (eq? 'in (fifth exp))
         (color? (sixth exp)))))

(define make-color-in
  (lambda (var col1 col2)
    (list 'color var '= col1 'in col2)))

(define color-in->var second)
(define color-in->val fourth)
(define color-in->body sixth)


;; ------------------------------------------------------------------------
;;  do expressions
;; ------------------------------------------------------------------------

(define do?
  (lambda (exp)
    (and (list? exp)
         (eq? (first exp) 'do)
         (check-rest (rest exp)))))

(define check-rest
  (lambda (exp)
    (if (= 1 (length exp))
        (color? (first exp))
        (and (assignment? (first exp))
             (check-rest (rest exp))))))

(define assignment?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 3)
         (varref? (assgn->varref exp))
         (eq? (second exp) '<=)
         (color? (assgn->color exp)))))

(define do->color
  (lambda (exp)
    (list-ref exp (sub1 (length exp)))))

(define do->assgn
  (lambda (exp)
    (letrec ((helper (lambda (exp1 lst)
                       (if (= 1 (length exp1))
                           lst
                           (helper (rest exp1) (append lst (list (first exp1))))))))
      (helper (rest exp) '()))))
 
(define assgn->varref first)
(define assgn->color third)

(define make-do
  (lambda (assgn . color)
    (if (null? color)
        (append (list 'do) (list assgn))
        (append (list 'do) (list assgn) color))))

(define make-do-in-fn
  (lambda (assgn color)
    (if (null? assgn)
        (list 'do color)
        (append (list 'do) assgn (list color)))))

(define make-assignment
  (lambda (varref color)
    (list varref '<= color)))


;; ------------------------------------------------------------------------
