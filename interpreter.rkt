;;  ------------------------------------------------------------------------
;; |   FILE           :  interpreter.rkt                                    |
;; |   AUTHOR         :  Farhan Amjad                                       |
;; |   CREATION DATE  :  2020/04/26                                         |
;; |   DESCRIPTION    :  Intepreter for Huey language consisting of a       |
;; |                     preprocessor and an evaluator. Used the huey-v1
;; |                     used in class to recreate                          |
;;  ------------------------------------------------------------------------

#lang racket

(require "syntax-procs.rkt")
(require "utilities.rkt")
(require "datatypes.rkt")
(provide preprocess eval-exp run-huey)

;;  ------------------------------------------------------------------------
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
;
;; -------------------------------------------------------------------------
;; preprocess :: full-exp -> core-exp
;; -------------------------------------------------------------------------

(define preprocess
  (lambda (exp)
    (cond ((rgb? exp) exp)
          ((varref? exp)
           (make-varref exp))
          
          ((1color? exp)
           (make-1color (preprocess (1color->color exp))
                                          (1color->op exp)
                                          (1color->number exp)))

          ((unary? exp)
           (if (eq? (unary->op exp) 'darker) 
                                 (make-1color (preprocess (unary->color exp))
                                              '* 0.5)
                                 (make-unary (unary->op exp)
                                             (preprocess (unary->color exp)))))
          ((assignment? exp)
           (make-assignment (preprocess (assgn->varref exp))
                                              (preprocess (assgn->color exp))))
          ((color-in? exp)
           (make-color-in (color-in->var exp)
                                            (preprocess (color-in->val exp))
                                            (preprocess (color-in->body exp))))
          ((do? exp)
           (let ((assgn (map preprocess (do->assgn exp)))
                                   (color (preprocess (do->color exp))))
                               (if (null? assgn)
                                   (make-do color)
                                   (make-do-in-fn assgn color))))


          ((define-var? exp) (make-define (define->varref exp)
                                          (preprocess (define->body exp))))

          (else             (if (eq? (2color->op exp) 'mix)                   
                                (make-2color (make-1color (preprocess (2color->first-col exp))
                                                          '* 0.5)
                                             '+
                                             (make-1color (preprocess (2color->second-col exp))
                                                          '* 0.5))
                                (make-2color (preprocess (2color->first-col exp))
                                             (2color->op exp)
                                             (preprocess (2color->second-col exp))))))))

;; --------------------------------------------------------------------------
;; This function preprocesses and evaluates programs written in the full
;; Huey language.  If its argument is not a valid exp, it signals an error.
;; --------------------------------------------------------------------------

(define eval-exp
  (lambda (exp)
    (if (color? exp)
        (post-process (eval-helper (preprocess exp) initial-env))
        (error 'huey "illegal expression -- ~a" exp))))

(define post-process  ;(rgb 1 2 3) --> (1 2 3)
  (lambda (exp)
    (list (rgb->byte1 exp)
          (rgb->byte2 exp)
          (rgb->byte3 exp))))

(define eval-helper
  (lambda (exp env)
    (cond ((rgb? exp)       (eval-rgb exp))
          ((varref? exp)    (eval-varref exp env))  
          ((unary? exp)     (eval-unary exp env))
          ((1color? exp)    (eval-1color exp env))
          ((color-in? exp)  (eval-color-in exp (bind (color-in->var exp)
                                                (cell (eval-helper (color-in->val exp) env)) env)))
          ((2color? exp)    (eval-2color exp env))
          ((do? exp)        (eval-do exp env))

          ((assignment? exp) (handle-assign (list exp) env env))
          ((define-var? exp) (eval-define exp env))

          ((is-keyword? exp) (error
                              'eval-exp "expression is keyword -- ~a" exp))
          (else              (error 'eval-exp "undefined expression -- ~a" exp)))))

;; --------------------------------------------------------------------------
;; Helpers for eval-exp
;; --------------------------------------------------------------------------

(define eval-unary 
  (lambda (exp env)
    (let ((data (eval-helper (unary->color exp) env)))
      (make-rgb (- 255 (rgb->byte1 data))
                (- 255 (rgb->byte2 data))
                (- 255 (rgb->byte3 data))))))

(define eval-2color
  (lambda (exp env)
    (let ((color1 (eval-helper (2color->first-col exp) env))
          (color2 (eval-helper (2color->second-col exp) env)))
      (if (eq? (2color->op exp) '+)
          (make-rgb (+ (rgb->byte1 color1) (rgb->byte1 color2))
                    (+ (rgb->byte2 color1) (rgb->byte2 color2))
                    (+ (rgb->byte3 color1) (rgb->byte3 color2)))
          (make-rgb (- (rgb->byte1 color1) (rgb->byte1 color2))
                    (- (rgb->byte2 color1) (rgb->byte2 color2))
                    (- (rgb->byte3 color1) (rgb->byte3 color2)))))))

(define eval-1color
  (lambda (exp env)
    (let ((color (eval-helper (1color->color exp) env))
          (number (1color->number exp)))
      (if (eq? (1color->op exp) '*)
          (make-rgb (* number (rgb->byte1 color))
                    (* number (rgb->byte2 color))
                    (* number (rgb->byte3 color)))
          (make-rgb (+ (rgb->byte1 color) number)
                    (+ (rgb->byte2 color) number)
                    (+ (rgb->byte3 color) number))))))

(define eval-color-in
  (lambda (exp env)
    (eval-helper (color-in->body exp) env)))
 
(define eval-do-helper              
  (lambda (exp env)
    (eval-helper exp env)))

(define eval-do
  (lambda (exp env)
    (begin (handle-assign (do->assgn exp) env env)  
           (eval-do-helper (do->color exp) env))))  

(define handle-assign
  (lambda (assgns env base-env)
    (if (null? assgns)
        env
        (letrec ((assignment (first assgns))
                 (varref     (assgn->varref assignment))
                 (curr       (assoc varref env))
                 (body       (assgn->color assignment)))
          (begin
            (cell-set! (cdr curr) (eval-helper body base-env)) 
            (handle-assign (rest assgns) env base-env))))))

(define eval-varref                 
  (lambda (exp env)
    (let ((curr-cell (assoc exp env)))
      (if curr-cell
          (cell-value (cdr curr-cell))
          (error 'undefined "~a is undefined in the envronment." exp)))))

(define eval-rgb
  (lambda (exp)
    (make-rgb (rgb->byte1 exp)
              (rgb->byte2 exp)
              (rgb->byte3 exp))))

(define eval-define
  (lambda (exp env)
    (letrec ((new-cell (cell (eval-helper (define->body exp) env)))
             (curr-env (bind (define->varref exp)
                             new-cell
                             env)))
      (begin 
        (write (post-process (eval-helper (define->varref exp) curr-env)))
        curr-env))))


;; -------------------------------------------------------------------------
;; repl
;; -------------------------------------------------------------------------


(define run-huey
  (lambda()
    (display "Enter a Huey expression to evaluate: ")
    (let ((exp (read)))
      (if (color? exp)
          (eval-exp exp)
          (display "Illegal Huey expression.")))
    (newline)
    (run-huey)))

;; -------------------------------------------------------------------------
;; initial environment
;; -------------------------------------------------------------------------

(define white-cell (cell '(rgb 255 255 255)))

(define black-cell (cell '(rgb 0 0 0)))

(define initial-env (bind 'white white-cell
                          (bind 'black black-cell
                                (make-bindings))))


;; -------------------------------------------------------------------------
