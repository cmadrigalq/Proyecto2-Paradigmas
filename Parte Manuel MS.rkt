;	Manuel Céspedes Osorio 		115940141 
;	Cynthia Madrigal Quesada 	115100465 
;	Manuel Masís Segura 		401730747 
;	Reinaldo Stephens Chaves 	402270666 
;	Paradigmas de programación 
;	II Ciclo 2018

#lang racket

(require racket/list)

;***************************************** agregado recientemente *****************************************
; http://www.inf.puc-rio.br/~roberto/icc/texto/icc.html

;polinomio de grado n-1
(n × an) xn-1 + ((n-1) × an-1) xn-2 + ... + 2a2 x + a1
      (define calc
         (lambda (exp)
            (cond
               ((number? exp) exp)
               ((equal? (car exp) '+) (+ (calc (cadr exp)) (calc (caddr exp))))
               ((equal? (car exp) '*) (* (calc (cadr exp)) (calc (caddr exp)))))))

;******************************************* agregado recientemente *******************************************

;*************************************** Funcion para evaluar polinomios **************************************

(define (horner x l) 
    (foldr (lambda (a b) (+ a (* b x))) 0 l))

;********************************* Esta es otra implementación de la funcion **********************************

(define (evalPol x l) 
    (doblar (lambda (a b) (+ a (* b x))) 0 l))

; Funcion foldr implementada, la cual es usada por la funcion anterior

(define (doblar funcion final lista)
  (if (null? lista)
      final
      (funcion (car lista) (foldr funcion final (cdr lista)))))

; Otra implementación para la funcion foldr
(define (my-foldr proc init lst)
  (cond
    [(empty? lst) init]
    [else
     (proc (first lst)
           (my-foldr proc
                     init
                     (rest lst)))]))

;******************************************* Evaluacion de la funcion *******************************************

(evalPol 3 '(-19 7 -4 6))
(horner 3 '(-19 7 -4 6))

;*****************************************************************************************************************

; Implementacion de la funcion para dividir polinomios

;*************************************** Division de polinomios - Parte 1 ****************************************
;(require racket/list)

(define (qt-p p1 p2)
  (define out (list->vector p1))
  (define normaliser (car p2))
  (define p2-length (length p2))
  (define out-length (vector-length out))
  (for ((i (in-range 0 (- out-length p2-length -1))))
    (vector-set! out i (quotient (vector-ref out i) normaliser))
    (define coef (vector-ref out i))
    (unless (zero? coef)
      (for ((i+j (in-range (+ i 1)
                           (+ i p2-length)))
            (p2_j (in-list (cdr p2))))
        (vector-set! out i+j (+ (vector-ref out i+j) (* coef p2_j -1))))))
  (split-at (vector->list out) (- out-length (sub1 p2-length))))



;************************************* Evaluacion de la funcion - Parte 1 *************************************

(module+ main
  (displayln "Division de polinomios - Parte 1")
  (define N '(1 -12 0 -42))
  (define D '(1 -3))
  (define-values (Q R) (qt-p N D))
  (printf "El cociente de ~a / ~a es ~a~%" N D Q))

;*************************************** Division de polinomios - Parte 2 ***************************************
;(require racket/list)

(define (rem-p p1 p2)
  (define out (list->vector p1))
  (define normaliser (car p2))
  (define p2-length (length p2))
  (define out-length (vector-length out))
  (for ((i (in-range 0 (- out-length p2-length -1))))
    (vector-set! out i (quotient (vector-ref out i) normaliser))
    (define coef (vector-ref out i))
    (unless (zero? coef)
      (for ((i+j (in-range (+ i 1)
                           (+ i p2-length)))
            (p2_j (in-list (cdr p2))))
        (vector-set! out i+j (+ (vector-ref out i+j) (* coef p2_j -1))))))
  (split-at (vector->list out) (- out-length (sub1 p2-length))))

;**************************************** Evaluacion de la funcion - Parte 2 ******************************************

(module+ main
  (displayln "Division de polinomios - Parte 1")
  (define N '(1 -12 0 -42))
  (define D '(1 -3))
  (define-values (Q R) (qt-p N D))
  (printf "El residuo de ~a / ~a es ~a~%" N D R))

;****************************************** Division de polinomios - Parte 3 ******************************************
;(require racket/list)

(define (/-p p1 p2)
  (define out (list->vector p1))
  (define normaliser (car p2))
  (define p2-length (length p2))
  (define out-length (vector-length out))
  (for ((i (in-range 0 (- out-length p2-length -1))))
    (vector-set! out i (quotient (vector-ref out i) normaliser))
    (define coef (vector-ref out i))
    (unless (zero? coef)
      (for ((i+j (in-range (+ i 1)
                           (+ i p2-length)))
            (p2_j (in-list (cdr p2))))
        (vector-set! out i+j (+ (vector-ref out i+j) (* coef p2_j -1))))))
  (split-at (vector->list out) (- out-length (sub1 p2-length))))
  
;************************************* Evaluacion de la funcion - Parte 3 *************************************

(module+ main
  (displayln "Division de polinomios - Parte 3")
  (define N '(1 -12 0 -42))
  (define D '(1 -3))
  (define-values (Q R) (/-p N D))
  (printf "(~a ~a)" Q R))
  
;******************************************* agregado recientemente *******************************************

;car devuelve primer elemento, cdr lo elimina
(define L '(1 2 3 4 0 0 0))

;Elimina el ultimo elemento de la lista
(define remove-last
  (lambda(lst)
    (if (null? (cdr lst)) 
     '() 
     (cons (car lst) (remove-last (cdr lst))))))           

;[1 2 3 4 0 0 0] => [1 2 3 4]
(define simplificar
  (lambda(Lst)
    (cond ( (zero?(list-ref Lst (- (length Lst) 1)) ) (simplificar(remove-last Lst)) )
          (else Lst)
    )
   ))
   
;(e,i) => ex^i
(define printE
  (lambda (e i)
    (
      cond( (zero? e) "")
         (else (
             cond((zero? i)(number->string e))
             (else (
                    string-append (cond((> e 0 ) "+" )(else ""))
                                  (cond((= i 1)
                                       (string-append (number->string e) "x"))
                                       (else (string-append (number->string e) (string-append "x^" (number->string i)))))
             ))    
         ))
    )
    )
  )

;****************************PRUEBAS***************************
;(remove-last L)
;(simplificar L)
;(printE 0 9) || (printE 2 5) || (printE 8 0)
