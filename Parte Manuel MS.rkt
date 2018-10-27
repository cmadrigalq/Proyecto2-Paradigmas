;	Manuel Céspedes Osorio 		115940141 
;	Cynthia Madrigal Quesada 	115100465 
;	Manuel Masís Segura 		401730747 
;	Reinaldo Stephens Chaves 	402270666 
;	Paradigmas de programación 
;	II Ciclo 2018

#lang racket

;*******************agregado recientemente*****************
; http://www.inf.puc-rio.br/~roberto/icc/texto/icc.html

;polinomio de grado n-1
(n × an) xn-1 + ((n-1) × an-1) xn-2 + ... + 2a2 x + a1
      (define calc
         (lambda (exp)
            (cond
               ((number? exp) exp)
               ((equal? (car exp) '+) (+ (calc (cadr exp)) (calc (caddr exp))))
               ((equal? (car exp) '*) (* (calc (cadr exp)) (calc (caddr exp)))))))

;*******************agregado recientemente******************

; Funcion para evaluar polinomios

(define (horner x l) 
    (foldr (lambda (a b) (+ a (* b x))) 0 l))

; **************************************************************
; Esta es otra implementación de la funcion

(define (evalPol x l) 
    (doblar (lambda (a b) (+ a (* b x))) 0 l))

; Funcion foldr implementada

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

; *****************************Evaluacion de la funcion*********************************

(evalPol 3 '(-19 7 -4 6))
(horner 3 '(-19 7 -4 6))

;*******************agregado recientemente******************

; Implementacion de la funcion para dividir polinomios
; Esta funcion genera una lista con los valores del residuo y otra lista con los valores del cociente

(define (grado p) 
  (for/fold ([d -inf.0]) ([(pi i) (in-indexed p)])
    (if (zero? pi) d i)))
(define (lead p) (vector-ref p (grado p)))
(define (mono c d) (build-vector (+ d 1) (λ(i) (if (= i d) c 0))))
(define (divPol*cx^n c n p) (vector-append (make-vector n 0) (for/vector ([pi p]) (* c pi))))
(define (divPol+ p q) (divPol/lin 1 p  1 q))
(define (divPol- p q) (divPol/lin 1 p -1 q))
(define (divPol/lin a p b q)
  (cond [(< (grado p) 0) q] 
        [(< (grado q) 0) p]
        [(< (grado p) (grado q)) (divPol/lin b q a p)]
        [else (define ap+bq (for/vector #:length (+ (grado p) 1) #:fill 0
                              ([pi p] [qi q]) (+ (* a pi) (* b qi))))
              (for ([i (in-range (+ (grado q) 1) (+ (grado p) 1))])
                (vector-set! ap+bq i (* a (vector-ref p i))))
              ap+bq]))
 
(define (divPol/ n d)
  (define N (grado n))
  (define D (grado d))
  (cond
    [(< N 0) (error 'divPol/ "No puede dividir entre 0")]
    [(< N D) (values 0 n)]
    [else    (define c (/ (lead n) (lead d)))
             (define q (mono c (- N D)))
             (define r (divPol- n (divPol*cx^n c (- N D) d)))
             (define-values (q1 r1) (divPol/ r d))
             (values (divPol+ q q1) r1)]))

; *****************************Evaluacion de la funcion*********************************

(divPol/ #(-42 0 -12 1) #(-3 1))

;*******************agregado recientemente******************

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
