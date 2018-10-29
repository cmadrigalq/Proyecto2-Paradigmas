;	Manuel Céspedes Osorio 		115940141 
;	Cynthia Madrigal Quesada 	115100465 
;	Manuel Masís Segura 		401730747 
;	Reinaldo Stephens Chaves 	402270666 
;	Paradigmas de programación 
;	II Ciclo 2018

#lang racket

(define (divisor? x y)
      (= 0 (remainder y x)))

(define (filtra-divisores lista x)
   (cond
      ((null? lista) '())
      ((divisor? (car lista) x) (cons (car lista)
                                      (filtra-divisores (cdr lista) x)))
      (else (filtra-divisores (cdr lista) x))))

(define (divisores x)
   (concatenar (filtra-divisores (lista-hasta x) x) (map - (filtra-divisores (lista-hasta x) x))))

(define (lista-hasta x)
   (if (= x 0)
      '()
      (cons x (lista-hasta (- x 1)))))

(define concatenar
  (lambda (L1 L2)
    (cond ((null? L1) L2)
          ((null? L2) L1)
          (else
           (cons (car L1) (concatenar (cdr L1) L2)))
          )))
#|
(define raiz-encontrada
  (lambda (L1 )
    (cond ((null? L1) L1)
          (else
           (+ (* (car (divisores (first L1))) (last L1)) (cadr (reverse L1)))
                )
          )
    )
  )
|#

;****************************PRUEBAS***************************
;(divisores 6)
