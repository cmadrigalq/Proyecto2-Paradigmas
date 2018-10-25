;	Manuel Céspedes Osorio 		115940141 
;	Cynthia Madrigal Quesada 	115100465 
;	Manuel Masís Segura 		401730747 
;	Reinaldo Stephens Chaves 	402270666 
;	Paradigmas de programación 
;	II Ciclo 2018

#lang racket

;*******************agregado recientemente*****************
http://www.inf.puc-rio.br/~roberto/icc/texto/icc.html

polinomio de grado n-1
(n × an) xn-1 + ((n-1) × an-1) xn-2 + ... + 2a2 x + a1
      (define calc
         (lambda (exp)
            (cond
               ((number? exp) exp)
               ((equal? (car exp) '+) (+ (calc (cadr exp)) (calc (caddr exp))))
               ((equal? (car exp) '*) (* (calc (cadr exp)) (calc (caddr exp)))))))

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
