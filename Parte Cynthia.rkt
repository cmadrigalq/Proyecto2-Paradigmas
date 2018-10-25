;	Manuel Céspedes Osorio 		115940141 
;	Cynthia Madrigal Quesada 	115100465 
;	Manuel Masís Segura 		401730747 
;	Reinaldo Stephens Chaves 	402270666 
;	Paradigmas de programación 
;	II Ciclo 2018

#lang racket
;car devuelve primer elemento, cdr lo elimina
(define L '(1 2 3 4 0 0 0))


(define (member? x list)
     (if (null? list) 0                                ;(1)
         (if (equal? x (car list)) x                   ;(2)
              (member? x (cdr list)))))                ;(3)



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

;[1 2 3] => 1 + 2x + 3x^2    Despliege de polinomios                       
(define printL
    (lambda (L i)
        (
            cond(
                (< i (length L))
                (string-append(printE (list-ref L i) i)(printL L (+ i 1))))
                (else "")
        )
    )
)
(define despliegue
    (lambda (L)
        (printL L 0)
    )
)    
;Meodo para ayudar sumas y restas
; ([1,2],[1,4,5]=> [1,2,0],[1,4,5])
(define emparejar
  (lambda (L1 L2)
    ( cond( (< (length L1)(length L2))
          (emparejar(append L1 '(0)) L2)
      )(else L1)
    )
  )
)

;Método para sumar
;[1 2 3] + [1 2 3] = [2 4 6]
;[1 2 3] + [1 2] => (1 + 2x + 3x^2) + (1 + 2x + 0x^2)
;[1 2 3] + [1 0 3]
;(1 2x 3x^2) + (1 + 0x +3x^2)
(define sumar
  (lambda (L1 L2 i)
    (cond((< i (length L1))
          (cons ( + (list-ref L1 i) (list-ref L2 i)) (sumar L1 L2 (+ i 1)))   
     )(else '())
    )
  )
)

;NO FUNCIONA AUN!!!
(define suma
  (lambda (L);recibe una lista de listas
    (cond((= 1 (length L))
          (simplificar (list-ref L 0))
     )(else(cond(;segunda condicion
            (> 2 (length L))
            (suma (append(suma(cons(car L) (cons (car(cdr L))'()) )) (cddr L)))
       )
       (else (
              simplificar(
                 sumar (car L)(car(car L)) 0
              )
        ))));final de la segunda condicion
    )
  )
)


;****************************PRUEBAS***************************
;(remove-last L)
;(simplificar L)
;(printE 0 9) || (printE 2 5) || (printE 8 0)
;(despliegue L)
;(emparejar '(1) '(1 2 3 4 5))