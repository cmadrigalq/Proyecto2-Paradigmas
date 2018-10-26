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
        (display(printL L 0))
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
     ) (else '())
    )
  )
)


(define sumapolinomios
  (lambda (L1);recibe una lista de listas
    (cond ( (> (length L1) 2)
          (sumapolinomios (cons(sumapolinomios(cons(car L1) (cons (car(cdr L1))'()) )) (cddr L1)));THEN
      )(else (
              simplificar(
                  sumar (emparejar(car L1)(list-ref L1 1)) (emparejar(list-ref L1 1)(car L1)) 0
              )
             )
        )
    )
  )
)
(define suma
   (lambda (L)
       (despliegue (sumapolinomios L))
    )
)
;RESTAR
;[1 2 3] - [1 2 3] = [0 0 0]
;[1 2 3] - [1 2] => (1 + 2x + 3x^2) - (1 + 2x + 0x^2)
;[1 2 3] - [1 0 3]
;(1 2x 3x^2) - (1 + 0x +3x^2)
(define restar
  (lambda (L1 L2 i)
    (cond((< i (length L1))
          (cons ( - (list-ref L1 i) (list-ref L2 i)) (restar L1 L2 (+ i 1)))   
     ) (else '())
    )
  )
)
(define restapolinomios
  (lambda (L1);recibe una lista de listas
    (cond ( (> (length L1) 2)
          (restapolinomios (cons(restapolinomios(cons(car L1) (cons (car(cdr L1))'()) )) (cddr L1)));THEN
      )(else (
              simplificar(
                  restar (emparejar(car L1)(list-ref L1 1)) (emparejar(list-ref L1 1)(car L1)) 0
              )
             )
        )
    )
  )
)
(define resta
   (lambda (L)
       (despliegue (restapolinomios L))
    )
)

;DERIVAR
(define derivarSubLista
  (lambda (subL index)
      (cond( (< index (length subL))
             (append (cons(* index (list-ref subL index)) '() ) (derivarSubLista subL (+ 1 index)))
       )(else '())
      )
   )
)
(define derivarPolinomios
   (lambda(L index)
       (cond( (< index(length L))
              (despliegue(simplificar(derivarSubLista(list-ref L index) 1)))
              (newline)
              (derivarPolinomios (cdr L) 0)       )
      )
    )
)
(define derivar
 (lambda (L)
     (derivarPolinomios L 0)
  )
)



;****************************PRUEBAS***************************
;(remove-last L)
;(simplificar L)
;(printE 0 9) || (printE 2 5) || (printE 8 0)
;(despliegue L)
;(emparejar '(1) '(1 2 3 4 5))
;(suma '((1 2 3) (-1 6 5 9 7) (-2 3) (1) ))
;(resta '((1 2 3) (-1 6 5 9 7) (-2 3) (1) ))
;(derivar '((1 2 3) (3 4 5 6 7) (5555 7 8 6) ))