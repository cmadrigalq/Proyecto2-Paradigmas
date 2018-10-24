# Proyecto2-Paradigmas
Polinomios en scheme

Función Descripción

(display-p p) Despliegue de polinomios.
La función mostrará el polinomio utilizando una notación
estándar.
Por ejemplo, la invocación:
(display-p '(1 3 0 -2))
producirá como resultado:
1 + 3x + -2x^3
Observen que el exponente no se muestra cuando vale 1 y el
factor constante, (que corresponde al exponente 0) no despliega
la variable x.

(+p p1 p2 …) Suma de polinomios.
La función recibe una cantidad arbitraria de polinomios en forma
de una lista de coeficientes, los suma y regresa el polinomio
resultante. Si el polinomio resultante es de menor grado que
alguno de los utilizados como argumento de la función, éste debe
simplificarse correctamente.

(-p p1 p2 …) Resta de polinomios.
La función recibe una cantidad arbitraria de polinomios en forma
de una lista de coeficientes y regresa el polinomio resultante. La
semántica de la función corresponde con la función aritmética de
resta. Ya que la operación no es asociativa, los parámetros se
asociarán desde la izquierda, como en la función resta.
Es decir, la operación:
(-p p1 p2 p3)
Debe interpretarse correctamente como:
(- p (-p p1 p2) p3)

(*p p1 p2 …)) Multiplicación de polinomios.
La función recibe una cantidad arbitraria de polinomios en forma
de una lista de coeficientes, calcula el producto y regresa el
polinomio resultante.

(qt-p p1 p2) División de polinomios (parte 1).
Esta función calcula el cociente de la división de los polinomios
recibidos como parámetro. Observe que esta función recibe
solamente 2 parámetros.
(rem-p p1 p2) División de polinomios (parte 2).
Esta función calcula el residuo de la división de los polinomios
recibidos como parámetro. Observe que esta función recibe
solamente 2 parámetros.
(/-p p1 p2) División de polinomios (parte 3).
Esta función calcula el resultado de la división de los polinomios
recibidos como parámetro. El resultado es una lista que contiene
el cociente y el residuo de la división.

(drv-p p1 p2 …) Derivación de polinomios.
La función recibe una cantidad arbitraria de polinomios en forma
de una lista de coeficientes, deriva cada polinomio y regresa una
lista con el resultado.

(eval-p p x) Evaluación de polinomios.
La función evalúa el polinomio p en el valor x. La función no
utilizará ninguna función para elevar números a ninguna
potencia, ya sea una función de biblioteca o definida en el
programa.
Para evaluar el polinomio, se utilizarán multiplicaciones repetidas.
Este método se conoce como el algoritmo de Horner.
https://es.wikipedia.org/wiki/Algoritmo_de_Horner

(fact-p p1) Factorización de polinomios.
Esta función recibe un único polinomio como parámetro y
produce una lista con todos los polinomios que sean factores del
primero.
Aunque un polinomio real puede tener raíces conjugadas (raíces
imaginarias), la función sólo considerará raíces reales.
La función de multiplicación (*p) debe poder aplicarse a la lista
resultante para obtener el polinomio original.
Por ejemplo:
(fact-p '(0 2 0 -2))
producirá como resultado:
'((1 -1) (1 1) (0 2))
Este resultado representa:
(1 − 𝑥)(1 + 𝑥)(2𝑥) = 2𝑥 − 2𝑥 3
El polinomio:
(2 + 𝑥) 2 = 4 + 4𝑥 + 𝑥 2
se escribe:
'(4 4 1)
y se factoriza como:
'((2 1) (2 1))
Observe que, aunque ambos monomios son el mismo, aparecen
repetidos en el resultado.
Los polinomios de grado 2 y 3 pueden factorizarse directamente,
utilizando la fórmula general correspondiente.
https://es.wikipedia.org/wiki/Ecuaci%C3%B3n_de_segundo_grado
https://es.wikipedia.org/wiki/Ecuaci%C3%B3n_de_tercer_grado
