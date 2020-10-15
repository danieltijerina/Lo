Roberto Treviño     A01282035
Daniel Tijerina     A01281870

Avance #2 
Semantica

Cubo semantico

El cubo semantico esta hecho a base de pattern matching. 
Utilizamos el pattern matching de ocaml para ver los patrones validos y dar como resultado assertion en falso a aquiellos que no lo son.


Tablas de variables (VarTabl.ml)

Las tablas de variables son hechas utilizando types y hashtables de ocaml. Creamos un type que contiene la informacion de
el nombre, el tipo de variable y un hashtable de la misma variable keyed con strings (el nombre). Tambien, se tiene un hashtable
global que contiene las estructuras iniciales, como el main, funciones, etc..

Utilizamos hashtables ya que son rapidas en las busquedas y con el nombre como llave es facil buscarlas en ocaml. Una desventaja posible
es que se tiene que hacer cambio de tamano lo que afecta la complejidad.

Aunque esta solucion ya funciona, estamos todavia buscando otras opciones para ver si existe algo diferente. En el archivo de VarTabl se encuentra
una parte del codigo comentado, en donde probamos diferences types para ver cual resultaba mejor.

Si se quiere correr el archivo, se puede utilizar el makefile, y luego correr el commando ./vartabl  .