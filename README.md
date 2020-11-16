# Lo
Object oriented language. Developed using Ocaml.

## Avance actual:

Los procesos de lexing y parsing han sido terminados y probados. El parser genera un arbol sintactico que se consume en el backend, donde se realizan las validaciones semanticas de cada instruccion encontrada en el programa. El analisis semantico tambien ha sido terminado.

Actualmente estamos trabajando en la generacion de codigo intermedio usando cuadruplos. Ya se generan exitosamente los cuadruplos de expresiones, asignaciones, condiciones, loops for, loops while y funciones. Estamos trabajando en la generacion de cuadruplos para clases. Los cuadruplos generados por el compilador (guardados en un archivo .clo) son usados como input para la maquina virutal. Tambien dentro de este archivo incluimos las constantes e informacion de las funciones (cuantos datos utiliza etc..).

Actualmente la maquina virtual apenas esta obteniendo la informacion de los cuadruplos y no hace el procesamiento adecuado. Estamos haciendo esta maquina virtual usando c++ en lugar de ocaml. 