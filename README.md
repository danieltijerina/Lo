# Lo
Object oriented language. Developed using Ocaml.

## Avance actual:

Los procesos de lexing y parsing han sido terminados y probados. El parser genera un arbol sintactico que se consume en el backend, donde se realizan las validaciones semanticas de cada instruccion encontrada en el programa. El analisis semantico tambien ha sido terminado.

Actualmente estamos trabajando en la generacion de codigo intermedio usando cuadruplos. Ya se generan exitosamente los cuadruplos de expresiones, asignaciones, condiciones, bucles for y bucles while. Estamos trabajando en la generacion de cuadruplos para funciones y clases, asi como sus llamados. Los cuadruplos generados por el compilador (guardados en un archivo .clo) seran ejecutados en una maquina virtual que no hemos empezado.