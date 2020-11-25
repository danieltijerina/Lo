# Lo
Object oriented language. Developed using Ocaml y C++;

## Como usar el lenguaje

Para usar el lenguaje, se usan los archivos lo_compile y lo_run que se encuentran en el folder. Se tiene que crear un archivo con terminación .lo y luego se corre de la siguiente manera:

./lo_compile nombreArhico.lo
./lo_run nombreArchivo.clo

Es importante notar lo_compile genera un archivo .clo y ese se tiene que usar para correr el ejecutable de lo_run.

## Compilador

El compilador esta hecho en ocaml utilizando ocamllex y ocamlyacc. Si se quiere volver a crear el ejecutable, se usa el siguiente comando. 

ocamlbuild -use-menhir -r  main.native


## Máquina virtual

La maquina virtual esta hecha en C++ utilizando varios archivos dentro del folder de vm. Para volver a crear el ejecutable, se puede usar el makefile dentro del folder para crearlo.


## Lenguaje

El lenguaje cuenta con las siguientes funcionalidades:
- Declaracion de variables
- Operaciones de variables
- Condicionales y Ciclos
- Funciones
- Recursividad
- Input / Output
- Clases
- Herencia simple de clases
- Arreglos y matrices