proyecto_pokemon
================

Laboratorio de Lenguajes 
Proyecto # 2

Grupo 25
Arocha Juan Carlos        09-10055
Ontiveros Mary Esther   08-10813

  El código entregado contiene todas las especificaciones establecidas en el enunciado del proyecto 
y funciona correctamente. Incluye los conceptos y tipos de datos sugeridos en el enunciado y además 
fueron agregados nuevos tipos de datos que mejoran la interpretación y manejo de la información.  
  Se implementaron funciones de cálculo para las estadísticas y otras ecuaciones suministradas para 
efectos del desarrollo del juego. 
  Para efectos de lectura y apertura de archivos se asume que estos existen. Para el desarrollo del 
flujo de la batalla se asume que el número que acompaña a los comandos  de acción son enteros y que 
los archivos suministrados no contienen líneas en blanco. 
El proyecto se encuentra separado en 4  módulos que contienen lo siguiente:

Módulo Pokemon: En este módulo se encuentran las declaraciones de los tipos de datos utilizados para 
  definir la estructura de un pokemon y sus características. Además se incluyen las instancias de 
  show para estos tipos definidos. 
Módulo Batalla: Contiene las funciones principales de batalla y de verificación de comandos. 
Módulo Parseo: Incluye las funciones utilizadas para generar las listas de especies, ataques y de 
  equipos a partir de la data suministrada. Igualmente contiene los diccionarios de especies y de 
  ataques implementados con el Map de la librería Data de Haskell.
Módulo Main: Contiene la función principal que recibe los nombres de los archivos como argumentos 
  en la línea de comando, los abre y extrae su contenido para el parseo posterior. 

  Además de los módulos, se entrega un makeFile que ejecuta Cabal install para cargar la librería 
Data.Split que no se encuentra dentro del paquete estándar de Haskell.  
        
        
