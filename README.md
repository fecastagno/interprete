# Interprete de TLC/LISP hecho en Clojure para la materia Programación III, INSPT(Universidad Tecnológica Nacional)
# 
# Planteo del trabajo práctico: TP Prog III.pdf
# 
# Instrucciones:
# 
# 1) Hay que tener instalado Java, se puede descargar de la página de Oracle: https://www.oracle.com/es/java/technologies/javase-downloads.html
# 
# 2) Ejecutar el archivo "repl.bat" o ejecutar desde la consola: "java -jar clojure-1.8.0.jar". 
# Mostrará en la consola: Clojure 1.8.0
#                         user=> _
# 
# 3) Abrir el archivo de Clojure con el comando: (load-file "tlc-lisp.clj"), si no tiene errores imprimirá: true
# 
# 4) Ejecutar el REPL creado con Clojure con el comando: (repl). Si todo va bien, imprimirá en pantalla:
# Interprete de TLC-LISP en Clojure
# Trabajo Practico de Programacion III
# Inspirado en:
# TLC-LISP Version 1.51 for the IBM Personal Computer
# Copyright (c) 1982, 1983, 1984, 1985 The Lisp Company
# >>> 
# 
# 5) (ver archivo TP Prog III.pdf para información detallada). Cargar el archivo jarras con el comando: (load 'jarras), si no tiene errores imprimirá:
# exito
# Jarra8
# 
# 6) Ejecutar el comando "(breadth-first bc)". Pedirá el estado inicial y final de las jarras, hay que pasarle una lista de dos elementos que son las dos jarras.
# 
# 7) Ej: (2 6)
#        (4 4)
# No existe solución
# 
# 8) Ej: (0 0)
#        (4 0)
# Exito !!!
# Profundidad ... 11
# Solucion ... ((0 0) (5 0) (0 5) (5 5) (2 8) (2 0) (0 2) (5 2) (0 7) (5 7) (4 8) (4 0))
