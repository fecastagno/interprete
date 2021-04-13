(declare evaluar)											;done
(declare aplicar)											;done
(declare controlar-aridad)						;done
(declare igual?) 											;done
(declare cargar-arch)									;done
(declare imprimir)										;done
(declare actualizar-amb) 							;done
(declare revisar-f)										;done
(declare revisar-lae)									;done
(declare buscar) 											;done
(declare evaluar-cond)								;done
(declare evaluar-secuencia-en-cond)		;done

; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra >>> y lee una expresion y la evalua.
; Si la 2da. posicion del resultado es nil, retorna true (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado. 
(defn repl
   ([]
      (println "Interprete de TLC-LISP en Clojure")
	  	(println "Trabajo Practico de Programacion III")
	  	(println "Inspirado en:")
      (println "TLC-LISP Version 1.51 for the IBM Personal Computer")
      (println "Copyright (c) 1982, 1983, 1984, 1985 The Lisp Company") (flush)
      (repl '(add add append append cond cond cons cons de de env env equal equal eval eval exit exit
 			  first first ge ge gt gt if if lambda lambda length length list list load load lt lt nil nil not not
 			  null null or or prin3 prin3 quote quote read read rest rest reverse reverse setq setq sub sub
 			  t t terpri terpri + add - sub)))
   ([amb]  
      (print ">>> ") (flush)
      (try (let [res (evaluar (read) amb nil)]
	            (if (nil? (fnext res))
				    true
						(do (imprimir (first res)) (repl (fnext res)))))
           (catch Exception e (println) (print "*error* ") (println (get (Throwable->map e) :cause)) (repl amb))))
)

; Carga el contenido de un archivo.
; Aridad 3: Recibe los ambientes global y local y el nombre de un archivo
; (literal como string o atomo, con o sin extension .lsp, o el simbolo ligado al nombre de un archivo en el ambiente), abre el archivo 
; y lee un elemento de la entrada (si falla, imprime nil), lo evalua y llama recursivamente con el (nuevo?) amb., nil, la entrada y un arg. mas: el resultado de la evaluacion.
; Aridad 4: lee un elem. del archivo (si falla, imprime el ultimo resultado), lo evalua y llama recursivamente con el (nuevo?) amb., nil, la entrada y el resultado de la eval.
(defn cargar-arch
  ([amb-global amb-local arch]
    (let [nomb (first (evaluar arch amb-global amb-local))]
      (if (and (seq? nomb) (igual? (first nomb) '*error*))
	    	(do (imprimir nomb) amb-global) 
        (let [nm (clojure.string/lower-case (str nomb)),
              nom (if (and (> (count nm) 4)(clojure.string/ends-with? nm ".lsp")) nm (str nm ".lsp")),
              ret (try (with-open [in (java.io.PushbackReader. (clojure.java.io/reader nom))]
                             (binding [*read-eval* false] (try (let [res (evaluar (read in) amb-global nil)]
							                                        (cargar-arch (fnext res) nil in res))
	                                                           (catch Exception e (imprimir nil) amb-global))))
			  	       (catch java.io.FileNotFoundException e (imprimir (list '*error* 'file-open-error 'file-not-found nom '1 'READ)) amb-global))]
  		     ret))))
  ([amb-global amb-local in res]
    (try (let [res (evaluar (read in) amb-global nil)] (cargar-arch (fnext res) nil in res))
         (catch Exception e (imprimir (first res)) amb-global)))
)

; Evalua una expresion usando los ambientes global y local. 
; Siempre retorna una lista con un resultado y un ambiente.
; Si la evaluacion falla, el resultado es una lista con '*error* como primer elemento, por ejemplo: (list '*error* 'too-many-args) y el ambiente es el ambiente global.
; Si la expresion es un escalar numero o cadena, retorna la expresion y el ambiente global.
; Si la expresion es otro tipo de escalar, la busca (en los ambientes local y global) y retorna el valor y el ambiente global.
; Si la expresion es una secuencia nula, retorna nil y el ambiente global.
; Si el primer elemento de la expresion es '*error*, retorna la expresion y el ambiente global.
; Si el primer elemento de la expresion es una forma especial o una macro, valida los demas elementos y retorna el resultado y el (nuevo?) ambiente.
; Si no lo es, se trata de una funcion en posicion de operador (es una aplicacion de calculo lambda), por lo que se llama a la funcion aplicar,
; pasandole 4 argumentos: la evaluacion del primer elemento, una lista con las evaluaciones de los demas, el ambiente global y el ambiente local. 
(defn evaluar [expre amb-global amb-local]
	(if (not (seq? expre))
		(if (or (number? expre) (string? expre)) (list expre amb-global) (list (buscar expre (concat amb-local amb-global)) amb-global))
			(cond (igual? expre nil) (list nil amb-global)
			      (igual? (first expre) '*error*) (list expre amb-global)
			      (igual? (first expre) 'exit) (if (< (count (next expre)) 1) (list nil nil) (list (list '*error* 'too-many-args) amb-global))
		        (igual? (first expre) 'setq) 
			        (do
			        	;(prn expre)
				        (cond (< (count (next expre)) 2) (list (list '*error* 'list 'expected nil) amb-global)
							    (igual? (fnext expre) nil) (list (list '*error* 'cannot-set nil) amb-global)
									(not (symbol? (fnext expre))) (list (list '*error* 'symbol 'expected (fnext expre)) amb-global)
									(= (count (next expre)) 2) (let [res (evaluar (first (nnext expre)) amb-global amb-local)]
									                             (do 
									                             		;(prn (first res))
									                             		(let [nuevoAmb (actualizar-amb amb-global (fnext expre) (first res))]
									                             				 (do 
									                             				 		;(prn nuevoAmb)
									                             				 		(list (first res) nuevoAmb)
									                             				 )
									                             		)
									                             )
								                             )
									true (let [res (evaluar (first (nnext expre)) amb-global amb-local)]
			                 (evaluar (cons 'setq (next (nnext expre))) (actualizar-amb amb-global (fnext expre) (first res)) amb-local))
								)
							)

						(igual? (first expre) 'de) 
							(cond (< (count (next expre)) 2) (list (list '*error* 'list 'expected nil) amb-global)
										(and (not (igual? (first (nnext expre)) nil)) (not (seq? (first (nnext expre))))) (list (list '*error* 'list 'expected (first (nnext expre))) amb-global)
				            (igual? (fnext expre) nil) (list (list '*error* 'cannot-set nil) amb-global)
				            (not (symbol? (fnext expre))) (list (list '*error* 'symbol 'expected (fnext expre)) amb-global)
										true (list (fnext expre) (actualizar-amb amb-global (fnext expre) (cons 'lambda (nnext expre))))
							)
					  (igual? (first expre) 'quote) (list (if (igual? (fnext expre) nil) nil (fnext expre)) amb-global)
					  (igual? (first expre) 'lambda) 
					  	(cond (< (count (next expre)) 1) (list (list '*error* 'list 'expected nil) amb-global)
										(and (not (igual? (fnext expre) nil)) (not (seq? (fnext expre)))) (list (list '*error* 'list 'expected (fnext expre)) amb-global)
										true (list expre amb-global)
							)
	   			  (igual? (first expre) 'cond) (evaluar-cond (next expre) amb-global amb-local)

						; if: forma especial (evalúa una condición)
	   			  (igual? (first expre) 'if) (if (nil? (next expre)) 
	   			  															 (list (list '*error* 'list 'expected nil) amb-global)
	   			  															 (if (nil? (first (evaluar (second expre) amb-global amb-local)))
	   			  															 		 (evaluar (first (drop 3 expre)) amb-global amb-local)
	   			  															 		 (evaluar (first (drop 2 expre)) amb-global amb-local)
	   			  															 ) 
	   			  													 )

						; load: carga un archivo									
	   			  (igual? (first expre) 'load) (cond 
	   			  																(< (count (next expre)) 1) (list (list '*error* 'too-few-args2) amb-global)
                                         		(> (count (next expre)) 1) (list (list '*error* 'not-implemented) amb-global)
                                            true (list \space (cargar-arch amb-global amb-local (fnext expre)))
                                         )

						; or: macro (evalúa mientras no obtenga t)
	   			  (igual? (first expre) 'or) (if (nil? (next expre))
                                           (list nil amb-global)
                                           (let [res-eval (evaluar (fnext expre) amb-global amb-local)]
                                              (if (not (igual? (first res-eval) nil))
                                                res-eval
                                                (recur (cons 'or (nnext expre)) (fnext res-eval) amb-local)
                                              )
                                           )
                                       )


				  	true (aplicar (first (evaluar (first expre) amb-global amb-local)) (map (fn [x] (first (evaluar x amb-global amb-local))) (next expre)) amb-global amb-local)
			)
	)
	; Todas formas especiales y macros
	; cond: macro (evalúa múltiples condiciones)
	; de: macro (define función y la liga a símbolo)
	; exit: sale del intérprete
	; if: forma especial (evalúa una condición)
	; lambda: macro (define una func. anónima)
	; load: carga un archivo
	; or: macro (evalúa mientras no obtenga t)
	; quote: forma especial (impide evaluación)
	; setq: forma especial (liga símbolo a valor)

)

; Aplica una funcion a una lista de argumentos evaluados, usando los ambientes global y local. Siempre retorna una lista con un resultado y un ambiente.
; Si la aplicacion falla, el resultado es una lista con '*error* como primer elemento, por ejemplo: (list '*error* 'arg-wrong-type) y el ambiente es el ambiente global.
; Aridad 4: Recibe la func., la lista de args. evaluados y los ambs. global y local. Se llama recursivamente agregando 2 args.: la func. revisada y la lista de args. revisada.
; Aridad 6: Si la funcion revisada no es nil, se la retorna con el amb. global.
; Si la lista de args. evaluados revisada no es nil, se la retorna con el amb. global.
; Si no, en caso de que la func. sea escalar (predefinida o definida por el usuario), se devuelven el resultado de su aplicacion (controlando la aridad) y el ambiente global.
; Si la func. no es escalar, se valida que la cantidad de parametros y argumentos coincidan, y:
; en caso de que se trate de una func. lambda con un solo cuerpo, se la evalua usando el amb. global intacto y el local actualizado con los params. ligados a los args.,  
; en caso de haber multiples cuerpos, se llama a aplicar recursivamente, pasando la funcion lambda sin el primer cuerpo, la lista de argumentos evaluados,
; el amb. global actualizado con la eval. del 1er. cuerpo (usando el amb. global intacto y el local actualizado con los params. ligados a los args.) y el amb. local intacto. 
(defn aplicar
		([f lae amb-global amb-local]
      (aplicar (revisar-f f) (revisar-lae lae) f lae amb-global amb-local))
		([resu1 resu2 f lae amb-global amb-local]
    	(cond resu1 (list resu1 amb-global)
		    resu2 (list resu2 amb-global)
		    true (if (not (seq? f))
		              (list (cond
								   				;cons: retorna inserción de elem. en cabeza de lista
													(igual? f 'cons)  (let [ari (controlar-aridad lae 2)]
												                      (cond 
												                      	(seq? ari) ari
																				        (and (not (nil? (second lae))) (not (seq? (second lae)))) (list '*error* 'not-implemented)
																                true (cons (first lae) (second  lae))
															                )
															              )	

	  			                (igual? f 'env) (if (> (count lae) 0)
								                    				(list '*error* 'too-many-args)
																						(concat amb-global amb-local)
																					)
													(igual? f 'first) (let [ari (controlar-aridad lae 1)]
													                      (cond (seq? ari) ari
																					        (igual? (first lae) nil) nil
																					        (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
																	                true (ffirst lae)
																                )
																            )
													(igual? f 'add) (if (< (count lae) 2)
												                    (list '*error* 'too-few-args3)
												                    (try (reduce + lae) 
																		     			(catch Exception e (list '*error* 'number-expected))
																		     		)
												                  )
													; append: retorna la fusión de dos listas
													(igual? f 'append)  (let [ari (controlar-aridad lae 2)]
													                      (cond 
													                      	(seq? ari) ari
																					        (and (not (igual? nil (first lae))) (not (seq? (first lae)))) (list '*error* 'list 'expected (first lae))
																					        (and (not (igual? nil (second lae))) (not (seq? (second lae)))) (list '*error* 'not-implemented)
																	                true (let [res (concat (first lae) (second lae))]
																		                			(if (empty? res)
																		                					nil
																		                					res
																		                			)
																	                		 )
																                )
																              )

													; equal: retorna t si dos elementos son iguales
													(igual? f 'equal) (let [ari (controlar-aridad lae 2)]
																							(cond
													                      (seq? ari) ari
																								(igual? (first lae) (second lae)) 't
																							)
																						)

													; eval: retorna la evaluación de una lista
													(igual? f 'eval) (let [ari (controlar-aridad lae 1)]
																							(cond
													                      (seq? ari) ari
																								true (first (evaluar (first lae) amb-global amb-local))
																							)
																					 )

													; ge: retorna t si el 1° núm. es mayor o igual que 2°
													(igual? f 'ge) (let [ari (controlar-aridad lae 2)]
																							(cond
													                      (seq? ari) ari
																								(>= (first lae) (second lae)) 't
																							)
																						)

													; gt: retorna t si el 1° núm. es mayor que el 2°
													(igual? f 'gt) (let [ari (controlar-aridad lae 2)]
																							(cond
													                      (seq? ari) ari
																								(> (first lae) (second lae)) 't
																							)
																						)

													; lt: retorna t si el 1° núm. es menor que el 2°
													(igual? f 'lt) (let [ari (controlar-aridad lae 2)]
																						(cond
												                      (seq? ari) ari
																							(< (first lae) (second lae)) 't
																						)
																					)												

													; length: retorna la longitud de una lista
													(igual? f 'length)  (let [ari (controlar-aridad lae 1)]
																								(cond
														                      (seq? ari) ari
														                      (not (or (string? (first lae)) (seq? (first lae)))) (list '*error* 'arg-wrong-type (first lae))
																									true (count (first lae))
																								)
																							)	

													; list: retorna una lista formada por los args.
													(igual? f 'list)  (if (< (count lae) 1)
													                    nil
													                    lae																			     	
													                  )	

										 			; not: retorna la negación de un valor de verdad	
													(igual? f 'not) (let [ari (controlar-aridad lae 1)]
											                      (cond 
											                      	(seq? ari) ari
											                      	(igual? (first lae) 't) nil
															                true 't
														                )
															            )	

													; null: retorna t si un elemento es nil	
													(igual? f 'null) 	(let [ari (controlar-aridad lae 1)]
																							(cond
													                      (seq? ari) ari
																								(nil? (first lae)) 't
																							)
																						)	

													; prin3: imprime un elemento y lo retorna				
													(igual? f 'prin3) (let [ari (count lae)]
																						  (case ari
																						    0 (list '*error* 'too-few-args4)
																						    1 (do (print (first lae)) (flush) (first lae))
																						    2 (list '*error* 'not-implemented)
																						    (list '*error* 'too-many-args)
																							)
																						)

													; rest: retorna una lista sin su 1ra. posición		
													(igual? f 'rest) (let [ari (controlar-aridad lae 1)]
													                      (cond (seq? ari) ari
																					        (igual? (first lae) nil) nil
																					        (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
																	                true (next (first lae))
																                )
																            )

				              		; read: retorna la lectura de un elemento	
													(igual? f 'read) (if (> (count lae) 0) 
																							(list '*error* 'not-implemented)
																							(read)																						 	
																					 )

													; reverse: retorna una lista invertida			
													(igual? f 'reverse) (let [ari (controlar-aridad lae 1)]
													                      (cond (seq? ari) ari
																					        (igual? (first lae) nil) nil
																					        (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
																	                true (reverse (first lae))
																                )
																	            )

													; sub: retorna la resta de los argumentos
													(igual? f 'sub) (if (< (count lae) 2)
												                    (list '*error* 'too-few-args5)
												                    (try (reduce - lae) 
																		     			(catch Exception e (list '*error* 'number-expected))
																		     		)
												                  )

													; terpri: imprime un salto de línea y retorna nil
													(igual? f 'terpri)(if (> (count lae) 0) 
																							(list '*error* 'not-implemented)
																							(do (println) (flush) nil)								 	
																					 	)


													true (let [lamb (buscar f (concat amb-local amb-global))]
															 	 (cond (or (number? lamb) (igual? lamb 't) (igual? lamb nil)) (list '*error* 'non-applicable-type lamb)
	                                  (or (number? f) (igual? f 't) (igual? f nil)) (list '*error* 'non-applicable-type3 f);;;;;error
	                                  (igual? (first lamb) '*error*) lamb
 	                                  true (aplicar lamb lae amb-global amb-local)
		 	                           )
														   )
												) 
		              amb-global)

									(cond (< (count lae) (count (fnext f))) (list (list '*error* 'too-few-args6) amb-global)
							    	(> (count lae) (count (fnext f))) (list (list '*error* 'too-many-args) amb-global)
							      true (if (nil? (next (nnext f)))
						               (evaluar (first (nnext f)) amb-global (concat (reduce concat (map list (fnext f) lae)) amb-local))
							           	 (aplicar (cons 'lambda (cons (fnext f) (next (nnext f)))) lae (fnext (evaluar (first (nnext f)) amb-global (concat (reduce concat (map list (fnext f) lae)) amb-local))) amb-local)
						           	 )
						      )
							)
			)
		)

		; Total Funciones
		; add: retorna la suma de los argumentos
		; append: retorna la fusión de dos listas
		; cons: retorna inserción de elem. en cabeza de lista
		; env: retorna el ambiente
		; equal: retorna t si dos elementos son iguales
		; eval: retorna la evaluación de una lista
		; first: retorna la 1ra. posición de una lista
		; ge: retorna t si el 1° núm. es mayor o igual que 2°
		; gt: retorna t si el 1° núm. es mayor que el 2°
		; length: retorna la longitud de una lista
		; list: retorna una lista formada por los args.
		; lt: retorna t si el 1° núm. es menor que el 2°
		; not: retorna la negación de un valor de verdad
		; null: retorna t si un elemento es nil
		; prin3: imprime un elemento y lo retorna
		; read: retorna la lectura de un elemento
		; rest: retorna una lista sin su 1ra. posición
		; reverse: retorna una lista invertida
		; sub: retorna la resta de los argumentos
		; terpri: imprime un salto de línea y retorna nil
		; +: equivale a add
		; -: equivale a sub
)

;------------------------------------------------------------------------------------------
; Actualiza un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] 
; y valores en las pares [2, 4, 6...]
; Recibe el ambiente, la clave y el valor.
; Si el valor no es escalar y en su primera posicion contiene '*error*,
; retorna el ambiente intacto.
; Si no, coloca la clave y el valor en el ambiente 
; (puede ser un alta o una actualizacion) y lo retorna
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (defn actualizar-amb [amb clave valor]
; 	(if (list? valor)
; 		; lista
; 		(if (= '*error* (first valor)) ;contiene *error*?
; 			amb
; 		)
; 		; else escalar:
; 		(if
; 			(pos? (.indexOf amb clave)) ; si existe
; 			(concat (take (inc (.indexOf amb clave)) amb) (list valor) (drop (+ 2 (.indexOf amb clave)) amb)) ;modifica
; 			; si no existe, agregarlo al final
; 			; else
; 			(concat amb (list clave valor))
; 		)
; 	)
; )
; 
; 
; (a 1 b 2 c 3)
; (actualizar-amb '(a 1 b 2 c 3) 'b 4)

(defn actualizar-amb 
	([amb clave valor] (actualizar-amb amb clave valor '()))
	([amb clave valor nuevoAmb] 
		(if (empty? amb)
				(concat nuevoAmb (list clave valor))
				(if (igual? clave (first amb))
						(concat nuevoAmb (list clave valor) (nnext amb))
						(recur (nnext amb) clave valor (concat nuevoAmb (list (first amb) (second amb))))
				)
		)
	)
)

; Ejemplos
; (actualizar-amb '(+ add - sub) 'x 1) ;=> (+ add - sub x 1)
; (actualizar-amb '(+ add - sub x 1 y 2) 'x 3) ;=> (+ add - sub x 3 y 2)

;------------------------------------------------------------------------------------------
; Controla la aridad (cantidad de argumentos de una funcion).
; Recibe una lista y un numero. Si la longitud de la lista coincide con el numero, retorna el numero.
; Si es menor, retorna (list '*error* 'too-few-args).
; Si es mayor, retorna (list '*error* 'too-many-args).
(defn controlar-aridad [lis num]
	(def len (count lis))
	(if 
		(= num len)
		num
		; si es menor
		(if
			(< len num)
			(list '*error* 'too-few-args7)
			(list '*error* 'too-many-args)
		)
	)
)

; EJEMPLOS
; (controlar-aridad '(a b c) 4) ;=> (*error* too-few-args)
; (controlar-aridad '(a b c d) 4) ;=> 4
; (controlar-aridad '(a b c d e) 4) ;=> (*error* too-many-args)

;------------------------------------------------------------------------------------------
; Imprime, con salto de linea, atomos o listas en formato estandar (las cadenas con comillas) 
; y devuelve su valor. Muestra errores sin parentesis.
; Aridad 1: Si recibe un escalar, lo imprime con salto de linea en formato estandar 
; (pero si es \space no lo imprime), purga la salida y devuelve el escalar.
; Si recibe una secuencia cuyo primer elemento es '*error*, se llama recursivamente
; con dos argumentos iguales: la secuencia recibida.
; Si no, imprime lo recibido con salto de linea en formato estandar, 
; purga la salida y devuelve la cadena.
; Aridad 2: Si el primer parametro es nil, imprime un salto de linea, 
; purga la salida y devuelve el segundo parametro.

; Si no, imprime su primer elemento en formato estandar, 
; imprime un espacio y se llama recursivamente con la cola
; del primer parametro y el segundo intacto.
(defn imprimir
	([elem] 
		(cond 
			(not (list? elem))
				;escalar
				(do
					(if
						(not= '\space elem)				
						(prn elem) ;lo imprime con salto de linea en formato estandar 
					) 				
					(flush)
					elem
				)			

			(= '*error* (first elem)) ;lista con *error* al comienzo			
				(imprimir elem elem)
			
			:default	
				(do
					(prn elem) (flush)
					elem 
				)
		)
	)
	([lis orig]
		(if
			(nil? lis)
			;true
			(do
				(println) (flush)
				orig
			)		
			(do 
				(pr (first lis))
				(print " ")
				(imprimir (next lis) orig)
			)
		)
	)
)

; EJEMPLOS
; (imprimir "hola") 
; ;=> "hola" 
; ;=> "hola"
; (imprimir 5) 
; ;=> 5 
; ;=> 5
; (imprimir 'a) 
; ;=> a 
; ;=> a
; (imprimir \space) ;=> \space
; (imprimir '(hola "mundo")) 
; ;=> (hola "mundo")
; ;=> (hola "mundo")
; (imprimir '(*error* hola "mundo")) 
;=> *error* hola "mundo"
;=> (*error* hola "mundo")

;------------------------------------------------------------------------------------------
; Busca una clave en un ambiente 
; (una lista con claves en las posiciones impares [1, 3, 5...] 
; y valores en las pares [2, 4, 6...] y retorna el valor asociado.
; Si no la encuentra, retorna una lista con 
; '*error* en la 1ra. pos., 'unbound-symbol en la 2da. y el elemento en la 3ra.
; (defn buscar [elem lis] 
; 	(if
; 		(pos? (.indexOf lis elem)) ;existe en la lista?
; 		(second (next (.indexOf lis elem))) ;retornar siguiente
; 		(list '*error* 'unbound-symbol elem) ;imprimir error
; 	)
; )
(defn buscar [elem lis]
	; (let [num (.indexOf (map first (partition 2 lis)) elem)] ;ACE
	; 	(if (not (neg? num))
	; 			(nth lis (inc (* 2 num)))
	; 			(list '*error* 'unbound-symbol elem)
	; 	)
	; )
	(cond (empty? lis) (list '*error* 'unbound-symbol elem)
				(igual? elem (first lis)) (second lis)
				true (recur elem (nnext lis))
	)
)

; EJEMPLOS:
; (buscar '- '(+ add - sub)) ;=> sub
; (buscar 'doble '(+ add - sub)) ;=> (*error* unbound-symbol doble)
;------------------------------------------------------------------------------------------
; Verifica la igualdad de dos simbolos.
; Recibe dos simbolos a y b. Retorna true si se deben considerar iguales; si no, false.
; Se utiliza porque TLC-LISP no es case-sensitive 
; y ademas no distingue entre nil y la lista vacia
(defn igual? [a b] 
	(if (or (seq? a) (seq? b))
		(cond	
			(and (nil? a) (seq? b) (empty? b)) true		; nil '()'
			(and (nil? b) (seq? a) (empty? a)) true		; '() nil
			(and (nil? a) (nil? b)) true			; nil nil
			(and (seq? a) (empty? a) (seq? b) (empty? b)) true 	; '() '()
			:else (= a b)
		)
		;false
		(cond
			(and (= 'NIL a) (nil? b)) true 		; 'NIL nil
			(and (nil? a) (= 'NIL b)) true 		; nil 'NIL
			:else	
			(= (.toLowerCase (str a)) (.toLowerCase (str b)))
		) 
	)
	; '() => es una secuencia
	; 'NIL => no es una secuencia
	; (and (= 'NIL a) (empty? b)) true	; 'NIL '()'
	; (and (empty? a) (= 'NIL b)) true	; '() 'NIL
)

;------------------------------------------------------------------------------------------
; Revisa una lista que representa una funcion.
; Recibe la lista y, si esta comienza con '*error*, la retorna. Si no, retorna nil.
(defn revisar-f [lis]
	(if
		(list? lis)
		;true
		(if
			(= '*error* (first lis))
			lis
			nil
		)
		;false
		nil
	)
)
; EJEMPLOS
; (revisar-f 'doble) ;=> nil
; (revisar-f '(*error* too-few-args)) ;=> (*error* too-few-args)

;------------------------------------------------------------------------------------------
; Revisa una lista de argumentos evaluados.
; Recibe la lista y, si esta contiene alguna sublista 
; que comienza con '*error*, retorna esa sublista. Si no, retorna nil.
(defn revisar-lae [lis] 
	(if
		(= nil (first (filter identity (map list? (map revisar-f lis)))))
		nil
		(first (filter identity (map revisar-f lis)))
	)
)

; EJEMPLOS
; (revisar-lae '(1 add first)) ;=> nil
; (revisar-lae '(1 add (*error* too-many-args) first)) ;=> (*error* too-many-args)

;------------------------------------------------------------------------------------------
; Evalua el cuerpo de una macro COND. 
; Siempre retorna una lista con un resultado y un ambiente.
; Recibe una lista de sublistas 
; (cada una de las cuales tiene una condicion en su 1ra. posicion)
;  y los ambientes global y local.

; Si la lista es nil, el resultado es nil y el ambiente retornado es el global.
; Si no, evalua (con evaluar) la cabeza de la 1ra. sublista y, 
; si el resultado no es nil, retorna el res. de invocar a evaluar-secuencia-en-cond 
; con la cola de esa sublista.
; En caso contrario, sigue con las demas sublistas.
(defn evaluar-cond [lis amb-global amb-local] 
	(if
		(nil? lis)
		(list nil amb-global)
		;else
		(if
			(not (nil? (first (evaluar (ffirst lis) amb-global amb-local))))
				(evaluar-secuencia-en-cond (next (first lis)) amb-global amb-local)
				(recur (next lis) amb-global amb-local)
		)
	)
)

;Ejemplo en LISP
; (cond ((> x 1) (print x)(first w)(rest w))
; 	((= x 1) (print x))
; 	(t (print z))
; )

;Ejemplo en Clojure
; (cond
; 	(> x 1)
; 		(do
; 			(print x)
; 			(first w)
; 			(rest w)
; 		)
; 	(= x 1)
; 		(print x)
; 	true
; 		(print z)
; )


;lis '(((equal 'a 'b) (setq x 1)))
; (first '(((equal 'a 'b) (setq x 1)))) ;=> 
; 		((equal (quote a) (quote b)) (setq x 1))

; (ffirst '(((equal 'a 'b) (setq x 1)))) ;=> 
; 		(equal (quote a) (quote b))

; (drop 1 (first '(((equal 'a 'b) (setq x 1))))) ;=> 
; 		(equal (quote a) (quote b))

; (drop 1 (first lis)) ;=> 
; 		(equal (quote a) (quote b))

; EJEMPLOS
; (evaluar-cond nil '(equal equal setq setq) nil) ;=> (nil (equal equal setq setq))
; (evaluar-cond '(((equal 'a 'b) (setq x 1))) '(equal equal first first) nil) ;=> (nil (equal equal first first))
; (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2)))) ;=> '(equal equal setq setq) nil) (2 (equal equal setq setq y 2))
; (evaluar-cond '(((equal'a 'b)(setq x 1))((equal 'a 'a)(setq y 2)(setq z 3))) ;=> '(equal equal setq setq) nil) (3 (equal equal setq setq y 2 z 3))

;(evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2))) '(equal equal setq setq) nil) ;=> (2 (equal equal setq setq y 2))

;------------------------------------------------------------------------------------------
; Evalua (con evaluar) secuencialmente las sublistas de una lista 
; y retorna el valor de la ultima evaluacion.
(defn evaluar-secuencia-en-cond [lis amb-global amb-local]
	(if (nil? (next lis))
			(evaluar (first lis) amb-global amb-local)
			
			(let [res (evaluar (first lis) amb-global amb-local)]
				(if (and (seq? (first res)) (igual? (ffirst res) '*error*))
						res
						(recur (next lis) (second res) amb-local)
				)
			)
	)
)

true
;Ejemplo en LISP
; (cond ((> x 1) (print x)(first w)(rest w))
; 	((= x 1) (print x))
; 	(t (print z))
; )

; EJEMPLOS

; (evaluar-secuencia-en-cond '((setq y 2)) '(setq setq) nil) ;=> (2 (setq setq y 2))
; (evaluar-secuencia-en-cond '((setq y 2) (setq z 3)) '(setq setq) nil) ;=> (3 (setq setq y 2 z 3))




; Falta hacer que la carga del interprete en Clojure (tlc-lisp.clj) retorne true 


