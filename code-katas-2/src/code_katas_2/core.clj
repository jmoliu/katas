(ns code-katas-2.core)


(defn unpartial
"Escribir una funcion que acepte una funcion parcial con cantidad de argumentos desconocida,
retornar una funcion equivalente de n argumentos"
[f]
(fn [& args]
                 (loop [funcion f argumentos args]
                   (let [res (funcion (first argumentos))]
                     (if (fn? res)
                       (recur res (rest argumentos))
                       res
                       ))
                     )))


;;No esta funcionando con secuencias infinitas
(defn search
"Dado un numero cualquiera de secuencias, cada una ya ordenada de menor a mayor, encontrar el numero
mas chico que aparezca en todas las secuencias, las secuencias pueden ser infinitas."
[& seqs]
(loop [secuencias seqs]
           (def candidatos (map first secuencias))
           (if (= (count (distinct candidatos)) 1)
             (first candidatos)
             (let [mayor (primer-mayor secuencias)]
               (recur (avanzar mayor secuencias)))
             )))

;;FUNCION AGREGADA
(defn avanzar
  [mayor secuencias]
  (loop [seqs secuencias acc []]
     (if (empty? seqs)
      acc
      (if (< (first (first seqs)) mayor)
        
        (recur (rest seqs) (concat acc [(rest (first seqs))]))
        (recur (rest seqs) (concat acc [(first seqs)]))
        
        ;(recur (rest seqs) (into acc [(vec (rest (first seqs)))]))
        ;(recur (rest seqs) (into acc [(vec (first seqs))])))
 ))))

;;FUNCION AGREGADA
(defn primer-mayor
[seqs]
(reduce max (map first seqs)))


(defn intercalar
"Escriba una funcion que tome un predicado de 2 argumentos, un valor y una coleccion, y
retorne una nueva coleccion donde el valor es insertado intercalado cada dos argumentos
que cumplan el predicado"
[predicado valor secuencia]
(lazy-seq
                       (if-not (empty? (rest secuencia))
                         (if (predicado (first secuencia) (first (rest secuencia)))
                           
                                  (conj (intercalar predicado valor (rest secuencia)) valor (first secuencia))
                                  (conj (intercalar predicado valor (rest secuencia)) (first secuencia))
                             )
                                secuencia
                         ) 	
                       ))

(defn tartamudeo
"Escriba una funcion que retorne una secuencia lazy que comprima el tartamudeo de una secuencia de numeros.
Comprimir el tartamudeo se refiere a que [1 1 1] se exprese como [3 1] y a su vez [3 1] se exprese como [1 3 1 1].
La funcion debe aceptar una secuencia inicial de numeros, y devolver una secuencia infinita de compresiones, donde
cada nuevo elemento es el elemento anterior comprimido."
[secuencia]
)
