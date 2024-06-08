(ns tp2.core
  (:require [clojure.java.io :as io])
  (:require [clojure.math :as math])
  (:gen-class))

(defn transformacion [axioma reglas iter]
  "transforma el axioma con las reglas la cantidad de veces indicada"
  (if (zero? iter)
    axioma
    (let [nuevo-axioma (apply str (replace reglas (vec axioma)))]
      (transformacion nuevo-axioma reglas (dec iter)))))

(defn secuencia-archivo! [nombre]
  "Lee el archivo y devuelve una secuencia de los renglones del mismo."
  (let [crudo (slurp (io/resource (str "archivos-sl/" nombre)))]
    (seq (.split crudo "\n"))))

(defn division-clave-valor [s]
  (let [espacio-indice (.indexOf s " ")]
    [(subs s 0 espacio-indice) (subs s (inc espacio-indice))]))

(defn dic-reglas [seq-archivo]
  "Recibe la secuencia del archivo sl y devuelve un diccionario con las reglas"
  (let [seq-reglas (rest (rest seq-archivo))]
    (into {}
          (map (fn [s]
                 (let [[k v] (division-clave-valor s)]
                   [(first k) v]))
               seq-reglas))))

(defn axioma-archivo [seq-archivo]
  (second seq-archivo))

(defn angulo-archivo [seq-archivo]
  (Double/parseDouble (first seq-archivo)))

;****************************************************************
;GRAFICOS TORTUGA
;****************************************************************

(def medio-giro 180)

(defn grados-a-radianes [grados]
  (* grados (/ math/PI medio-giro)))

(defrecord Tortuga [x y angulo])

(defn mover-tortuga [tortuga distancia]
  (let [angulo-rad (grados-a-radianes (:angulo tortuga))
        nuevo-x (+ (:x tortuga) (* distancia (math/cos angulo-rad)))
        nuevo-y (+ (:y tortuga) (* distancia (math/sin angulo-rad)))]
    (assoc tortuga :x nuevo-x :y nuevo-y)))

(defn girar-tortuga [tortuga angulo]
  (update tortuga :angulo + angulo))



(def unidad-svg 3)



(defn ejecutar-comando [tortuga comando angulo pila lista-svg]
  "Ejecuta un comando en la tortuga.
   Devuelve la tortuga actualizada después de ejecutar el comando."
  (case comando
    \F [(mover-tortuga tortuga unidad-svg) pila (conj lista-svg [:linea (:x tortuga) (:y tortuga) (:x (mover-tortuga tortuga unidad-svg)) (:y (mover-tortuga tortuga unidad-svg))])]
    \G [(mover-tortuga tortuga unidad-svg) pila (conj lista-svg [:linea (:x tortuga) (:y tortuga) (:x (mover-tortuga tortuga unidad-svg)) (:y (mover-tortuga tortuga unidad-svg))])]
    \f [(mover-tortuga tortuga unidad-svg) pila lista-svg]
    \g [(mover-tortuga tortuga unidad-svg) pila lista-svg]
    \+ [(girar-tortuga tortuga angulo) pila lista-svg]
    \- [(girar-tortuga tortuga (- angulo)) pila lista-svg]
    \| [(girar-tortuga tortuga medio-giro) pila lista-svg]
    \[ [tortuga (conj pila tortuga) lista-svg]
    \] [(peek pila) (pop pila) lista-svg]
    [tortuga pila lista-svg]))

(defn interpretar [comandos angulo tortuga pila lista-svg]
  (if (empty? comandos)
    lista-svg
    (let [vec (ejecutar-comando tortuga (first comandos) angulo pila lista-svg)]
      (recur (rest comandos) angulo (first vec) (second vec) (last vec)))))




;************************************
; ESCRIBIR ARCHIVO
;**************************************

(def margen 15)

(defn obtener-bordes-dibujo [lista-svg]
  "Obtiene los valores maximos y minimos tanto en x como en y de la lista de lineas."
  (reduce (fn [[min-x min-y max-x max-y] [_ x1 y1 x2 y2]]
            [(min min-x (min x1 x2))
             (min min-y (min y1 y2))
             (max max-x (max x1 x2))
             (max max-y (max y1 y2))])
          [0 0 0 0]
          lista-svg))

(defn agregar-margen [[min-x min-y max-x max-y]]
  "Agrega un margen a los valores maximos y minimos."
  [(- min-x margen) (- min-y margen) (+  max-x margen) (+ max-y margen)])


(defn encabezado-svg [[min-x min-y max-x max-y]]
  "Crea el encabezado del SVG y lo retorna como una cadena."
  (str "<svg viewBox=\"" (str min-x " " min-y " " (- max-x min-x) " " (- max-y min-y)) "\" xmlns=\"http://www.w3.org/2000/svg\">\n"))

(defn final-svg []
  "Crea el final del SVG y lo retorna como una cadena."
  "</svg>")

(defn cuerpo-svg [lista-svg]
  "Crea el cuerpo del SVG y lo retorna como una cadena."
  (apply str (map (fn [[_ x1 y1 x2 y2]] (str "<line x1=\"" x1 "\" y1=\"" y1 "\" x2=\"" x2 "\" y2=\"" y2 "\" stroke-width=\"1\" stroke=\"black\"/>\n")) lista-svg)))

(defn escribir-svg! [nombre-archivo lista-svg]
  "Escribe el contenido SVG en un archivo."
  (spit (str "resources/archivos-svg/" nombre-archivo) (str (encabezado-svg (agregar-margen (obtener-bordes-dibujo lista-svg))) (cuerpo-svg lista-svg) (final-svg))))


;************************************
; MAIN
;************************************

(def orientacion-correcta -90)

(defn -main [& args]
  (let [nombre-archivo (first args)
        iter (Integer/parseInt (second args))
        archivo-salida (nth args 2)
        seq-archivo (secuencia-archivo! nombre-archivo)
        axioma (axioma-archivo seq-archivo)
        reglas (dic-reglas seq-archivo)
        comandos (transformacion axioma reglas iter)
        lista-svg (interpretar comandos (angulo-archivo seq-archivo) (Tortuga. 0 0 orientacion-correcta) [] [])]
    (escribir-svg!  archivo-salida lista-svg)))



