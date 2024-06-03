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
  (let [crudo (slurp (io/resource nombre))]
    (seq (.split crudo "\n"))))

(defn split-once [s]
  (let [space-index (.indexOf s " ")]
    [(subs s 0 space-index) (subs s (inc space-index))]))

(defn dic-reglas [seq-archivo]
  "Recibe la secuencia del archivo sl y devuelve un diccionario con las reglas"
  (let [seq-reglas (rest (rest seq-archivo))]
    (into {}
          (map (fn [s]
                 (let [[k v] (split-once s)]
                   [(first k) v]))
               seq-reglas))))

(defn axioma-archivo [seq-archivo]
  (second seq-archivo))

(defn angulo-archivo [seq-archivo]
  (Double/parseDouble (first seq-archivo)))

;****************************************************************
;GRAFICOS TORTUGA
;****************************************************************



(defn grados-a-radianes [grados]
  (* grados (/ math/PI 180)))

(defrecord Tortuga [x y angulo])

(defn mover-tortuga [tortuga distancia]
  (let [angulo-rad (grados-a-radianes (:angulo tortuga))
        nuevo-x (+ (:x tortuga) (* distancia (math/cos angulo-rad)))
        nuevo-y (+ (:y tortuga) (* distancia (math/sin angulo-rad)))]
    (assoc tortuga :x nuevo-x :y nuevo-y)))

(defn girar-tortuga [tortuga angulo]
  (update tortuga :angulo + angulo))

;En todos nuestros sistemas-L usaremos como alfabeto los caracteres ASCII, y a cada sistema le asignaremos además un ángulo α.

;Nuestras imágenes serán generadas asignando una operación de la tortuga a cada caracter, de la siguiente manera:

;; F o G: Avanzar una unidad
;; f o g: Pluma arriba, avanzar una unidad, pluma abajo
;; +: Girar a la derecha α
;; -: Girar a la izquierda α
;; |: Invertir la dirección (es decir, girar 180°)
;; Además, contaremos con una pila de tortugas, y la tortuga activa será siempre la que está en el tope de la pila. Los siguientes comandos controlarán la pila:

;; [: Apilar una nueva tortuga, que arranca con el mismo estado (posición, orientación y pluma) que la tortuga que estaba previamente en el tope.
;; ]: Desapilar (la tortuga que estaba en el tope se descarta).
;; Cualquier caracter que no tenga asignada una operación (es decir, que no sea uno de los listados arriba), será ignorado.

(def UNIDAD-SVG 3)

(defn ejecutar-comando [tortuga comando angulo pila lista-svg]
  ;; (println tortuga)
  ;; (println comando)
  ;; (println angulo)
  ;; (println pila)
  (case comando
    \F [(mover-tortuga tortuga UNIDAD-SVG) pila (conj lista-svg [:linea (:x tortuga) (:y tortuga) (:x (mover-tortuga tortuga UNIDAD-SVG)) (:y (mover-tortuga tortuga UNIDAD-SVG))])]
    \G [(mover-tortuga tortuga UNIDAD-SVG) pila (conj lista-svg [:linea (:x tortuga) (:y tortuga) (:x (mover-tortuga tortuga UNIDAD-SVG)) (:y (mover-tortuga tortuga UNIDAD-SVG))])]
    \f [(mover-tortuga tortuga UNIDAD-SVG) pila lista-svg]
    \g [(mover-tortuga tortuga UNIDAD-SVG) pila lista-svg]
    \+ [(girar-tortuga tortuga angulo) pila lista-svg]
    \- [(girar-tortuga tortuga (- angulo)) pila lista-svg]
    \| [(girar-tortuga tortuga 180) pila lista-svg]
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


(defn encabezado-svg []
  "Crea el encabezado del SVG y lo retorna como una cadena."
  "<svg viewBox=\"-181.3250343814076 -891.0 325.5414406036851 972.0\" xmlns=\"http://www.w3.org/2000/svg\">\n")

(defn final-svg []
  "Crea el final del SVG y lo retorna como una cadena."
  "</svg>")

(defn cuerpo-svg [lista-svg]
  "Crea el cuerpo del SVG y lo retorna como una cadena."
  (apply str (map (fn [x] (str "<line x1=\"" (nth x 1) "\" y1=\"" (nth x 2) "\" x2=\"" (nth x 3) "\" y2=\"" (nth x 4) "\" stroke-width=\"1\" stroke=\"black\"/>\n")) lista-svg)))

(defn escribir-svg! [nombre-archivo lista-svg]
  "Escribe el contenido SVG en un archivo."
  (spit nombre-archivo (str (encabezado-svg) (cuerpo-svg lista-svg) (final-svg))))


;************************************
; MAIN
;**************************************

(defn -main [& args]
  (let [nombre-archivo (first args)
        iter (Integer/parseInt (second args))
        archivo-salida (nth args 2)
        seq-archivo (secuencia-archivo! nombre-archivo)
        axioma (axioma-archivo seq-archivo)
        reglas (dic-reglas seq-archivo)
        comandos (transformacion axioma reglas iter)
        lista-svg (interpretar comandos (angulo-archivo seq-archivo) (Tortuga. 0 0 -90) [] [])]
    (escribir-svg!  archivo-salida lista-svg)))

