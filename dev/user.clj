(ns user
  (:require
   [cljsl.compiler :as c :refer [defparam defuniform defshaderfn defshader]]))

(c/definterface vertex-data
  {:position ("vec3" :layout {"location" 0})
   :normal ("vec3" :layout {"location" 1})})

(c/defstruct light-data
  {:direction "vec3"
   :color "vec3"})

(defuniform light light-data)

(defuniform ambient-lighting "float")

(defparam frag-normal "vec3"
  :interpolation "flat")

(defparam vert-color "vec3")

(defshaderfn calc-lighting
  ^"vec3" [^"vec3" normal ^light-data light]
  (let [^"float" factor (dot normal (:direction light))]
    (return (* factor (:color light)))))

(defshader vert-shader
  {vertex-data :in
   frag-normal :out
   vert-color :out}
  (set! gl_Position (:position vertex-data))
  (set! frag-normal (:normal vertex-data))
  (set! vert-color (vec3 0 1 0)))

(defparam frag-color "vec4")

(defshader frag-shader
  {frag-normal :in
   vert-color :in
   frag-color :out}
  (let [^"vec3" light (+ (calc-lighting frag-normal light)
                         (vec3 ambient-lighting))]
    (set! frag-color (* light vert-color))))
