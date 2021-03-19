(ns user
  (:require
   [cljsl.compiler :as c :refer [defconst defparam defuniform defshaderfn defshader]]
   [clojure.string :as str]))

(defconst light-color "vec3"
  ""
  (vec3 1 2 3))

(defuniform ambient-lighting "float"
  "")

(defuniform light-dir-uniform "vec3"
  "")

(defparam position "vec3"
  ""
  :layout {"location" 0})

(defparam vert-normal "vec3"
  ""
  :layout {"location" 1})

(defparam frag-normal "vec3"
  ""
  :interpolation "flat")

(defparam vert-color "vec3"
  "")

(defparam frag-color "vec4"
  "")

(defshaderfn calc-lighting
  ""
  ^"vec3" [^"vec3" normal ^"vec3" light-color]
  (let [^"float" factor (dot normal light-dir-uniform)]
    (return (* factor light-color))))

(defshader vert-shader
  ""
  {position :in
   vert-normal :in
   frag-normal :out
   vert-color :out}
  (set! gl_Position position)
  (set! frag-normal vert-normal)
  (set! vert-color (vec3 0 1 0)))

(defshader frag-shader
  ""
  {frag-normal :in
   vert-color :in
   frag-color :out}
  (let [^"vec3" light (+ (calc-lighting frag-normal light-color)
                         (vec3 ambient-lighting))]
    (set! frag-color (* light vert-color))))

(def pipeline
  "Map with the two shaders."
  {:vertex vert-shader
   :fragment frag-shader})
