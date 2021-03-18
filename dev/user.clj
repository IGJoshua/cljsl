(ns user
  (:require
   [cljsl.compiler :as c]))

(def sampler-uniform)

(def light-dir-uniform)

(defmacro defshaderfn
  ""
  {:arglists '([symbol docstring? [params*] & body])}
  [sym & more]
  (let [docstring (when (string? (first more))
                    (first more))
        params (if (string? (first more))
                 (second more)
                 (first more))
        body (nthrest more (if (string? (first more)) 2 1))]
    `(def ~sym ~@(when docstring [docstring])
       (let [[src# deps#] (c/compile-function '~(symbol (name (ns-name *ns*)) (name sym))
                                              '~params
                                              ~(:tag (meta params))
                                              '~body)]
         {:source src#
          :deps deps#}))))

(defshaderfn calc-lighting
  ""
  ^"vec3" [^"vec3" normal ^"vec3" light-color]
  (let [^"float" factor (dot normal light-dir-uniform)]
    (return (* factor light-color))))

(comment

  (c/compile
   '(set! gl_Position (vec4 1 1 1 0))
   {})

  (c/compile
   '(do (set! gl_Position
              (* (vec4 (+ (vec3 obj-pos 0) v-pos) 1)
                 (vec4 0.1 0.1 0 1)))
        (set! color obj-color)
        (discard))
   {})

  (c/compile
   '(cond
      (> x 10) (set! x 10)
      (< x 1) (set! x 1)
      :otherwise (set! done 1))
   {})

  (c/compile
   '(let [^"int" a 10
          ^"vec4" b (vec4 1 2 3 0)]
      (return (* a b)))
   {})

  (c/compile
   '(for [^"int" x 0 (set! x (+ x 1))]
      (< x 10)
      (sample sam (vec2 0 (/ x 10))))
   {})

  (c/compile-function 'main '(^{:mods "out" :tag "vec4"} a)
                      nil '((set! gl_Position a) (discard)))

  )
