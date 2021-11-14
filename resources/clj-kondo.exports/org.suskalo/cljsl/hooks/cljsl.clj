(ns ^:no-doc hooks.cljsl
  (:require
   [clj-kondo.hooks-api :as api])
  (:refer-clojure :exclude [definterface]))

(defn definterface
  [{:keys [node]}]
  (let [[interface-name & more] (rest (:children node))
        [docstring shader-bindings & kwargs] (if-not (string? (api/sexpr (first more)))
                                               (cons nil more)
                                               more)
        bindings (->> (partition 2 (:children shader-bindings))
                      (mapcat (fn [[k v]]
                                [k (if (api/list-node? v)
                                     (api/list-node (cons `do (:children v)))
                                     v)]))
                      (api/map-node))
        node (api/list-node
              (list
               (api/token-node `def)
               interface-name
               (if docstring
                 docstring
                 (api/string-node ""))
               (api/list-node
                (list*
                 (api/token-node `do)
                 bindings
                 kwargs))))]
    {:node node}))

(defn defshader
  [{:keys [node]}]
  (let [[shader-name & more] (rest (:children node))
        [docstring param-bindings & body] (if-not (string? (api/sexpr (first more)))
                                            (cons nil more)
                                            more)
        node (api/list-node
              (list
               (api/token-node `def)
               shader-name
               (if docstring
                 docstring
                 (api/string-node ""))
               (api/list-node
                (list*
                 (api/token-node `do)
                 param-bindings
                 body))))]
    {:node node}))
