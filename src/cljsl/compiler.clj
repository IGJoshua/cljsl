(ns cljsl.compiler
  "Compiles a subset of Clojure data, interpreted as a lisp into GLSL."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   [clojure.zip :as z])
  (:import
   (clojure.lang Symbol))
  (:refer-clojure
   :exclude [compile definterface defstruct]))

(def digit?
  "All the digit characters."
  #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0})

(defn str->ident
  "Converts a string into a GLSL-compliant identifier."
  [s]
  (apply str
         (sequence (comp (replace {\- \_ \space \_ \newline \_})
                         (map (fn [c]
                                (if (re-matches #"[0-9a-zA-Z_]" (str c))
                                  (str c)
                                  (str (apply str "__" (replace {\space \_ \( \_ \) \_}
                                                                (Character/getName (int c))))
                                       "__")))))
                   (seq s))))

(defn sym->ident
  ""
  [s]
  (if (namespace s)
    (str "NS__" (str->ident (namespace s)) "__SYM__" (str->ident (name s)))
    (str (let [f (first (name s))]
           (if (digit? f)
             (str \_ f)
             f))
         (str->ident (subs (name s) 1)))))

(defprotocol Atom
  "Objects that can be converted directly into text in GLSL code."
  :extend-via-metadata true
  (compile-atom [a] "Converts the object into GLSL."))

(extend-protocol Atom
  Long
  (compile-atom [a]
    (str a))

  Integer
  (compile-atom [a]
    (str a))

  Short
  (compile-atom [a]
    (str a))

  Byte
  (compile-atom [a]
    (str a))

  Float
  (compile-atom [a]
    (str a "f"))

  Double
  (compile-atom [a]
    (str a))

  Symbol
  (compile-atom [a]
    (sym->ident a)))

(declare ^:private special-forms compile)

(defn- compile-fn-call
  "Constructs a GLSL function call for a form"
  [form env]
  (let [compiled (map #(compile % env) (rest form))
        args (map first compiled)
        deps (reduce set/union (map second compiled))]
    [(str (compile-atom (first form)) "(" (str/join ", " args) ")")
     deps]))

(defn- compile-field-access
  "Constructs a GLSL struct field access for a form"
  [form env]
  (let [[object deps] (compile (second form) env)]
    [(str object "." (sym->ident (first form)))
     deps]))

(defn- ensure-ns
  "Takes a symbol, and if it lacks a namespace, adds [[*ns*]]."
  [sym]
  (if (namespace sym)
    sym
    (symbol (name (ns-name *ns*)) (name sym))))

(defn compile
  ""
  [form env]
  (if (seq? form)
    (cond
      (and (symbol? (first form))
           (contains? special-forms (symbol (name (first form)))))
      (let [new-first (symbol (name (first form)))]
        (apply (special-forms new-first) (conj (rest form) new-first) env (rest form)))

      (and (symbol? (first form))
           (resolve env (first form)))
      (when-let [var (resolve env (first form))]
        (if (.isMacro var)
          (recur (apply var form env (rest form)) env)
          (let [new-sym (ensure-ns (first form))
                [compiled deps] (compile-fn-call (cons new-sym (rest form)) env)]
            [compiled (conj deps var)])))

      (symbol? (first form))
      (compile-fn-call (cons (symbol (name (first form)))
                             (rest form))
                       env)

      (keyword? (first form))
      (compile-field-access form env))
    (cond
      (symbol? form)
      (if-let [var (resolve env form)]
        [(compile-atom (ensure-ns form)) #{var}]
        [(compile-atom form) #{}])

      :otherwise (compile-atom form))))

(defn- cljsl-if
  ""
  ([form env test then]
   (cljsl-if form env test then nil))
  ([form env test then else]
   (let [[test-src test-deps] (compile test env)
         [then-src then-deps] (compile then env)
         [else-src else-deps] (when else
                                (compile else env))]
     [(str "if(" test-src ")\n"
           then-src
           (when else
             (str "else\n"
                  else-src)))
      (set/union test-deps then-deps else-deps)])))

(defn- cljsl-cond
  ""
  [form env & clauses]
  (let [compiled (map #(if-not (keyword? %)
                         (compile % env) ; compile all the tests
                         [% #{}])        ; but use keywords for the else
                      clauses)
        deps (reduce set/union (map second compiled)) ; collect all the dependencies
        clauses (partition 2 (map first compiled))]   ; group into clauses
    [(str "if(" (first (first clauses)) ")\n"
          "{\n"
          (second (first clauses))
          ";\n"
          "}\n"
          (apply str
                 (for [[test result] (take-while (comp (complement keyword?) first)
                                                 (rest clauses))]
                   (str "else if(" test ")\n"
                        "{\n"
                        result
                        ";\n"
                        "}\n")))
          (when (keyword? (first (last clauses)))
            (str "else\n"
                 "{\n"
                 (second (last clauses))
                 ";\n"
                 "}\n")))
     deps]))

(defn- cljsl-set!
  ""
  [form env var initexpr]
  (let [[initexpr deps] (compile initexpr env)
        dep-var (resolve env var)
        deps (cond-> deps
               dep-var (conj dep-var))]
    [(str (compile-atom (if dep-var
                          (ensure-ns var)
                          var))
          "=" initexpr)
     deps]))

(defn- cljsl-do
  ""
  [form env & body]
  (let [compiled (map #(compile % env) body)
        statements (map first compiled)
        deps (reduce set/union (map second compiled))]
    [(str (str/join ";\n" statements)
          ";\n")
     deps]))

(defn- var-declaration
  ""
  ([type var init env]
   (var-declaration type nil var init env))
  ([type mods var init env]
   (when-not type
     (throw (ex-info "variable declarations require a type"
                     {:var var})))
   (let [type-dep (if-let [type-var (when (symbol? type)
                                      (resolve type))]
                    #{type-var}
                    #{})
         [initexpr deps] (when init
                           (compile init env))]
     [(str mods
           " "
           (if (symbol? type)
             (compile-atom (ensure-ns type))
             (str->ident type))
           " "
           (compile-atom var)
           (when initexpr
             (str "=" initexpr)))
      (set/union type-dep deps)])))

(defn- cljsl-let
  ""
  [form env bindings & body]
  (let [binding (let [bindings (map vec (partition 2 bindings))]
                   (map var-declaration
                        (map (comp :tag meta first) bindings)
                        (map first bindings)
                        (map second bindings)
                        (reductions conj env bindings)))
        binding-src (map first binding)
        binding-deps (reduce set/union (map second binding))
        body (map #(compile % (merge env (apply hash-map bindings))) body)
        body-src (map first body)
        body-deps (reduce set/union (map second body))]
    [(str "{\n"
          (str/join ";\n" binding-src)
          ";\n"
          (str/join ";\n" body-src)
          ";\n"
          "}\n")
     (set/union binding-deps body-deps)]))

(defn- cljsl-for
  ""
  [form env binding test & body]
  (let [new-env (assoc env (first binding) (second binding))
        [decl-src decl-deps] (var-declaration (:tag (meta (first binding)))
                                              (first binding)
                                              (second binding)
                                              env)
        [test-src test-deps] (compile test new-env)
        [inc-src inc-deps] (compile (nth binding 2) new-env)
        body (map #(compile % new-env) body)
        body-src (map first body)
        body-deps (map second body)]
    [(str "for(" decl-src
          ";" test-src
          ";" inc-src
          ")\n"
          "{\n"
          (str/join ";\n" body-src)
          ";\n"
          "}\n")
     (reduce set/union body-deps [decl-deps test-deps inc-deps])]))

(defn- infix-op
  ""
  [op]
  (fn [form env & args]
    (let [compiled (map #(compile % env) args)
          exprs (map first compiled)
          deps (reduce set/union (map second compiled))]
      [(str "("
            (str/join op exprs)
            ")")
       deps])))

(defn- unary-minus
  [form env arg]
  (let [[expr deps] (compile arg env)]
    [(str "(-(" expr "))")
     deps]))

(def ^:private nary-minus (infix-op "-"))

(defn- cljsl-minus
  ""
  [form env & args]
  (if (= 1 (count args))
    (unary-minus form env (first args))
    (apply nary-minus form env args)))

(defn- grouped-infix
  ""
  [op grouping]
  (fn [form env & args]
    (let [compiled (map #(compile % env) args)
          args (map first compiled)
          deps (reduce set/union (map second compiled))
          groups (partition 2 1 args)]
      [(str "("
            (str/join grouping
                      (map #(str "(" (first %) op (second %) ")") groups))
            ")")
       deps])))

(defn- cljsl-return
  ""
  ([form env val]
   (let [[expr deps] (compile val env)]
     [(str "return " expr)
      deps]))
  ([form env]
   ["return" #{}]))

(defn- cljsl-break
  ""
  [form env]
  ["break" #{}])

(defn- cljsl-continue
  ""
  [form env]
  ["continue" #{}])

(defn- cljsl-discard
  ""
  [form env]
  ["discard" #{}])

(def ^:private special-forms
  {'if #'cljsl-if
   'cond #'cljsl-cond
   'set! #'cljsl-set!
   'do #'cljsl-do
   'let #'cljsl-let
   'for #'cljsl-for
   'return #'cljsl-return
   'break #'cljsl-break
   'continue #'cljsl-continue
   'discard #'cljsl-discard
   'float (fn [form env val]
            [(compile-atom (float val)) #{}])
   'double (fn [form env val]
             [(compile-atom (double val)) #{}])
   'long (fn [form env val]
           [(compile-atom (long val)) #{}])
   'int (fn [form env val]
          [(compile-atom (int val)) #{}])
   'short (fn [form env val]
            [(compile-atom (short val)) #{}])
   'byte (fn [form env val]
           [(compile-atom (byte val)) #{}])
   '+ (infix-op "+")
   '/ (infix-op "/")
   '* (infix-op "*")
   '- #'cljsl-minus
   '< (grouped-infix "<" "&&")
   '> (grouped-infix ">" "&&")
   '>= (grouped-infix ">=" "&&")
   '<= (grouped-infix "<=" "&&")
   '== (grouped-infix "==" "&&")
   '&& (infix-op "&&")
   '|| (infix-op "||")
   '& (infix-op "&")
   '| (infix-op "|")})

(defn compile-function
  ""
  [name args ret body]
  (when-not name
    (throw (ex-info "functions require a name" {})))
  (let [bindings (map var-declaration
                      (map (comp :tag meta) args)
                      (map (comp :mods meta) args)
                      args
                      (repeat nil)
                      (repeat {}))
        bindings-src (map first bindings)
        bindings-deps (reduce set/union (map second bindings))
        env (zipmap args (repeat :function-parameter))
        body (compile (cons 'do body) env)
        body-src (first body)
        body-deps (second body)
        ret-dep (if-let [ret-var (when (symbol? ret)
                                   (resolve ret))]
                  #{ret-var}
                  #{})]
    [(str (if ret
            (if (symbol? ret)
              (compile-atom ret)
              (str->ident ret))
            "void")
          " "
          (compile-atom name)
          "("
          (str/join "," bindings-src)
          ")\n"
          "{\n"
          body-src
          "}\n")
     (set/union bindings-deps body-deps ret-dep)]))

(defn- layout-str
  ""
  [layout]
  (str "layout(" (str/join "," (map #(str (key %) (when-let [v (val %)] (str "=" v))) layout)) ")"))

(defn compile-global
  ""
  [var-name type storage & {:keys [layout invariant? interpolation array-size init memory-qualifier] :as opts}]
  (when-not var-name
    (throw (ex-info "globals require a name" {})))
  (when-not type
    (throw (ex-info "globals require a type"
                    {:var var-name})))
  (let [[initexpr init-deps] (when init (compile init {}))
        [arr-expr arr-deps] (when array-size (compile array-size {}))
        type-deps (if-let [var (when (symbol? type)
                                 (resolve type))]
                    #{var}
                    #{})]
    [(str (when invariant? "invariant")
          " " interpolation
          " " (when layout (layout-str layout))
          " " memory-qualifier
          " " storage
          " " (if (symbol? type) (sym->ident (ensure-ns type)) type)
          " " (sym->ident var-name) (when arr-expr (str "[" arr-expr "]"))
          " " (when init (str "=" initexpr))";\n")
     (or (set/union init-deps arr-deps) #{})]))

(defn compile-block
  ""
  [block-name storage bindings & {:keys [layout instance-name array-size memory-qualifier]}]
  (when (and memory-qualifier
             (not= storage "buffer"))
    (throw (ex-info "memory qualifiers can only be specified on buffer blocks"
                    {:block block-name :qualifier memory-qualifier})))
  (let [[arr-expr deps] (when array-size (compile array-size {}))
        bindings (map (fn [[name binding]]
                        (let [[type & {:keys [storage] :as mods}]
                              (if-not (seq? binding) [binding] binding)]
                          (apply compile-global name type storage (mapcat identity mods))))
                      bindings)
        bindings-src (map first bindings)
        bindings-deps (map second bindings)]
    [(str (when layout (layout-str layout))
          " " memory-qualifier
          " " storage
          " " (sym->ident block-name) "\n"
          "{\n"
          (str/join bindings-src)
          "} "
          (when instance-name
            (sym->ident instance-name))
          (when arr-expr
            (str "[" arr-expr "]"))
          ";\n")
     (or (reduce set/union deps bindings-deps) #{})]))

;; ==================================================================
;; Macros

(defmacro defconst
  ""
  {:arglists '([symbol type docstring? init & {:keys [array-size]}])}
  [sym type & args]
  (let [[docstring init & {:keys [array-size]}]
        (cond->> args
          (not (string? (first args))) (cons nil))
        [src deps] (compile-global (ensure-ns sym)
                                   type
                                   "const"
                                   :init init
                                   :array-size array-size)]
    `(def ~sym ~@(when docstring [docstring])
       {::type :const
        ::source ~src
        ::deps ~deps})))

(defmacro defparam
  ""
  {:arglists '([symbol type docstring? & {:keys [array-size interpolation layout invariant?]}])}
  [sym type & args]
  (let [[docstring & {:as opts}]
        (cond->> args
          (not (string? (first args))) (cons nil))]
    `(def ~sym ~@(when docstring [docstring])
       {::type :param
        ::name '~(ensure-ns sym)
        ::param-type ~type
        ::opts ~opts
        ::deps ~(if (symbol? type)
                  #{(resolve type)}
                  #{})})))

(defmacro defuniform
  ""
  {:arglists '([symbol type docstring? & {:keys [array-size layout]}])}
  [sym type & args]
  (let [[docstring & {:keys [array-size layout]}]
        (cond->> args
          (not (string? (first args))) (cons nil))
        [src deps] (compile-global (ensure-ns sym)
                                   type
                                   "uniform"
                                   :layout layout
                                   :array-size array-size)]
    `(def ~sym ~@(when docstring [docstring])
       {::type :uniform
        ::source ~src
        ::deps ~deps})))

(defmacro definterface
  ""
  {:arglists '([symbol docstring? structure-map & {:keys [layout instance-name array-size]}])}
  [sym & args]
  (let [[docstring structure-map & {:keys [layout array-size] :as opts}]
        (cond->> args
          (not (string? (first args))) (cons nil))
        instance-name (:instance-name opts ::not-found)
        instance-name (if (= instance-name ::not-found)
                        (ensure-ns sym)
                        instance-name)
        block-name (ensure-ns sym)]
    `(def ~sym ~@(when docstring [docstring])
       {::type :interface
        ::block-name '~block-name
        ::instance-name '~instance-name
        ::bindings '~structure-map
        ::opts ~(dissoc opts :instance-name)
        ::deps ~(into #{}
                      (comp (map (fn [v] (if (seq? v) (first v) v))) ; the item or the first element of the list
                            (filter symbol?)                         ; only look at symbols
                            (keep resolve))                          ; only the ones that resolve are new deps
                      (vals structure-map))})))

(defmacro defuniformbuffer
  ""
  {:arglists '([symbol docstring? structure-map & {:keys [layout instance-name array-size]}])}
  [sym & args]
  (let [[docstring structure-map & {:keys [layout array-size] :as opts}]
        (cond->> args
          (not (string? (first args))) (cons nil))
        instance-name (:instance-name opts ::not-found)
        instance-name (if (= instance-name ::not-found)
                        (ensure-ns sym)
                        instance-name)
        [src deps] (compile-block (ensure-ns (symbol (namespace sym) (str (name sym) "_UNIFORM")))
                                  "uniform"
                                  structure-map
                                  :instance-name instance-name
                                  :layout layout
                                  :array-size array-size)]
    `(def ~sym ~@(when docstring [docstring])
       {::type :uniform
        ::source ~src
        ::deps ~deps})))

(defmacro defstruct
  ""
  {:arglists '([symbol docstring? structure-map & {:keys [layout]}])}
  [sym & args]
  (let [[docstring structure-map & {:keys [layout]}]
        (cond->> args
          (not (string? (first args))) (cons nil))
        [src deps] (compile-block (ensure-ns sym)
                                  "struct"
                                  structure-map
                                  :layout layout)]
    `(def ~sym ~@(when docstring [docstring])
       {::type :struct
        ::source ~src
        ::deps ~deps})))

(defmacro defshaderfn
  ""
  {:arglists '([symbol docstring? [params*] & body])}
  [sym & args]
  (let [[docstring params & body]
        (cond->> args
          (not (string? (first args))) (cons nil))
        [src deps] (compile-function (ensure-ns sym)
                                     params
                                     (:tag (meta params))
                                     body)]
    `(def ~sym ~@(when docstring [docstring])
       {::type :function
        ::source ~src
        ::deps ~deps})))

(defn- realize-param
  ""
  [param storage]
  (apply compile-global (::name param) (::param-type param) storage (mapcat identity (::opts param))))

(defn- realize-interface
  ""
  [interface storage]
  (apply compile-block (symbol (namespace (::block-name interface))
                               (str (name (::block-name interface)) "_INTERFACE"))
         storage (::bindings interface)
         (concat (mapcat identity (::opts interface)) (list :instance-name
                                                            (::block-name interface)))))

(defn collect-deps
  ""
  [start-deps]
  (letfn [(deps-seq [deps]
            (when deps
              (concat (mapcat (comp deps-seq ::deps deref) deps) deps)))]
    (distinct (deps-seq start-deps))))

(defmacro defshader
  ""
  {:arglists '([symbol docstring? {in-out-param :in-or-out} & body])}
  [sym & args]
  (let [[docstring bindings & body]
        (cond->> args
          (not (string? (first args))) (cons nil))
        bindings (reduce-kv (fn [m k v] (assoc m (ensure-ns k) v))
                            {} bindings)
        [src deps] (compile-function 'main nil nil body)
        deps (group-by ::type (map deref (collect-deps deps)))]
    `(def ~sym ~@(when docstring [docstring])
       ~(str/join "\n\n"
                  [(str/join (map ::source (:struct deps)))
                   (str/join (map ::source (:const deps)))
                   (str/join (map ::source (:uniform deps)))
                   (str/join (map (fn [param]
                                    (when-not (bindings (::name param))
                                      (throw
                                       (ex-info "parameters require a storage specifier"
                                                {:param (::name param)})))
                                    (first (realize-param param (name (bindings (::name param))))))
                                  (:param deps)))
                   (str/join (map (fn [interface]
                                    (when-not (bindings (::block-name interface))
                                      (throw
                                       (ex-info "interface blocks require a storage specifier"
                                                {:interface (::block-name interface)})))
                                    (first (realize-interface interface
                                                              (name (bindings (::block-name interface))))))
                                  (:interface deps)))
                   (str/join "\n" (map ::source (:function deps)))
                   src]))))
