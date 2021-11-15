(ns cljsl.compiler
  "Compiles a subset of Clojure data, interpreted as a lisp into GLSL."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   [clojure.string :as str])
  (:import
   (clojure.lang Symbol))
  (:refer-clojure
   :exclude [compile definterface defstruct]))

(def digit?
  "All the digit characters."
  #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0})

(defn str->ident
  "Converts a string into a GLSL-compliant identifier.

  Strings starting with digits are prefixed with an underscore."
  [s]
  (apply str
         (cond->>
             (sequence (comp (replace {\- \_ \space \_ \newline \_})
                             (map (fn [c]
                                    (if (re-matches #"[0-9a-zA-Z_]" (str c))
                                      (str c)
                                      (str (apply str "_" (replace {\space \_ \( \_ \) \_}
                                                                   (Character/getName (int c))))
                                           "_")))))
                       (seq s))
           (digit? (first s)) (cons \_))))

(defn sym->ident
  "Converts a [[Symbol]] into a GLSL-compliant identifier.

  The resulting identifier includes both the namespace and the name."
  [s]
  (if (namespace s)
    (str "NS_" (str->ident (namespace s)) "_SYM_" (str->ident (name s)))
    (str->ident (name s))))

(defprotocol Atom
  "Converts an object to a GLSL string used to compile."
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

  Boolean
  (compile-atom [a]
    (str a))

  Symbol
  (compile-atom [a]
    (sym->ident a)))

(declare ^:private special-forms compile)

(defn- compile-fn-call
  "Constructs a GLSL function call for a form."
  [form env]
  (let [compiled (map #(compile % env) (rest form))
        args (map first compiled)
        deps (reduce set/union (map second compiled))]
    [(str (compile-atom (first form)) "(" (str/join ", " args) ")")
     deps]))

(defn- compile-field-access
  "Constructs a GLSL struct field access for a form."
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
  "Compiles a `form` to GLSL expressions.

  Returns a vector of the source string and a set of vars that the code depends
  on.

  `env` is a map with symbol keys representing local variables. The values are
  an implementation detail and should not be relied upon."
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
    (or
      (when-let [var (when (symbol? form)
                       (resolve env form))]
        [(compile-atom (ensure-ns form)) #{var}])
      [(compile-atom form) #{}])))

;; ==================================================================
;; Builtin functions and special forms

(defn- cljsl-if
  ([form env test then]
   (cljsl-if form env test then nil))
  ([_form env test then else]
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
  [_form env & clauses]
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
  [_form env var initexpr]
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
  [_form env & body]
  (let [compiled (map #(compile % env) body)
        statements (map first compiled)
        deps (reduce set/union (map second compiled))]
    [(str (str/join ";\n" statements)
          ";\n")
     deps]))

(defn- var-declaration
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
  [_form env bindings & body]
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
  [_form env binding test & body]
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
     (reduce set/union #{} (list* decl-deps test-deps inc-deps body-deps))]))

(defn- infix-op
  "Constructs an infix operation compilation function out of `op`."
  [op]
  (fn [_form env & args]
    (let [compiled (map #(compile % env) args)
          exprs (map first compiled)
          deps (reduce set/union (map second compiled))]
      [(str "("
            (str/join op exprs)
            ")")
       deps])))

(defn- unary-minus
  [_form env arg]
  (let [[expr deps] (compile arg env)]
    [(str "(-(" expr "))")
     deps]))

(def ^:private nary-minus (infix-op "-"))

(defn- cljsl-minus
  "Compiles a minus operation conditionally as either unary or infix."
  [form env & args]
  (if (= 1 (count args))
    (unary-minus form env (first args))
    (apply nary-minus form env args)))

(defn- grouped-infix
  "Constructs an infix operation function which combines using `grouping` out of `op`."
  [op grouping]
  (fn [_form env & args]
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
  ([_form env val]
   (let [[expr deps] (compile val env)]
     [(str "return " expr)
      deps]))
  ([_form _env]
   ["return" #{}]))

(defn- cljsl-break
  [_form _env]
  ["break" #{}])

(defn- cljsl-continue
  [_form _env]
  ["continue" #{}])

(defn- cljsl-discard
  [_form _env]
  ["discard" #{}])

(defn- cljsl-cast
  [_form env value type]
  (let [[compiled form-deps] (compile value env)
        type-deps (if (symbol? type)
                    #{(resolve type)}
                    #{})]
    [(str "("
          (if (symbol? type)
            (compile-atom (ensure-ns type))
            (str->ident type))
          ")("
          compiled
          ")")
     (set/union form-deps type-deps)]))

(def ^:private special-forms
  "Map from symbols to compilation functions."
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
   'cast #'cljsl-cast
   'float (fn [form env val]
            (if (number? val)
              [(compile-atom (float val)) #{}]
              (cljsl-cast form env val "float")))
   'double (fn [form env val]
             (if (number? val)
               [(compile-atom (double val)) #{}]
               (cljsl-cast form env val "double")))
   'int (fn [form env val]
          (if (number? val)
            [(compile-atom (int val)) #{}]
            (cljsl-cast form env val "int")))
   'uint (fn [form env val]
          (if (number? val)
            [(compile-atom (unchecked-int val)) #{}]
            (cljsl-cast form env val "uint")))
   'boolean (fn [form env val]
              (if (number? val)
                [(compile-atom (boolean val)) #{}]
                (cljsl-cast form env val "bool")))
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
  "Compiles a function with the given `name` and `args` with the `ret` type from the `body`.

  Returns a vector of the source string and a set of vars that the code depends
  on."
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
  "Construct a layout string to attach to a uniform declaration."
  [layout]
  (str "layout(" (str/join "," (map #(str (key %) (when-let [v (val %)] (str "=" v))) layout)) ")"))

(defn compile-global
  "Compile a global variable from `var-name` with the given `type` and `storage` qualifier.

  Returns a vector of the source string and a set of vars that the code depends
  on, in this case only types.

  If `array-size` is `:variable`, then it will produce a variable-sized array,
  suitable for use only in shader storage objects."
  [var-name type storage & {:keys [layout invariant? interpolation array-size init memory-qualifier]}]
  (when-not var-name
    (throw (ex-info "globals require a name" {})))
  (when-not type
    (throw (ex-info "globals require a type"
                    {:var var-name})))
  (let [[initexpr init-deps] (when init (compile init {}))
        [arr-expr arr-deps] (when array-size
                              (if (= array-size :variable)
                                [:variable #{}]
                                (compile array-size {})))
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
          " " (sym->ident var-name) (when arr-expr
                                      (str "[" (when-not (= arr-expr :variable)
                                                 arr-expr)
                                           "]"))
          " " (when init (str "=" initexpr))";\n")
     (or (set/union init-deps arr-deps type-deps) #{})]))

(defn compile-block
  "Compiles a uniform or interface block with the given `block-name` and `storage` qualifier.

  The `bindings` are used to construct the members of the block.

  Returns a vector of the source string and a set of vars that the code depends
  on, in this case only types."
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

(s/def ::type-name (s/or :native string?
                         :user-defined symbol?))
(s/def ::array-size nat-int?)

(defmacro defconst
  "Defines a constant for use in shaders.

  The `type` is a string naming a GLSL type, or a symbol naming a var
  representing a type.

  See [[defstruct]]."
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
(s/fdef defconst
  :args (s/cat :sym simple-symbol?
               :type ::type-name
               :docstring (s/? string?)
               :init any?
               :kwargs (s/keys* :opt-un [::array-size])))

(s/def ::interpolation string?)
(s/def ::layout (s/map-of string? any?))
(s/def ::invariant? boolean?)

(defmacro defparam
  "Define an in/out parameter to pass values between shader stages.

  The `type` is a string naming a GLSL type, or a symbool naming a var
  representing a type.

  See [[defstruct]]."
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
(s/fdef defparam
  :args (s/cat :sym simple-symbol?
               :type ::type-name
               :docstring (s/? string?)
               :kwargs (s/keys* :opt-un [::array-size ::interpolation ::layout ::invariant?])))

(defmacro defuniform
  "Define a uniform variable to pass values from your program to the shader.

  The `type` is a string naming a GLSL type, or a symbol naming a var
  representing a type.

  The uniform identifier name to introspect the GL state and set the uniform can
  be constructed using [[sym->ident]].

  See [[defstruct]]."
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
(s/fdef defuniform
  :args (s/cat :sym simple-symbol?
               :type ::type-name
               :docstring (s/? string?)
               :kwargs (s/keys* :opt-un [::array-size ::layout])))

(s/def ::instance-name simple-symbol?)
(s/def ::type-with-args (s/cat :name ::type-name
                               :kwargs (s/keys* :opt-un [::layout])))

(defmacro definterface
  "Define an interface block for passing values between shader stages.

  The `structure-map` is a map from keywords to types for those fields."
  {:arglists '([symbol docstring? structure-map & {:keys [layout instance-name array-size]}])}
  [sym & args]
  (let [[docstring structure-map & {:keys [_layout _array-size] :as opts}]
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
(s/fdef definterface
  :args (s/cat :sym simple-symbol?
               :docstring (s/? string?)
               :structure-map (s/map-of simple-keyword? ::type-with-args)
               :kwargs (s/keys* :opt-un [::array-size ::layout ::instance-name])))

(defmacro defuniformbuffer
  "Define a uniform buffer for passing values from your program to the shader.

  The `structure-map` is a map from keywords to types for those fields."
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
(s/fdef defuniformbuffer
  :args (s/cat :sym simple-symbol?
               :docstring (s/? string?)
               :structure-map (s/map-of simple-keyword? ::type-name)
               :kwargs (s/keys* :opt-un [::array-size ::layout ::instance-name])))

(defmacro defstruct
  "Define a struct type for use defining variables and functions."
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
(s/fdef defstruct
  :args (s/cat :sym simple-symbol?
               :docstring (s/? string?)
               :structure-map (s/map-of simple-keyword? ::type-name)
               :kwargs (s/keys* :opt-un [::array-size ::layout ::instance-name])))

(defmacro defshaderfn
  "Define a function for use in shader programs.

  All parameters must be type hinted."
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
(s/fdef defshaderfn
  :args (s/cat :sym simple-symbol?
               :docstring (s/? string?)
               :params (s/coll-of (s/and simple-symbol?
                                         (comp :tag meta))
                                  :kind vector?)
               :body (s/* any?)))

(defn- realize-param
  "Take a specification for a parameter and turn it into a compiled GLSL string.

  Returns a vector of the string and a set of its dependencies."
  [param storage]
  (apply compile-global (::name param) (::param-type param) storage (mapcat identity (::opts param))))

(defn- realize-interface
  "Take a specification for an interface block and compiles it to a GLSL string.

  Returns a vector of the string and a set of its dependencies."
  [interface storage]
  (apply compile-block (symbol (namespace (::block-name interface))
                               (str (name (::block-name interface)) "_INTERFACE"))
         storage (::bindings interface)
         (concat (mapcat identity (::opts interface)) (list :instance-name
                                                            (::block-name interface)))))

(defn collect-deps
  "Collects all the dependencies for a starting set, including transitive deps."
  [start-deps]
  (letfn [(deps-seq [deps]
            (when deps
              (concat (mapcat (comp deps-seq ::deps deref) deps) deps)))]
    (distinct (deps-seq start-deps))))

(defmacro defshader
  "Define a shader stage.

  The var produced holds a map with the key `:cljsl.compiler/source` which holds
  the source string for the shader function.

  The metadata keys on `symbol` `:version` and `:extensions` are used to specify
  the GLSL version and extensions. `:version` is a number defaulting to 400, and
  `:extensions` is a list of strings, defaulting to a singleton list including
  only \"core\"."
  {:arglists '([symbol docstring? {in-out-param :in-or-out} & body])}
  [sym & args]
  (let [[docstring bindings & body]
        (cond->> args
          (not (string? (first args))) (cons nil))
        bindings (reduce-kv (fn [m k v] (assoc m (ensure-ns k) v))
                            {} bindings)
        [src deps] (compile-function 'main nil nil body)
        grouped-deps (group-by ::type (map deref (collect-deps deps)))
        src (str
             "#version " (or (:version (meta sym)) 400)
             " " (str/join " " (or (:extensions (meta sym))
                                   '("core")))
             "\n"
             (str/join "\n\n"
                       [(str/join (map ::source (:struct grouped-deps)))
                        (str/join (map ::source (:const grouped-deps)))
                        (str/join (map ::source (:uniform grouped-deps)))
                        (str/join (map (fn [param]
                                         (when-not (bindings (::name param))
                                           (throw
                                            (ex-info "parameters require a storage specifier"
                                                     {:param (::name param)})))
                                         (first (realize-param param (name (bindings (::name param))))))
                                       (:param grouped-deps)))
                        (str/join (map (fn [interface]
                                         (when-not (bindings (::block-name interface))
                                           (throw
                                            (ex-info "interface blocks require a storage specifier"
                                                     {:interface (::block-name interface)})))
                                         (first (realize-interface interface
                                                                   (name (bindings (::block-name interface))))))
                                       (:interface grouped-deps)))
                        (str/join "\n" (map ::source (:function grouped-deps)))
                        src]))]
    `(def ~sym ~@(when docstring [docstring])
       {::type :shader
        ::source ~src
        ::deps ~deps})))
(s/fdef defshader
  :args (s/cat :sym simple-symbol?
               :docstring (s/? string?)
               :bindings (s/map-of symbol? #{:in :out})
               :body (s/* any?)))
