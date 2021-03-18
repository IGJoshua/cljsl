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
   :exclude [compile]))

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
          (let [[compiled deps] (compile-fn-call form env)]
            [compiled (conj deps var)])))

      (symbol? (first form))
      (compile-fn-call (cons (symbol (name (first form)))
                             (rest form))
                       env))
    [(compile-atom form)
     (or (when (symbol? form)
           (when-let [var (resolve env form)]
             #{var}))
         #{})]))

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
  (let [[initexpr deps] (compile initexpr env)]
    [(str (compile-atom var) "=" initexpr) deps]))

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
   (let [[initexpr deps] (when init
                           (compile init env))]
     [(str mods
           " "
           (if (symbol? type)
             (compile-atom type)
             (str->ident type))
           " "
           (compile-atom var)
           (when initexpr
             (str "=" initexpr)))
      deps])))

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
  (let [bindings (map var-declaration
                      (map (comp :tag meta) args)
                      (map (comp :mods meta) args)
                      args
                      (repeat nil)
                      (repeat {}))
        bindings-src (map first bindings)
        bindings-deps (reduce set/union (map second bindings))
        body (compile (cons 'do body) {})
        body-src (first body)
        body-deps (second body)]
    [(str (if ret
            (if (string? ret)
              (compile-atom (symbol ret))
              (compile-atom ret))
            "void")
          " "
          (compile-atom name)
          "("
          (str/join "," bindings-src)
          ")\n"
          "{\n"
          body-src
          "}\n")
     (set/union bindings-deps body-deps)]))
