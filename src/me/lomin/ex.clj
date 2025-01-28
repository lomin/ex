(ns me.lomin.ex
  (:require [com.rpl.specter :as sp]
            [malli.core :as m]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit]
            [clojure.walk :as walk]))

(declare catch-clause-validator)
(declare finally-clause-validator)

(defmulti ex (fn [_context k] k))

(defn- token-schema [token?] (m/-simple-schema {:type :catch, :pred token?}))

(def ^:private registry
  (merge
    (m/base-schemas)
    (m/type-schemas)
    (m/sequence-schemas)
    {:catch          (token-schema #{'catch})
     :catch-clause   [:catn
                      [:token :catch]
                      [:exits+exceptions [:+ [:altn
                                              [:exit :keyword]
                                              [:ex-info-navigators [:vector :any]]
                                              [:exception :class-symbol]]]]
                      [:binding :any]
                      [:body [:* :any]]]
     :finally        (token-schema #{'finally})
     :finally-clause [:catn
                      [:token :finally]
                      [:body [:* :any]]]
     :not-a-token    (m/-simple-schema {:type :not-a-token
                                        :pred #(not (or (catch-clause-validator %) (finally-clause-validator %)))})
     :class-symbol   (m/-simple-schema {:type :class-symbol, :pred #(and (symbol? %) (isa? (resolve %) Exception))})}))

(def ^:private catch-clause-validator (m/validator :catch-clause {:registry registry}))
(def ^:private finally-clause-validator (m/validator :finally-clause {:registry registry}))


(def ^:private try-schema
  [:catn
   [:body [:+ :not-a-token]]
   [:catch-clauses [:* [:schema :catch-clause]]]
   [:finally-clause [:? [:schema :finally-clause]]]])

(def parse-try (m/parser try-schema {:registry registry}))

(defn ex- [message cause data]
  (proxy [java.lang.RuntimeException clojure.lang.IExceptionInfo] [message cause true false]
    (getData [] data)))

(def ex-class (class (ex- nil nil nil)))

(defn exit
  ([type] (exit type nil))
  ([type msg] (exit type msg nil))
  ([type msg data] (exit type msg data nil))
  ([type msg data cause]
   (throw (ex- msg
               cause
               (cond-> {:type type}
                       msg (assoc :exit/msg msg)
                       cause (assoc :exit/cause cause)
                       :always (merge data))))))

(defn- select-ex-type [exits+exceptions ex-type]
  (->> exits+exceptions
       (group-by first)
       (ex-type)
       (map second)))

(defn- ->exception-clauses [clauses]
  (for [{:keys [token exits+exceptions binding body]} clauses
        e (select-ex-type exits+exceptions :exception)]
    `(~token ~e ~binding ~@body)))

(defn- ->exit-clauses [clauses data-sym]
  (sequence
    cat
    (for [{:keys [exits+exceptions binding body]} clauses
          exit (select-ex-type exits+exceptions :exit)]
      `[(isa? (:type ~data-sym) ~exit)
        (let [~binding ~data-sym]
          ~@body)])))

(defn- ->exit-dispatch [clauses]
  (let [data-sym (gensym "ex-data-")]
    (when-let [exit-clauses (seq (->exit-clauses clauses data-sym))]
      [`(catch ~ex-class ex-sym#
          (let [~data-sym (ex-data ex-sym#)]
            (cond
              ~@exit-clauses
              :else (throw ex-sym#))))])))

(defn- ->ex-info-clauses [clauses data-sym ex-sym]
  (sequence
    cat
    (for [{:keys [exits+exceptions binding body]} clauses
          exit (select-ex-type exits+exceptions :ex-info-navigators)]
      `[(sp/select-first ~exit ~data-sym)
        (let [~binding (merge ~data-sym {:ex/selected (sp/select-first ~exit ~data-sym)
                                         :cause       (ex-cause ~ex-sym)
                                         :message     (ex-message ~ex-sym)})]
          ~@body)])))

(defn- ->ex-info-dispatch [clauses]
  (let [data-sym (gensym "ex-data-")
        ex-sym   (gensym "ex-ex-info")]
    (when-let [exit-clauses (seq (->ex-info-clauses clauses data-sym ex-sym))]
      [`(catch clojure.lang.ExceptionInfo ~ex-sym
          (let [~data-sym (ex-data ~ex-sym)]
            (cond
              ~@exit-clauses
              :else (throw ~ex-sym))))])))

(defn- ->finally-clause [{:keys [token body] :as clause}]
  (when clause
    [`(~token ~@body)]))

(defmacro try+
  [& try-body]
  (let [parsed-try (parse-try try-body)]
    `(try
       ~@(:body parsed-try)
       ~@(-> []
             (into (->exit-dispatch (:catch-clauses parsed-try)))
             (into (->ex-info-dispatch (:catch-clauses parsed-try)))
             (into (->exception-clauses (:catch-clauses parsed-try)))
             (into (->finally-clause (:finally-clause parsed-try)))))))

(defn replace-ex-fns
  ([context body env]
   (walk/postwalk
     (fn [x]
       (if-let [replace (and (seq? x) (and (symbol? (first x))) (:ex/replace (meta (resolve (first x)))))]
         (replace (first x) (rest x) context env)
         x))
     body)))

(defmacro with-examples
  {:ex/replace (fn [_ args _context _env] (cons 'do (rest args)))}
  [bindings & body]
  `(let ~bindings ~@body))

(defmacro example
  {:ex/replace (fn [_ args _context _env] (first args))}
  [sym example]
  (if (and (symbol? sym) (or (resolve sym) (get &env sym))) sym example))

(defn analyze [body local-env extra-env]
  (let [env (assoc (ana.jvm/empty-env)
              :locals (merge (reduce-kv (fn [m k v]
                                          (assoc m k (if (:op v)
                                                       v
                                                       {:op    :binding
                                                        :name  (.-sym v)
                                                        :form  (.-sym v)
                                                        :local :let})))
                                        {}
                                        local-env)
                             extra-env))]
    (ana.jvm/analyze body env {})))

(defn deeply-resolvable? [env body]
  (some?
    (try
      (analyze body env {})
      (catch Exception _))))

(defn resolve-shallow* [env x]
  (cond (symbol? x)
        (if (get env x)
          x
          (list 'quote x))
        (list? x)
        (cons 'list (map (partial resolve-shallow* env) x))
        :else x))

(defmacro resolve-shallow [x]
  `~(resolve-shallow* &env x))


(defmethod ex :default [context k]
  (k (:ex.ex/opts context) context))

(defn replace-exchange [_ [trace-key sym & {:as opts}] context _env]
  (let [delayed-opts (reduce-kv (fn [m k v] (assoc m k (list 'delay v))) {} opts)]
    `(let [trace!#  (get ~context :ex/trace!)
           context# (merge ~context {:ex.ex/form (quote ~sym)
                                     :ex.ex/code (resolve-shallow ~sym)})
           result#  (or (when-let [selector# (get ~context ~trace-key)]
                          (if trace!#
                            (try
                              (let [result# (me.lomin.ex/ex (-> context#
                                                                (assoc :ex.ex/opts ~delayed-opts)
                                                                (assoc :ex.ex/trace-key ~trace-key))
                                                            selector#)]
                                (if (delay? result#) @result# result#))
                              (catch Exception e#
                                (trace!# [~trace-key (assoc context# :ex.ex/result e#)])
                                (throw e#)))
                            (let [result# (selector# ~delayed-opts context#)]
                              (if (delay? result#) @result# result#))))
                        ~sym)]
       (when trace!#
         (trace!# [~trace-key (assoc context# :ex.ex/result result#)]))
       result#)))

(defmacro exchange {:ex/replace replace-exchange}
  [_trace-key form & {:as opts}]
  (if-let [options (seq (filter (partial deeply-resolvable? &env) (conj (vals opts) form)))]
    (rand-nth options)
    ::unresolvable))

(defn destruct [var-or-expr name-or-expr body]
  (if (seq body)
    (if (symbol? name-or-expr)
      {:ex.as/expr  var-or-expr
       :ex.as/sym   name-or-expr
       :ex.as/forms body}
      {:ex.as/expr  var-or-expr
       :ex.as/sym   var-or-expr
       :ex.as/forms (into [name-or-expr] body)})
    (if (symbol? var-or-expr)
      {:ex.as/expr  var-or-expr
       :ex.as/sym   var-or-expr
       :ex.as/forms [name-or-expr]}
      {:ex.as/expr  var-or-expr
       :ex.as/sym   name-or-expr
       :ex.as/forms [name-or-expr]})))

(defn ->replacement-ast-node [sym]
  {:children    [],
   :name        sym,
   :op          :local,
   :env         {},
   :o-tag       nil,
   :form        sym,
   :tag         nil,
   :local       :let,
   :assignable? false})

(defn update-context [ctx]
  (-> ctx
      (assoc :ex.trace/parent-id (:ex.trace/id ctx))
      (assoc :ex.trace/id ((:ex/generate-id! ctx random-uuid)))))

(defn transform-ast [ex-sym name ast]
  (ast/prewalk ast
               (fn [m]
                 (if (= (:name m) name)
                   (assoc m :name ex-sym)
                   m))))

(defmacro with-ex [var-or-expr name-or-expr & body]
  (let [ex-sym   (gensym "with_ex_")
        {:ex.as/keys [sym forms expr]} (destruct var-or-expr name-or-expr body)
        body'    `(try+ ~@(replace-ex-fns ex-sym forms &env))
        analysis (try (transform-ast ex-sym
                                     sym
                                     (analyze body' &env {ex-sym (->replacement-ast-node ex-sym)}))
                      (catch Exception _
                        (analyze body' &env {ex-sym (->replacement-ast-node ex-sym)
                                             sym    (->replacement-ast-node ex-sym)})))]
    `(as-> (update-context ~expr) ~ex-sym
           ~(emit/emit-hygienic-form analysis))))
