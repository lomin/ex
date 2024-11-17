(ns me.lomin.ex
  (:require [com.rpl.specter :as sp]
            [malli.core :as m]
            [malli.generator :as mg]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit]
            [clojure.walk :as walk]))

(def system {})

(declare catch-clause-validator)
(declare finally-clause-validator)

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

(def ^:private ex-meta-key? #{:ex.gen/exits
                              :ex.gen/samples
                              :malli/schema
                              :ex.gen/provider})

(defn- ex-fn-var?? [a-var]
  (when (and a-var (ifn? a-var) (some ex-meta-key? (keys (meta a-var))))
    a-var))

(defn- ex-fn-var2?? [a-var]
  (and a-var (ifn? a-var) (:ex/id (meta a-var))))

(defn generate-fn
  [a-var choice]
  (let [meta*   (meta a-var)
        choices (into #{:ex.gen/default}
                      (filter ex-meta-key?)
                      (keys meta*))]
    (if (= :ex.gen/random choice)
      (recur a-var (-> choices
                       (cond->
                         (not (choices :ex.gen/samples)) (disj :ex.gen/provider)
                         :always (disj :ex.gen/random))
                       (vec)
                       (rand-nth)))
      (case (choices choice)
        :ex.gen/exits (fn [& _args] (let [ex (rand-nth (vec (:ex.gen/exits meta*)))]
                                      (exit ex (str ex))))
        :ex.gen/samples (fn [& _args] (rand-nth (vec (:ex.gen/samples meta*))))
        :ex.gen/provider (fn [& args] (mg/generate ((:ex.gen/provider meta*)
                                                    (conj (:ex.gen/samples meta*)
                                                          (apply a-var args)))))
        :malli/schema (mg/generate (:malli/schema meta*))
        a-var))))

(defmacro with-random [& body]
  `(with-redefs [system #(generate-fn %2 :ex.gen/random)]
     (let [result# (do ~@body)]
       result#)))

(defn ->fn [a-var system]
  (let [f (get system (symbol a-var))]
    (cond
      #_? f
      #_=> f
      #_? (:ex/gen system)
      #_=> (generate-fn a-var (:ex/gen system))
      :else a-var)))

(defn- replace-ex-fns [system body]
  (walk/postwalk
    (fn [x]
      (if (and (symbol? x) (ex-fn-var?? (resolve x)))
        `(->fn (var ~x) ~system)
        x))
    body))

(defmacro try-with [system & body]
  (let [body' (replace-ex-fns system body)]
    `(try+ ~@body')))

(defn- replace-ex-fns2 [body]
  (walk/postwalk
    (fn [x]
      (if-let [replace (and (seq? x) (and (symbol? (first x))) (:ex/replace (meta (resolve (first x)))))]
        (replace (rest x))
        (if-let [id (and (symbol? x) (ex-fn-var2?? (resolve x)))]
          `(system ~id (var ~x))
          x)))
    body))

(defmacro protect-var [sym-str]
  `(var ~(symbol sym-str)))

(defn replace-ex-fns3
  ([body] (replace-ex-fns3 body 'me.lomin.ex/system))
  ([body sym]
   (walk/postwalk
     (fn [x]
       (if-let [replace (and (seq? x) (and (symbol? (first x))) (:ex/replace (meta (resolve (first x)))))]
         (replace (rest x))
         (if-let [id (and (symbol? x) (ex-fn-var2?? (resolve x)))]
           (cons sym `(~id (protect-var ~(str x))))
           x)))
     body)))

(defn replace-ex-fns4
  ([config body env]
   (walk/postwalk
     (fn [x]
       (if-let [replace (and (seq? x) (and (symbol? (first x))) (:ex/replace (meta (resolve (first x)))))]
         (replace (first x) (rest x) config env)
         x))
     body)))

(defmacro with-try [& body]
  (let [body' (replace-ex-fns2 body)]
    `(try+ ~@body')))

(defmacro with-examples
  {:ex/replace (fn [args] (cons 'do (rest args)))}
  [bindings & body]
  `(let ~bindings ~@body))

(defmacro example
  {:ex/replace first}
  [sym example]
  (if (and (symbol? sym) (or (resolve sym) (get &env sym))) sym example))

(comment
  (with-try
    (let [x 2]
      (example x 1))))


(defn init [sys]
  (alter-var-root #'me.lomin.ex/system (fn [_] sys))
  sys)

(defn no-macro-expand
  ([form] form)
  ([form env] form))

(defn analyze [body local-env extra-env]
  (let [env (assoc (ana.jvm/empty-env)
              :locals (merge (reduce-kv (fn [m k v]
                                          (assoc m k {:op    :binding
                                                      :name  (.-sym v)
                                                      :form  (.-sym v)
                                                      :local :let}))
                                        {}
                                        local-env)
                             extra-env))]
    (ana.jvm/analyze body env (or {} {:bindings {#'ana/macroexpand-1 no-macro-expand}}))))

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

(defmacro with-ex [& [config & body]]
  (let [body' (replace-ex-fns4 config body &env)]
    `(try+ ~@body')))

(defn replace-exchange [_ [trace-key sym & {:as opts}] config _env]
  (let [delayed-opts (reduce-kv (fn [m k v] (assoc m k (list 'delay v))) {} opts)]
    `(let [trace!#  (get ~config :ex/trace!)
           context# {:ex.ex/form   (quote ~sym)
                     :ex.ex/config ~config
                     :ex.ex/code   (resolve-shallow ~sym)}
           result#  (or (when-let [selector# (get ~config ~trace-key)]
                          (if trace!#
                            (try
                              (let [result# (selector# ~delayed-opts context#)]
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
  [_trace-key sym & {:as opts}]
  (if (deeply-resolvable? &env sym)
    sym
    (rand-nth (vals opts))))

(defn destruct [var-or-expr name-or-expr body]
  (if (seq body)
    (if (symbol? name-or-expr)
      {:ex.as/expr  var-or-expr
       :ex.as/name  name-or-expr
       :ex.as/forms body}
      {:ex.as/expr  var-or-expr
       :ex.as/name  var-or-expr
       :ex.as/forms (into [name-or-expr] body)})
    (if (symbol? var-or-expr)
      {:ex.as/expr  var-or-expr
       :ex.as/name  var-or-expr
       :ex.as/forms [name-or-expr]}
      {:ex.as/expr  var-or-expr
       :ex.as/name  name-or-expr
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

(defn transform-ast [replacement name ast]
  (ast/prewalk ast
               (fn [m]
                 (if (= (:form m) name)
                   replacement
                   m))))

(defmacro with-ex2 [var-or-expr name-or-expr & body]
  (let [sym      (gensym "with_ex_")
        destr    (destruct var-or-expr name-or-expr body)
        forms    (:ex.as/forms destr)
        body'    `(try+ ~@(replace-ex-fns4 sym forms &env))
        analysis (try (transform-ast (->replacement-ast-node sym) (:ex.as/name destr) (analyze body' &env {sym (->replacement-ast-node sym)}))
                      (catch Exception _
                        (analyze body' &env {(:ex.as/name destr) (->replacement-ast-node sym)
                                             sym                 (->replacement-ast-node sym)})))]
    `(as-> (update-context ~(:ex.as/expr destr)) ~sym
           ~(emit/emit-hygienic-form analysis))))
