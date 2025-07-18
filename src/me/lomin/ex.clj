(ns me.lomin.ex
  (:require
   [clojure.tools.analyzer.ast :as ast]
   [clojure.tools.analyzer.jvm :as ana.jvm]
   [clojure.tools.analyzer.passes.jvm.emit-form :as emit]
   [clojure.walk :as walk]
   [com.rpl.specter :as sp]
   [malli.core :as m]))


(declare catch-clause-validator)
(declare finally-clause-validator)

(defmulti ex (fn [_context k] k))


(defn- token-schema
  [token?]
  (m/-simple-schema {:type :catch, :pred token?}))


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


(def parse-try (comp :values (m/parser try-schema {:registry registry})))


(defn ex-
  [message cause data]
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


(defn- select-ex-type
  [exits+exceptions ex-type]
  (->> exits+exceptions
       (group-by :key)
       (ex-type)
       (map :value)))


(defn ->exception-clauses
  [{clauses :catch-clauses}]
  (for [{:keys [token exits+exceptions binding body]} (map :values clauses)
        e (select-ex-type exits+exceptions :exception)]
    `(~token ~e ~binding ~@body)))


(defn- ->exit-clauses
  [clauses data-sym]
  (sequence
   cat
   (for [{:keys [exits+exceptions binding body]} (map :values clauses)
         exit (select-ex-type exits+exceptions :exit)]
     `[(isa? (:type ~data-sym) ~exit)
       (let [~binding ~data-sym]
         ~@body)])))


(defn ->exit-dispatch
  [{clauses :catch-clauses}]
  (let [data-sym (gensym "ex-data-")]
    (when-let [exit-clauses (seq (->exit-clauses clauses data-sym))]
      [`(catch ~ex-class ex-sym#
          (let [~data-sym (ex-data ex-sym#)]
            (cond
              ~@exit-clauses
              :else (throw ex-sym#))))])))


(defn- ->ex-info-clauses
  [clauses data-sym ex-sym]
  (sequence
   cat
   (for [{:keys [exits+exceptions binding body]} clauses
         exit (select-ex-type exits+exceptions :ex-info-navigators)]
     `[(sp/select-first ~exit ~data-sym)
       (let [~binding (merge ~data-sym {:ex/selected (sp/select-first ~exit ~data-sym)
                                        :cause       (ex-cause ~ex-sym)
                                        :message     (ex-message ~ex-sym)})]
         ~@body)])))


(defn ->ex-info-dispatch
  [{clauses :catch-clauses}]
  (let [data-sym (gensym "ex-data-")
        ex-sym   (gensym "ex-ex-info")]
    (when-let [exit-clauses (seq (->ex-info-clauses (:values clauses) data-sym ex-sym))]
      [`(catch clojure.lang.ExceptionInfo ~ex-sym
          (let [~data-sym (ex-data ~ex-sym)]
            (cond
              ~@exit-clauses
              :else (throw ~ex-sym))))])))


(defn ->finally-clause
  [{finally-clause :finally-clause}]
  (let [{:keys [token body]} (:values finally-clause)]
    (when finally-clause
      [`(~token ~@body)])))


(defn- build-try-from-parsed
  [parsed-try]
  `(try
     ~@(:body parsed-try)
     ~@(-> []
           (into (->exit-dispatch parsed-try))
           (into (->ex-info-dispatch parsed-try))
           (into (->exception-clauses parsed-try))
           (into (->finally-clause parsed-try)))))

(defmacro try+
  [& try-body]
  (build-try-from-parsed (parse-try try-body)))

(defn replace-ex-fns
  "Recursively replace forms that have :ex/replace metadata until no more replacements are possible."
  ([context body env]
   (loop [result body]
     (let [replaced (walk/postwalk
                     (fn [form]
                       (if (and (seq? form) (symbol? (first form)))
                         (if-let [replace-fn (:ex/replace (meta (resolve (first form))))]
                           (replace-fn (first form) (rest form) context env)
                           form)
                         form))
                     result)]
       (if (= result replaced)
         result  ; No more replacements possible
         (recur replaced))))))


(defmacro with-examples
  {:ex/replace (fn [_ args _context _env] (cons 'do (rest args)))}
  [bindings & body]
  `(let ~bindings ~@body))


(defmacro example
  {:ex/replace (fn [_ args _context _env] (first args))}
  [_sym example]
  example)


(defn analyze
  [body local-env extra-env]
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


(defn deeply-resolvable?
  [env body]
  (some?
   (try
     (analyze body env {})
     (catch Exception _))))


(defn resolve-shallow*
  [env x]
  (cond (symbol? x)
        (if (get env x)
          x
          (list 'quote x))
        (list? x)
        (cons 'list (map (partial resolve-shallow* env) x))
        :else x))


(defmacro resolve-shallow
  [x]
  `~(resolve-shallow* &env x))


(defmethod ex :default [context k]
  (k (:ex.ex/opts context) context))


(defn replace-exchange
  [_ [trace-key sym & {:as opts}] context _env]
  (let [delayed-opts (reduce-kv (fn [m k v] (assoc m k (list 'delay v))) {} opts)]
    `(let [trace!#  (get ~context :ex/trace!)
           context# (merge ~context {:ex.ex/form (quote ~sym)
                                     :ex.ex/code (resolve-shallow ~sym)})
           result#  (if (contains? ~context ~trace-key)
                      (let [selector# (get ~context ~trace-key)]
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


(defmacro exchange
  {:ex/replace replace-exchange}
  [_trace-key form & {:as opts}]
  (if-let [options (seq (filter (partial deeply-resolvable? &env) (conj (vals opts) form)))]
    (rand-nth options)
    ::unresolvable))


(defn destruct
  [bindings-or-expr & body]
  (if (vector? bindings-or-expr)
    (let [[system-sym system-expr] bindings-or-expr]
      {:ex.as/expr          system-expr
       :ex.as/sym           system-sym
       :ex.as/other-bindings (subvec bindings-or-expr 2)
       :ex.as/forms         body})
    {:ex.as/expr          bindings-or-expr
     :ex.as/sym           bindings-or-expr
     :ex.as/other-bindings []
     :ex.as/forms         body}))


(defn ->replacement-ast-node
  [sym]
  {:children    [],
   :name        sym,
   :op          :local,
   :env         {},
   :o-tag       nil,
   :form        sym,
   :tag         nil,
   :local       :let,
   :assignable? false})


(defn update-context
  [ctx]
  (-> ctx
      (assoc :ex.trace/parent-id (:ex.trace/id ctx))
      (assoc :ex.trace/id ((:ex/generate-id! ctx random-uuid)))))


(defn transform-ast
  [ex-sym name ast]
  (ast/prewalk ast
               (fn [m]
                 (if (= (:name m) name)
                   (assoc m :name ex-sym)
                   m))))


(defmacro with-ex
  [bindings-or-expr & body]
  (let [ex-sym   (gensym "with_ex_")
        {:ex.as/keys [sym forms expr other-bindings]} (apply destruct bindings-or-expr body)
        body'    (if (seq other-bindings)
                   (let [parsed-try (parse-try (replace-ex-fns ex-sym forms &env))
                         modified-parsed (assoc parsed-try :body
                                                [`(let ~other-bindings
                                                    ~@(:body parsed-try))])]
                     (build-try-from-parsed modified-parsed))
                   `(try+ ~@(replace-ex-fns ex-sym forms &env)))
        analysis (try (transform-ast ex-sym
                                     sym
                                     (analyze body' &env {ex-sym (->replacement-ast-node ex-sym)}))
                      (catch Exception _
                        (analyze body' &env {ex-sym (->replacement-ast-node ex-sym)
                                             sym    (->replacement-ast-node ex-sym)})))]
    `(as-> (update-context ~expr) ~ex-sym
       ~(emit/emit-hygienic-form analysis))))
