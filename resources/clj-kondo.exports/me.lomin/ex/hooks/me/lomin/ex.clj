(ns hooks.me.lomin.ex
  (:require [clj-kondo.hooks-api :as api]))

(defn- catch-clause? [node]
  (and (api/list-node? node)
       (= 'catch (some-> node :children first api/sexpr))))

(defn- transform-catch-clause [catch-node]
  ;; Transform (catch :keyword binding body...) to (catch Exception binding body...)
  (let [children (:children catch-node)
        [catch-token _error-type binding & body] children]
    (api/list-node
     (list* catch-token
            (api/token-node 'Exception)
            binding
            body))))

(defn with-ex [{:keys [node]}]
  (let [children (rest (:children node))
        first-arg (first children)
        body-args (rest children)
        ;; Separate regular body from catch/finally clauses
        {body-forms :body catch-forms :catch} 
        (group-by #(if (catch-clause? %) :catch :body) body-args)
        ;; Transform catch clauses for clj-kondo
        transformed-catches (map transform-catch-clause catch-forms)]
    
    (if (and (api/vector-node? first-arg)
             (seq (:children first-arg)))
      ;; Vector binding syntax: [$ system x 1 y 2] body...
      (let [bindings (:children first-arg)
            binding-pairs (partition 2 bindings)
            let-bindings (api/vector-node (vec (mapcat identity binding-pairs)))
            let-form (api/list-node
                      (list*
                       (api/token-node 'let)
                       let-bindings
                       body-forms))]
        {:node (if (seq transformed-catches)
                 (api/list-node
                  (list* (api/token-node 'try)
                         let-form
                         transformed-catches))
                 let-form)})
      ;; Non-vector syntax: system body...
      (let [let-form (api/list-node
                      (list*
                       (api/token-node 'let)
                       (api/vector-node [(api/token-node '_system) first-arg])
                       body-forms))]
        {:node (if (seq transformed-catches)
                 (api/list-node
                  (list* (api/token-node 'try)
                         let-form
                         transformed-catches))
                 let-form)}))))