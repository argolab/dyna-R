(ns dyna-backend.utils
  (:require [aprint.core :refer [aprint]])
  )

;; make functions like car caar cdr etc

(defn- make-cr-function-body [args]
  (if (= args "") 'x
      (case (subs args 0 1)
        "a" `(clojure.core/first ~(make-cr-function-body (subs args 1)))
        "d" `(clojure.core/rest ~(make-cr-function-body (subs args 1))))))

(defn- make-cr-function [args]
  `(defn ~(symbol (str "c" args "r")) ~'[x]
     ~(make-cr-function-body (apply str (reverse args)))))

(defn- make-function-depth [depth]
  (if (= depth 0) (list "")
      (for [l ["a" "d"]
            s (make-function-depth (- depth 1))]
        (str l s))))

;; doall forces the loop to not be lazy, which creates the functions
(doall (for [i (range 1 10)
             func (make-function-depth i)]
         (eval (make-cr-function func))))


(defn ??? [] (throw (RuntimeException. "Not Implemented")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://gist.github.com/ato/252421

(declare ^:dynamic *locals*)
(defmacro debugger-get-local-bindings
  "Produces a map of the names of local bindings to their values."
  []
  (let [symbols (map key @clojure.lang.Compiler/LOCAL_ENV)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))

(defn- eval-with-locals
  "Evals a form with given locals.  The locals should be a map of symbols to
  values."
  [locals form]
  (binding [*locals* locals]
    (eval
     `(let ~(vec (mapcat #(list % `(*locals* '~%)) (keys locals)))
        ~form))))

;; the system.out might be getting messed with.  The system/in is not getting echoed back

(defmacro debug-repl
  "Starts a REPL with the local bindings available."
  ([] `(debug-repl "dr"))
  ([prompt]
   `(let [~'local-bindings (debugger-get-local-bindings)]
      (.printStackTrace (Throwable. "Entering Debugger") System/out)
      (aprint ~'local-bindings)
      (clojure.main/repl
         :prompt #(print ~prompt "=> ")
         :eval (partial ~eval-with-locals ~'local-bindings)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn map-same-type [func val] ; I feel like this should be defined somewhere in the library???
  (cond (vector? val) (vector (map func val))
        (map? val) (into {} (map func val))
        (set? val) (into #{} (map func val))
        :else val))


(defn add-function-argument [functions argument body]
  (cond (list? body) (let [ags (map (partial add-function-argument functions argument) (cdr body))]
                       (if (contains? functions (car body))
                         (concat (list (car body) argument) ags)
                         (cons (car body) ags)))
        ;; though this map might return different type
        (or (vector? body) (set? body)) (map-same-type (partial add-function-argument functions argument) body)
        (map? body) (into {} (map (fn [[name b]] [name (add-function-argument functions argument b)]) body))
        :else body))



;; this function runs every call through resolve which thing it should be
;; though this does not improve the efficiency of the runtime at all as this still goes through with resolving lots of fields anytime it
;; attempts to call a function
(defn resolve-functions [body env]
  (cond (list? body)
        (concat (list (or (resolve env (car body)) (car body)))
                (map #(resolve-functions %1 env) (cdr body)))
        (vector? body) (vector (map #(resolve-functions %1 env) body))
        ;; there shouldn't be anything else that needs to get mapped here
        :else body))


;; if there is some function that can cache the result of some computation, then we can have a field that is nil
;; by default, and then run the function to store the result of the computation.
;; I suppose that this should allow for the function to be such that it
(defmacro cache-field [field & func]
  (let [sr (gensym)]
    `(if (nil? ~field)
       (let [~sr (do ~@func)]
         (set! ~field ~sr)
         ~sr)
       ~field)))


(defn overrideable-function
  "Allows for a function in a macro to be overriden via the optional arguments"
  [opts name body]
  (let [ret (atom body)]
    (doseq [o opts]
      (if (= (car o) name)
        (reset! ret o)))
    @ret))