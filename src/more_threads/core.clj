(ns more-threads.core)

(defn when-pred
  "Calls `then` on `x` if the result of calling `pred` on `x` is truthy.

  This is useful for thread pipelines where [[cond->]] is not sufficient,
  specifically when the condition depends on the current value flowing through
  the threading pipeline."
  [x pred then]
  (if (pred x)
    (then x)
    x))

(defn if-pred
  "Calls either `then` or `else` on `x` depending on the truthiness of calling `pred` on `x`.

  This is useful for thread pipelines where [[cond->]] is not sufficient,
  specifically when the condition depends on the current value flowing through
  the threading pipeline."
  [x pred then else]
  (if (pred x)
    (then x)
    (else x)))

(defn cond-pred
  "Returns the result of calling the then function on `x` from the first clause that returned truthy from calling its predicate on `x`.

  `clauses` are of the form pred-fn then-fn where each is a function of `x`."
  [x & clauses]
  (some #(when ((first %) x)
           ((second %) x))
        (partition 2 clauses)))

(defn partial->
  "Constructs a function of one argument that calls `f` with it as the first argument and `args` for the remaining arguments.

  This is useful for constructing partial functions for passing to [[swap!]] and
  other functions, especially when they do not support passing additional
  arguments to the target function, or when you wish to reuse the function
  across multiple callsites.

  This function can also be very useful when paired with [[if-pred]] and similar
  functions to allow the predicate arguments to act as if it were a form in a
  thread-first body."
  [f & args]
  (fn [v]
    (apply f v args)))

(defmacro when->
  "Threads `x` through `then` as [[->]], but only if `test` is truthy.

  If the `test` expression returns false, then `x` is returned unmodified.

  This is equivalent to [[cond->]] with one clause."
  [x test then]
  `(let [x# ~x]
     (if ~test
       (-> x# ~then)
       x#)))

(defmacro if->
  "Threads `x` through `then` or `else` as [[->]], depending on the truthiness of `test`."
  [x test then else]
  `(let [x# ~x]
     (if ~test
       (-> x# ~then)
       (-> x# ~else))))

(defmacro when->>
  "Threads `x` through `then` as [[->>]], but only if `test` is truthy.

  If the `test` expression returns false, then `x` is returned unmodified.

  This is equivalent to [[cond->>]] with one clause."
  [test then x]
  `(let [x# ~x]
     (if ~test
       (->> x# ~then)
       x#)))

(defmacro if->>
  "Threads `x` through `then` or `else` as [[->>]], depending on the truthiness of `test`."
  [test then else x]
  `(let [x# ~x]
     (if ~test
       (->> x# ~then)
       (->> x# ~else))))
