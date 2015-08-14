(ns clj-hclust.lance-williams
  (:require [clojure.core.matrix :as m]))

(defn mvalidate [m]
  {:pre [(m/square? m)]}
  (if (m/lower-triangular? m) (m/transpose m) (m/matrix m)))

(defn find-dmin [m]
  (reduce (fn [acc s]
            (let [jmin (->> (map-indexed vector s)
                            (filter #(> (first %) (:curr acc))) ; only above diag
                            (apply min-key second)
                            first)
                  smin (m/mget s jmin)]
              (if (< smin (:dij acc))
                (-> acc
                    (assoc :i (:curr acc))
                    (assoc :j jmin)
                    (assoc :dij smin)
                    (update-in [:curr] inc))
                (update-in acc [:curr] inc))))
          {:i nil :j nil :dij Double/POSITIVE_INFINITY :curr 0}
          (butlast (m/slices m))))

(defn clust-id [idx state]
  (+ idx ; increment idx based on previsouly merged clusters ids 
     (reduce #(if (>= idx %2) (inc %1) %1) 0 (:merged state))))

(defn clust-size [idx state]
  (let [c-id (clust-id idx state)
        cluster (get (:clusters state) c-id)
        res (atom 0)]
    (clojure.walk/postwalk
     (fn [f] 
       (when (and (sequential? f)
                  (not (sequential? (first f))))
         (swap! res inc))
       f) 
     cluster)
    @res))

(defn single-link 
  [dij dik djk state i j k]
  (min dik djk))

(defn complete-link 
  [dij dik djk state i j k]
  (max dik djk))

(defn merge-clusters!
  [m {:keys [i j dij]} state lw-updater]
  (doall
   (map (fn [getter]
          (let [dim (first (m/shape m))]
            (m/emap! (fn [dik djk k]
                       (if (or (= i k) (= j k))
                         dik
                         (lw-updater dij dik djk state i j k))) 
                     (getter m i)
                     (getter m j)
                     (range dim))))
        [m/get-column m/get-row]))
  m)

(defn next-state
  [state {:keys [i j dij]}]
  (let [idx j
        i (clust-id i state)
        j (clust-id j state)]
    (-> state 
        (assoc-in [:clusters i]
                  (vector (get-in state [:clusters i]) 
                          (get-in state [:clusters j])
                          dij))
        (update-in [:clusters] dissoc j)
        (update-in [:merged] conj idx))))

(defn next-m
  [m {:keys [j]}]
  (let [dim (first (m/shape m))
        subv (filter #(not= % j) (range dim))]
    (m/matrix (m/select m subv subv))))

(defn hclust [m lw-updater]
  (let [m (mvalidate m)
        dim (first (m/shape m))
        state-init {:merged [] 
                    :clusters (->> (range dim) 
                                   (map #(vector % [% % 0]))
                                   (into {}))}]
    (loop [m m state state-init]
      (if (= 1 (first (m/shape m)))
        (val (first (:clusters state))) ; only one kv-pair left, return the val
        (let [dmin (find-dmin m)]
          (merge-clusters! m dmin state lw-updater)
          (recur (next-m m dmin) (next-state state dmin)))))))

(comment
  (m/set-current-implementation :vectorz)
  (def M (m/matrix [[0.00 0.50 2.24 3.35 3.00] 
                    [0.50 0.00 2.50 3.61 3.04]
                    [2.24 2.50 0.00 1.12 1.41]
                    [3.35 3.61 1.12 0.00 1.50]
                    [3.00 3.04 1.41 1.50 0.00]]))
  (hclust M single-link))

