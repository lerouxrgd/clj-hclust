(ns clj-hclust.core
  (:require
   [clojure.core.matrix :as m]
   [medley.core :refer [map-keys]]))

(m/set-current-implementation :vectorz)

(defn prepare-m [m lw-updater]
  {:pre [(m/square? m)]}
  (let [unchanged? (atom true)
        m (cond-> m
            (m/lower-triangular? m) 
            (do (reset! unchanged? false)
                (m/transpose m))
            (= :ward lw-updater) 
            (do (reset! unchanged? false)
                (m/matrix (map #(m/square %) (m/slices m)))))]
    (if @unchanged?
      (m/matrix m)
      m)))

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
  (get (:merged state) idx))

(defn clust-size [idx state]
  (let [c-id (clust-id idx state)
        cluster (get (:clusters state) c-id)
        res (atom 0)]
    (clojure.walk/postwalk
     (fn [elem] 
       (when (and (sequential? elem) (not (sequential? (first elem))))
         (swap! res inc))
       elem) 
     cluster)
    @res))

;; TODO use protocol during calculation, mmethod for selecting it

(defmulti lw-update
  (fn [dij dik djk state i j k] 
    (:lw-updater state)))

(defmethod lw-update :single-link
  [dij dik djk state i j k]
  (min dik djk))

(defmethod lw-update :complete-link
  [dij dik djk state i j k]
  (max dik djk))

(defmethod lw-update :ward
  [dij dik djk state i j k]
  (let [ni (clust-size i state)
        nj (clust-size j state)
        nk (clust-size k state)
        n (+ ni nj nk)]
    (+ (* dik (/ (+ ni nk) n))
       (* djk (/ (+ nj nk) n))
       (* -1. dij (/ nk n)))))

(defmethod lw-update :default
  [dij dik djk state i j k]
  (throw (IllegalArgumentException. 
          (str "No implementation for lw-updater " (:lw-updater state)))))

(defn merge-clusters!
  [m {:keys [i j dij]} state]
  (doall
   (map (fn [getter]
          (let [dim (first (m/shape m))]
            (m/emap! (fn [dik djk k]
                       (if (or (= i k) (= j k))
                         dik
                         (lw-update dij dik djk state i j k))) 
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
        (update-in [:merged]
                   (fn [merged]
                     (let [[before after] (split-at idx (range (count merged)))]
                       (merge (select-keys merged before)
                              (->> (rest after)
                                   (select-keys merged)
                                   (map-keys dec)))))))))

(defn next-m
  [m {:keys [j]}]
  (let [dim (first (m/shape m))
        subv (filter #(not= % j) (range dim))]
    (m/matrix (m/select m subv subv))))

(defn hclust-lw [m lw-updater]
  (let [m (prepare-m m lw-updater)
        dim (first (m/shape m))
        state-init {:lw-updater lw-updater
                    :merged (->> (range dim)
                                 (map #(vector % %))
                                 (into {}))
                    :clusters (->> (range dim) 
                                   (map #(vector % [% % 0]))
                                   (into {}))}]
    (loop [m m state state-init]
      (if (= 1 (first (m/shape m)))
        (val (first (:clusters state))) ; only one kv-pair left, return its val
        (let [dmin (find-dmin m)]
          (merge-clusters! m dmin state)
          (recur (next-m m dmin) (next-state state dmin)))))))

