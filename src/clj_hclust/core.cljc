(ns clj-hclust.core
  (:require
   [clojure.core.matrix :as m]
   [hiccup.core :as h]
   [medley.core :refer [map-keys]]))

(m/set-current-implementation :vectorz)

(defn prepare-m [m lw]
  {:pre [(m/square? m)]}
  (let [unchanged? (atom true)
        m (cond-> m
            (m/lower-triangular? m) 
            (do (reset! unchanged? false)
                (m/transpose m))
            (= :ward lw) 
            (do (reset! unchanged? false)
                (m/matrix (map #(m/square %) (m/slices m)))))]
    (if @unchanged?
      (m/matrix m)
      m)))

(defn find-dmin [m]
  (reduce (fn [acc s]
            (let [jmin (->> (map-indexed vector s)
                            (filter #(> (first %) (:curr acc))) ;; only above diag
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

(defmulti lw-updater
  (fn [lw] lw))

(defmethod lw-updater :single-link [_]
  (fn [dij dik djk state i j k]
    (min dik djk)))

(defmethod lw-updater :complete-link [_]
  (fn [dij dik djk state i j k]
    (max dik djk)))

(defmethod lw-updater :ward [_]
  (fn [dij dik djk state i j k]
    (let [ni (clust-size i state)
          nj (clust-size j state)
          nk (clust-size k state)
          n (+ ni nj nk)]
      (+ (* dik (/ (+ ni nk) n))
         (* djk (/ (+ nj nk) n))
         (* -1. dij (/ nk n))))))

(defmethod lw-updater :default [lw]
  (throw (IllegalArgumentException. 
          (str "No implementation for lw-updater " lw))))

(defn merge-clusters!
  [m {:keys [i j dij]} lw-update state]
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

(defn hclust-lw [m lw]
  (let [m (prepare-m m lw)
        dim (first (m/shape m))
        lw-update (lw-updater lw)
        state-init {:merged (->> (range dim)
                                 (map #(vector % %))
                                 (into {}))
                    :clusters (->> (range dim) 
                                   (map #(vector % [% % 0]))
                                   (into {}))}]
    (loop [m m state state-init]
      (if (= 1 (first (m/shape m)))
        (val (first (:clusters state))) ;; only one kv-pair left, return its val
        (let [dmin (find-dmin m)]
          (merge-clusters! m dmin lw-update state)
          (recur (next-m m dmin) (next-state state dmin)))))))

;; Visualization

(defn shape [cluster]
  (cond 
    (not (sequential? cluster)) :value
    (not (sequential? (first cluster))) :leaf
    :else :cluster))

(defn hclust->points [clustering style]
  (let [xmax (get-in style [:params :dendro-width]) ;; svg max width (w/o radius and text)
        rad  (get-in style [:circle-style :r]) ;; svg max heigt = 2*rad*nb-leaves
        dmax (last clustering)
        nb-leaves (atom 0)
        bary-stack (atom (list))
        points (atom (transient []))]
    
    (clojure.walk/postwalk
     (fn [elem]
       (when (sequential? elem)
         (let [[l r d] elem ;; all sequential elements match [left right distance]
               shape-l (shape l) 
               shape-r (shape r)]
           (cond

             ;; elem is the merge of two leaves
             (and (= :leaf shape-l) (= :leaf shape-r))
             (let [y-leaf1 (* rad (+ 1 (* 2 @nb-leaves)))
                   y-leaf2 (+ y-leaf1 (* 2 rad))
                   x-bary (* xmax (- 1. (/ d dmax)))
                   y-bary (+ y-leaf1 rad)]
               (swap! points 
                      #(-> %
                           (conj! {:type :fork 
                                   :y-span [y-leaf1 y-leaf2] 
                                   :x-span [xmax xmax] 
                                   :x x-bary :y y-bary})
                           (conj! {:type :leaf 
                                   :id (first l) 
                                   :x (+ xmax rad) :y y-leaf1})
                           (conj! {:type :leaf 
                                   :id (first r) 
                                   :x (+ xmax rad) :y y-leaf2})))
               (swap! nb-leaves + 2)
               (swap! bary-stack conj [x-bary y-bary]))
                         
             ;; elem is the merge of a cluster and a leaf
             (or (and (= :cluster shape-l) (= :leaf shape-r))
                 (and (= :leaf shape-l) (= :cluster shape-r)))
             (let [[x-clust y-clust] (peek @bary-stack)
                   _ (swap! bary-stack pop)
                   y-leaf (* rad (+ 1 (* 2 @nb-leaves)))
                   id-leaf (first (if (= :leaf shape-r) r l))
                   x-bary (* xmax (- 1. (/ d dmax)))
                   y-bary (/ (+ y-clust y-leaf) 2.)]
               (swap! points
                      #(-> %
                           (conj! {:type :fork 
                                   :y-span [y-clust y-leaf] 
                                   :x-span [x-clust xmax]  
                                   :x x-bary :y y-bary})
                           (conj! {:type :leaf 
                                   :id id-leaf 
                                   :x (+ xmax rad) :y y-leaf})))
               (swap! nb-leaves + 1)
               (swap! bary-stack conj [x-bary y-bary]))            

             ;; elem is the merge of two clusters
             (and (= :cluster shape-l) (= :cluster shape-r))
             (let [[x-clust1 y-clust1] (peek @bary-stack)
                   _ (swap! bary-stack pop)
                   [x-clust2 y-clust2] (peek @bary-stack)
                   _ (swap! bary-stack pop)
                   x-bary (* xmax (- 1. (/ d dmax)))
                   y-bary (/ (+ y-clust1 y-clust2) 2.)]
               (swap! points 
                      #(conj! % {:type :fork 
                                 :y-span [y-clust1 y-clust2] 
                                 :x-span [x-clust1 x-clust2] 
                                 :x x-bary :y y-bary}))
               (swap! bary-stack conj [x-bary y-bary])))))
       elem)
     clustering)
    (persistent! @points)))

(defn translate [point]
  (str "translate(" (:x point) "," (:y point) ")"))

(defn mk-path [point direction]
  (let [move (case direction :up first :down second)]
    (str "M" (:x point) "," (:y point)
         "V" (move (:y-span point))
         "H" (move (:x-span point)))))

(defn points->hiccup [points style]
  {:pre [(if-let [names (:names style)] (vector? names) true)]}
  (let [{:keys [link-style node-style circle-style text-style xmlns]} style
        id->name (if-let [names (:names style)]
                   (fn [id] (get names id))
                   identity)]
    (loop [svg (transient [:svg xmlns]) 
           [point & others] points]
      (if point
        (cond
          (= :leaf (:type point))
          (recur (conj! svg [:g (conj node-style [:transform (translate point)])
                             [:circle circle-style]
                             [:text text-style (id->name (:id point))]])
                 others)
          (= :fork (:type point))
          (recur (-> svg
                     (conj! [:path (conj link-style [:d (mk-path point :up)])])
                     (conj! [:path (conj link-style [:d (mk-path point :down)])]))
                 others))
        (persistent! svg)))))

(def svg-default
  {:link-style {:fill "none" :stroke "#CCC" :stroke-width "1.5px"}
   :node-style {:font "10px sans-serif"}
   :circle-style {:r 5 :fill "#FFF" :stroke "#4682B4" :stroke-width "1.5px"}
   :text-style {:dx 8 :dy 3 :text-anchor "start"}
   :params {:dendro-width 500}
   :xmlns {"xmlns:svg" "http://www.w3.org/2000/svg"
           "xmlns" "http://www.w3.org/2000/svg"
           "xmlns:xlink" "http://www.w3.org/1999/xlink"
           "version" "1.0"}})

(defn hclust->svg
  ([clustering]
   (hclust->svg clustering {}))
  ([clustering style]
   (let [style (merge-with merge svg-default style)]
     (-> clustering
         (hclust->points style)
         (points->hiccup style)
         (h/html)))))

(defn hclust->newick [clustering]
  (str "("
       (clojure.walk/postwalk
        (fn [elem]
          (if (sequential? elem)
            (let [[l r d] elem]
              (if (number? l)
                (str l ":" d)
                (str "(" l "," r "):" d)))
            elem))
        clustering)
       ");"))
