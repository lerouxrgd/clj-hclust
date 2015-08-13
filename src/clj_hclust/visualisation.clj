(ns clj-hclust.visualisation
  (:require [hiccup.core :as h]
            [clj-hclust.batik :as b]))

;; TODO hclust->newick (using "format" from postwalk?)
(comment
  [[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.41] 2.24]
  "(((0:0,1:0):0.5,((2:0,3:0):1.12,4:0):1.41):2.24);")

(def C [[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.41] 2.24])
(def D [[[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.41] 2.24] [5 5 0] 3.33])

(defn shape [cluster]
  (cond 
    (not (sequential? cluster)) :value
    (not (sequential? (first cluster))) :leaf
    :else :cluster))

(defn hclust->points [clustering {:keys [rad xmax]}]
  (let [dmax (last clustering)
        nb-leaves (atom 0)
        bary-stack (java.util.ArrayDeque.)
        points (atom (transient []))]
    (clojure.walk/postwalk
     (fn [elem]
       (when (sequential? elem)
         (let [[l r d] elem ; all sequential elements match [left right distance]
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
                           (conj! {:type :fork :y-span [y-leaf1 y-leaf2] :x-span [xmax xmax] 
                                   :x x-bary :y y-bary})
                           (conj! {:type :leaf :id (first l) :x xmax :y y-leaf1})
                           (conj! {:type :leaf :id (first r) :x xmax :y y-leaf2})))
               (swap! nb-leaves + 2)
               (.push bary-stack [x-bary y-bary]))
                         
             ;; elem is the merge of a cluster and a leaf
             (and (= :cluster shape-l) (= :leaf shape-r))
             (let [[x-clust y-clust] (.poll bary-stack)
                   y-leaf (* rad (+ 1 (* 2 @nb-leaves)))
                   x-bary (* xmax (- 1. (/ d dmax)))
                   y-bary (/ (+ y-clust y-leaf) 2.)]
               (swap! points
                      #(-> %
                           (conj! {:type :fork :y-span [y-clust y-leaf] :x-span [x-clust xmax]  
                                   :x x-bary :y y-bary})
                           (conj! {:type :leaf :id (first r) :x xmax :y y-leaf})))
               (swap! nb-leaves + 1)
               (.push bary-stack [x-bary y-bary]))
             
             ;; elem is the merge of two clusters
             (and (= :cluster shape-l) (= :cluster shape-r))
             (let [[x-clust1 y-clust1] (.poll bary-stack)
                   [x-clust2 y-clust2] (.poll bary-stack)
                   x-bary (* xmax (- 1. (/ d dmax)))
                   y-bary (/ (+ y-clust1 y-clust2) 2.)]
               (swap! points 
                      #(conj! % {:type :fork :y-span [y-clust1 y-clust2] :x-span [x-clust1 x-clust2] 
                                 :x x-bary :y y-bary}))
               (.push bary-stack [x-bary y-bary])))))
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

(defn points->hiccup 
  "rad: svg cirle radius => svg max heigt = 2 * rad * nb-leaves
   xmax: svg max width"
  [points {:keys [rad xmax]}]
  (let [link {:fill "none" :stroke "#CCC" :stroke-width "1.5px"}
        node {:font "10px sans-serif"}
        circle [:circle {:r rad :fill "#FFF" :stroke "#4682B4" :stroke-width "1.5px"}]
        text {:dx 8 :dy 3 :text-anchor "start"}
        xmlns {"xmlns:svg" "http://www.w3.org/2000/svg"
	       "xmlns" "http://www.w3.org/2000/svg"
	       "xmlns:xlink" "http://www.w3.org/1999/xlink"
	       "version" "1.0"}]
    (loop [svg (transient [:svg xmlns]) 
           [point & others] points
           nb-leaves 0]
      (if point
        (cond
          (= :leaf (:type point))
          (recur (conj! svg [:g (conj node [:transform (translate point)])
                             circle
                             [:text text (:id point)]])
                 others
                 (inc nb-leaves))
          (= :fork (:type point))
          (recur (-> svg
                     (conj! [:path (conj link [:d (mk-path point :up)])])
                     (conj! [:path (conj link [:d (mk-path point :down)])]))
                 others
                 nb-leaves))
        (persistent! svg)))))

(let [css {:rad 5 :xmax 500}]
  (-> C
      (hclust->points css)
      (points->hiccup css)
      (h/html)
      (b/svg-jframe 600 100)))

