(ns clj-hclust.visualisation
  (:require [hiccup.core :as h]))

(defn shape [cluster]
  (cond 
    (not (sequential? cluster)) :value
    (not (sequential? (first cluster))) :leaf
    :else :cluster))

(defn hclust->points [clustering style]
  (let [xmax (get-in style [:params :dendro-width]) ; svg max width (w/o radius and text)
        rad (get-in style [:circle-style :r]); ; svg max heigt = 2*rad*nb-leaves
        dmax (last clustering)
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
             (or (and (= :cluster shape-l) (= :leaf shape-r))
                 (and (= :leaf shape-l) (= :cluster shape-r)))
             (let [[x-clust y-clust] (.poll bary-stack)
                   y-leaf (* rad (+ 1 (* 2 @nb-leaves)))
                   id-leaf (first (if (= :leaf shape-r) r l))
                   x-bary (* xmax (- 1. (/ d dmax)))
                   y-bary (/ (+ y-clust y-leaf) 2.)]
               (swap! points
                      #(-> %
                           (conj! {:type :fork :y-span [y-clust y-leaf] :x-span [x-clust xmax]  
                                   :x x-bary :y y-bary})
                           (conj! {:type :leaf :id id-leaf :x xmax :y y-leaf})))
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

