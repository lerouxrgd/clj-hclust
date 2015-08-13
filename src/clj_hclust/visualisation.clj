(ns clj-hclust.visualisation
  (:require [hiccup.core :as h] 
            [analemma.svg :as svg]
            [analemma.xml :as xml]
            [analemma.charts :as charts])
  (:import [javax.swing JFrame] 
           [org.apache.batik.dom.svg SAXSVGDocumentFactory]
           [org.apache.batik.swing JSVGCanvas]                                 
           [org.apache.batik.util XMLResourceDescriptor]))

;; TODO hclust->newick (using "format" from postwalk?)
(comment
  [[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.41] 2.24]
  "(((0:0,1:0):0.5,((2:0,3:0):1.12,4:0):1.41):2.24);")

(defn cos-sin-svg []
  (charts/emit-svg
   (let [x (range -5 5 0.05)
         y1 (map #(Math/cos %) x)
         y2 (map #(Math/sin %) x)]
     (-> (charts/xy-plot :xmin -5 :xmax 5 :ymin -1.5 :ymax 1.5 :height 500 :width 500)
         (charts/add-points [x y1] :transpose-data?? true)
         (charts/add-points [x y2] :transpose-data?? true :fill (svg/rgb 255 0 0))))))

(defn mk-svgviz [svg-str]
  (let [xml-rdr (java.io.StringReader. svg-str)
        sax-parser (SAXSVGDocumentFactory. (XMLResourceDescriptor/getXMLParserClassName))
        doc (.createSVGDocument sax-parser "dummy-uri" xml-rdr)
        svgcanvas (doto (JSVGCanvas.) (.setSVGDocument doc))]
    (.close xml-rdr)
    (java.awt.EventQueue/invokeLater
     (proxy [Runnable] []
       (run []
         (doto (JFrame. "Displaying SVG")
           (.. (getContentPane) (add svgcanvas))
           (.setSize 600 600)
           (.setVisible true)))))))
#_(mk-svgviz (cos-sin-svg))

(def C [[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.41] 2.24])
(def D [[[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.41] 2.24] [5 5 0] 3.33])

(defn shape [cluster]
  (cond 
    (not (sequential? cluster)) :value
    (not (sequential? (first cluster))) :leaf
    :else :cluster))

(defn hclust->points
  "rad: svg cirle radius => svg max heigt = 2 * rad * nb-leaves
   xmax: svg max width"
  [clustering {:keys [rad xmax]}]
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
                   center (+ y-leaf1 rad)
                   x-fork (* xmax (- 1. (/ d dmax)))]
               (swap! points 
                      #(-> %
                           (conj! {:type :leaf :id (first l) :x xmax :y y-leaf1})
                           (conj! {:type :leaf :id (first r) :x xmax :y y-leaf2})                 
                           (conj! {:type :fork :span [y-leaf1 y-leaf2] :x x-fork :y center})))
               (swap! nb-leaves + 2)
               (.push bary-stack center))
                         
             ;; elem is the merge of a cluster and a leaf
             (and (= :cluster shape-l) (= :leaf shape-r))
             (let [y-up (.poll bary-stack)                       
                   y-down (* rad (+ 1 (* 2 @nb-leaves)))
                   center (/ (+ y-up y-down) 2.)
                   x-fork (* xmax (- 1. (/ d dmax)))]
               (swap! points
                      #(-> %
                           (conj! {:type :leaf :id (first r) :x xmax :y y-down})
                           (conj! {:type :fork :span [y-up y-down] :x x-fork :y center})))
               (swap! nb-leaves + 1)
               (.push bary-stack center))
             
             ;; elem is the merge of two clusters
             (and (= :cluster shape-l) (= :cluster shape-r))
             (let [y-up (.poll bary-stack)
                   y-down (.poll bary-stack)
                   center (/ (+ y-up y-down) 2.)
                   x-fork (* xmax (- 1. (/ d dmax)))]
               (swap! points 
                      #(conj! % {:type :fork :span [y-up y-down] :x x-fork :y center}))
               (.push bary-stack center)))))
       elem)
     clustering)
    (persistent! @points)))
#_(hclust->points C {:rad 5 :xmax 500})

(defn points->svg-hiccup [points]
  )

