(ns clj-hclust.visualisation
  (:require [analemma.svg :as svg]
            [analemma.xml :as xml]
            [analemma.charts :as charts])
  (:import [javax.swing JFrame] 
           [org.apache.batik.dom.svg SAXSVGDocumentFactory]
           [org.apache.batik.swing JSVGCanvas]                                 
           [org.apache.batik.util XMLResourceDescriptor]))

;; TODO hclust->newick
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
         (doto (JFrame. "My Awesome SVG")
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
        points (atom [])]
    (clojure.walk/postwalk
     (fn [f]
       (when (sequential? f)
         (let [l (nth f 0) r (nth f 1) d (nth f 2) 
               shape-l (shape l) shape-r (shape r)]
           (cond
             ;; merge two leaves
             (and (= :leaf shape-l) (= :leaf shape-r))
             (do (swap! points conj {:type :leaf :id (first l) 
                                     :x xmax :y (* rad (+ 1 (* 2 @nb-leaves)))})
                 (swap! nb-leaves + 1)
                 (swap! points conj {:type :leaf :id (first r) 
                                     :x xmax :y (* rad (+ 1 (* 2 @nb-leaves)))})
                 (swap! nb-leaves + 1)
                 (let [center (* 2 (- @nb-leaves 1) rad)
                       y-up (- center rad)
                       y-down (+ center rad)]
                   (swap! points conj {:type :fork :span [y-up y-down]
                                       :x (double (* xmax (- 1 (/ d dmax)))) :y center})
                   (.push bary-stack center)))
             ;; merge cluster and one leaf
             (and (= :cluster shape-l) (= :leaf shape-r))
             (do (swap! points conj {:type :leaf :id (first r) 
                                     :x xmax :y (* rad (+ 1 (* 2 @nb-leaves)))})
                 (swap! nb-leaves + 1)
                 (let [y-up (.poll bary-stack)
                       center (+ (* 2 rad) y-up)
                       y-down (+ center rad)]
                   (swap! points conj {:type :fork :span [y-up y-down]
                                       :x (double (* xmax (- 1 (/ d dmax)))) :y center})
                   (.push bary-stack center)))
             ;; merge two clusters
             (and (= :cluster shape-l) (= :cluster shape-r))
             (let [y-up (.poll bary-stack)
                   y-down (.poll bary-stack)
                   center (/ (+ y-up y-down) 2.)] 
               (swap! points conj {:type :fork :span [y-up y-down]
                                   :x (double (* xmax (- 1 (/ d dmax)))) :y center})
               (.push bary-stack center)))))
       f)
     clustering)
    @points))
(hclust->points C {:rad 5 :xmax 500})

