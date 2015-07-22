(ns clj-hclust.visualisation
  (:require [analemma.svg :as svg]
            [analemma.xml :as xml]
            [analemma.charts :as charts])
  (:import [javax.swing JFrame] 
           [org.apache.batik.dom.svg SAXSVGDocumentFactory]
           [org.apache.batik.swing JSVGCanvas]                                 
           [org.apache.batik.util XMLResourceDescriptor]))

;; TODO
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

#_(defn ttt [clusters]
  (loop [d-els (doto (java.util.ArrayDeque.) (.addFirst clusters))
         d-dst (doto (java.util.ArrayDeque.) (.addFirst (nth 2 clusters)))
         branch (nth 1 clusters)]
    (if )
    ))
#_(ttt C)

(defn mk-dists [clustering]
  (let [r 5 ; svg cirle rayon => svg max heigt = r * nb-els
        xmax 500 ; svg max width 
        dmax (last clustering)
        nb-vals (atom 0)
        last-val (atom nil)
        leaf? (atom false)
        nb-leaves (atom 0)
        res (atom [])]
    (clojure.walk/postwalk
     (fn [f]
       (when (not (sequential? f))
         (swap! nb-vals inc)
         (reset! last-val f))
       (when (and (sequential? f) (< @nb-vals 3))
         (swap! res conj {-1 [(double (* xmax (- 1 (/ @last-val dmax)))) "???"]}) ;; TODO find out y-coord
         (reset! nb-vals 0))
       (when @leaf?
         (swap! res conj {(first f) [xmax (* r (+ 1 (* 2 @nb-leaves)))]})
         (swap! nb-leaves inc)
         (reset! leaf? false)
         (reset! nb-vals 0))
       (when (= 3 @nb-vals) ; prepare flag for next iter
         (reset! leaf? true))
       f)
     clustering)
    @res))
(println (mk-dists C))

;; simple print
(let [res (atom 0)]
  (clojure.walk/postwalk
   (fn [f]
     (println f)
     (when (and (sequential? f)
                (not (sequential? (first f))))
       (swap! res inc))
     f) 
   C)
  @res)


