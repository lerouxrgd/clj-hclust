(ns clj-hclust.batik
  (:import
   (javax.swing JFrame) 
   (org.apache.batik.dom.svg SAXSVGDocumentFactory)
   (org.apache.batik.swing JSVGCanvas)                                 
   (org.apache.batik.util XMLResourceDescriptor)))

(defn svg-jframe 
  ([svg-str width height]
   (svg-jframe svg-str width height "Displaying SVG"))
  ([svg-str width height title]
   (let [xml-rdr (java.io.StringReader. svg-str)
         sax-parser (SAXSVGDocumentFactory. (XMLResourceDescriptor/getXMLParserClassName))
         doc (.createSVGDocument sax-parser "dummy-uri" xml-rdr)
         svgcanvas (doto (JSVGCanvas.) (.setSVGDocument doc))]
     (.close xml-rdr)
     (java.awt.EventQueue/invokeLater
      (proxy [Runnable] []
        (run []
          (doto (JFrame. title)
            (.. (getContentPane) (add svgcanvas))
            (.setSize width height)
            (.setVisible true))))))))

