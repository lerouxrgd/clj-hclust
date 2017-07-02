(defproject clj-hclust "0.3.0"
  :description "Hierarchical clustering and visualisation for clojure"
  :url "https://github.com/lerouxrgd/clj-hclust"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [net.mikera/core.matrix "0.60.3"]
                 [net.mikera/vectorz-clj "0.47.0"]
                 [medley "1.0.0"]
                 [hiccup "1.0.5"]
                 [org.clojars.pallix/batik "1.7.0"]]
  :gobal-vars {*warn-on-reflection* true})

