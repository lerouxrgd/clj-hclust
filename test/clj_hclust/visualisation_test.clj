(ns clj-hclust.visualisation-test
  (:require [clojure.test :refer :all]
            [clj-hclust.visualisation :refer :all]))

(def C [[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.41] 2.24])

(deftest hclust->points-test
  (testing "Testing hclust->points, the main function for svg drawing"
    (is (= (hclust->points C svg-default)
           [{:type :fork, :y-span [5 15], :x-span [500 500], :x 388.39285714285717, :y 10} 
            {:type :leaf, :id 0, :x 500, :y 5} 
            {:type :leaf, :id 1, :x 500, :y 15} 
            {:type :fork, :y-span [25 35], :x-span [500 500], :x 250.0, :y 30} 
            {:type :leaf, :id 2, :x 500, :y 25} 
            {:type :leaf, :id 3, :x 500, :y 35} 
            {:type :fork, :y-span [30 45], :x-span [250.0 500], :x 185.2678571428572, :y 37.5} 
            {:type :leaf, :id 4, :x 500, :y 45} 
            {:type :fork, :y-span [37.5 10], :x-span [185.2678571428572 388.39285714285717], :x 0.0, :y 23.75}]))))

