(ns clj-hclust.visualisation-test
  (:require [clojure.test :refer :all]
            [clj-hclust.visualisation :refer :all]
            [clj-hclust.test-utils :refer :all]))

(def C [[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.41] 2.24])

(deftest hclust->points-test
  (testing "Testing hclust->points, the main function for svg drawing"
    (let [points (hclust->points C svg-default)
          xmax (get-in svg-default [:params :dendro-width])
          ;; leaves
          rad (get-in svg-default [:circle-style :r])
          y-leaf (fn [nb-leaves] (+ rad (* nb-leaves 2 rad)))
          ;; forks
          d1 (last (first C))
          d2 (last (first (second C)))
          d3 (last (second C))
          d4 (last C)
          dmax d4
          x-fork (fn [d] (* xmax (- 1. (/ d dmax))))
          y-fork (fn [el1 el2] 
                   (let [res (/ (+ el1 el2) 2)]
                     (if (ratio? res) (double res) res)))]
      ;; leaves
      (is (= (nth points 1) {:type :leaf, :id 0, :x xmax, :y (y-leaf 0)}))
      (is (= (nth points 2) {:type :leaf, :id 1, :x xmax, :y (y-leaf 1)}))
      (is (= (nth points 4) {:type :leaf, :id 2, :x xmax, :y (y-leaf 2)}))
      (is (= (nth points 5) {:type :leaf, :id 3, :x xmax, :y (y-leaf 3)}))
      (is (= (nth points 7) {:type :leaf, :id 4, :x xmax, :y (y-leaf 4)}))
      
      ;; fork (merge of leaf0 and leaf1 = clust0)
      (is (= (:x (nth points 0)) (x-fork d1)))
      (is (= (:y (nth points 0)) (y-fork (y-leaf 0) (y-leaf 1))))
      (is (= (:y-span (nth points 0)) [(y-leaf 0) (y-leaf 1)]))
      (is (= (:x-span (nth points 0)) [xmax xmax]))
      
      ;; fork (merge of leaf2 and leaf3 = clust1)
      (is (= (:x (nth points 3)) (x-fork d2)))
      (is (= (:y (nth points 3)) (y-fork (y-leaf 2) (y-leaf 3))))
      (is (= (:y-span (nth points 3)) [(y-leaf 2) (y-leaf 3)]))
      (is (= (:x-span (nth points 3)) [xmax xmax]))
      
      ;; fork (merge of clust1 and leaf4 = clust2)
      (is (= (:x (nth points 6)) (x-fork d3)))
      (is (= (:y (nth points 6)) (y-fork (y-fork (y-leaf 2) (y-leaf 3))
                                         (y-leaf 4))))
      (is (= (:y-span (nth points 6)) [(y-fork (y-leaf 2) (y-leaf 3)) 
                                       (y-leaf 4)]))
      (is (= (:x-span (nth points 6)) [(x-fork d2) xmax]))
      
      ;; fork (merge of clust2 and clust0 = final result)                  
      (is (= (:x (nth points 8)) (x-fork d4)))
      (is (= (:y (nth points 8)) (y-fork (y-fork (y-fork (y-leaf 2) (y-leaf 3))
                                                 (y-leaf 4))
                                         (y-fork (y-leaf 0) (y-leaf 1)))))                                    
      (is (= (:y-span (nth points 8)) [(y-fork (y-fork (y-leaf 2) (y-leaf 3))
                                               (y-leaf 4)) 
                                       (y-fork (y-leaf 0) (y-leaf 1))]))
      (is (= (:x-span (nth points 8)) [(x-fork d3) (x-fork d1)]))
      )))

