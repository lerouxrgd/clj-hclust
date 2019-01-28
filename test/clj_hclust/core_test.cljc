(ns clj-hclust.core-test
  (:require
   [clojure.test :refer :all]
   [clj-hclust.core :refer :all]
   [clj-hclust.test-utils :refer :all]
   [clojure.core.matrix :as m]))

(m/set-current-implementation :vectorz)

(def M (m/matrix [[0.00 0.50 2.24 3.35 3.00] 
                  [0.50 0.00 2.50 3.61 3.04]
                  [2.24 2.50 0.00 1.12 1.41]
                  [3.35 3.61 1.12 0.00 1.50]
                  [3.00 3.04 1.41 1.50 0.00]]))

(deftest find-dmin-test
  (testing "Testing find-dmin, normal behaviour"
    (is (= (find-dmin M)
           {:i 0, :j 1, :dij 0.5, :curr 4}))))

(deftest next-state-test
  (testing "Testing next-state, first merge"
    (is (= (next-state
            {:clusters {0 [0 0 0], 1 [1 1 0], 2 [2 2 0], 3 [3 3 0], 4 [4 4 0]}, 
             :merged []}
            {:i 0, :j 1, :dij 0.5, :curr 4}))
        {:clusters {0 [[0 0 0] [1 1 0] 0.5], 4 [4 4 0], 3 [3 3 0], 2 [2 2 0]}, 
         :merged [1]})))

(deftest clust-size-test
  (testing "Testing clust-size, correct sizes after 1 merge"
    (is (= (clust-size
            1 ;; idx 1 corresponds to cluster 2
            {:clusters {0 [[0 0 0] [1 1 0] 0.5], 4 [4 4 0], 3 [3 3 0], 2 [2 2 0]}, 
             :merged {0 0, 1 2, 2 3, 3 4}})
           1))
    (is (= (clust-size
            0 ;; idx 0 corresponds to cluster 0 (which is the merge of 0 and 1)
            {:clusters {0 [[0 0 0] [1 1 0] 0.5], 4 [4 4 0], 3 [3 3 0], 2 [2 2 0]}, 
             :merged {0 0, 1 2, 2 3, 3 4}})
           2))))

(deftest hclust-lw-test
  (testing "Testing hclust, lance-williams based clusterers"
    ;; single link
    (is (= (hclust-lw M :single-link)
           [[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.41] 2.24]))
    ;; complete link
    (is (= (hclust-lw M :complete-link)
           [[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.5] 3.61]))
    ;; ward
    (is (fuzzy= 0.05 0.25 (last (first (hclust-lw M :ward)))))          ;; merge 1
    (is (fuzzy= 0.05 1.25 (last (first (second (hclust-lw M :ward)))))) ;; merge 2
    (is (fuzzy= 0.05 2.42 (last (second (hclust-lw M :ward)))))         ;; merge 3
    (is (fuzzy= 0.05 19.88 (last (hclust-lw M :ward))))                 ;; merge 4
    ))

;; Visualization

(def C [[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.41] 2.24])

(deftest hclust->points-test
  (testing "Testing hclust->points, the main function for svg drawing"
    (let [points (hclust->points C svg-default)
          xmax (get-in svg-default [:params :dendro-width])
          ;; leaves
          rad (get-in svg-default [:circle-style :r])
          x-leaf (+ xmax rad)
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
      (is (= (nth points 1) {:type :leaf, :id 0, :x x-leaf, :y (y-leaf 0)}))
      (is (= (nth points 2) {:type :leaf, :id 1, :x x-leaf, :y (y-leaf 1)}))
      (is (= (nth points 4) {:type :leaf, :id 2, :x x-leaf, :y (y-leaf 2)}))
      (is (= (nth points 5) {:type :leaf, :id 3, :x x-leaf, :y (y-leaf 3)}))
      (is (= (nth points 7) {:type :leaf, :id 4, :x x-leaf, :y (y-leaf 4)}))
      
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

(deftest hclust->newick-test
  (testing "Testing hclust->newick string result"
    (is (= (hclust->newick C)
           "(((0:0,1:0):0.5,((2:0,3:0):1.12,4:0):1.41):2.24);"))))
