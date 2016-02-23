(ns clj-hclust.lance-williams-test
  (:require [clojure.test :refer :all]
            [clj-hclust.lance-williams :refer :all]
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
    (is (= (next-state {:clusters {0 [0 0 0], 1 [1 1 0], 2 [2 2 0], 3 [3 3 0], 4 [4 4 0]}, 
                        :merged []}
                       {:i 0, :j 1, :dij 0.5, :curr 4}))
        {:clusters {0 [[0 0 0] [1 1 0] 0.5], 4 [4 4 0], 3 [3 3 0], 2 [2 2 0]}, 
         :merged [1]})))

(deftest clust-size-test
  (testing "Testing clust-size, correct sizes after 1 merge"
    (is (= (clust-size 1 ; idx 1 corresponds to cluster 2
                       {:clusters {0 [[0 0 0] [1 1 0] 0.5], 4 [4 4 0], 3 [3 3 0], 2 [2 2 0]}, 
                        :merged {0 0, 1 2, 2 3, 3 4}})
           1))
    (is (= (clust-size 0 ; idx 0 corresponds to cluster 0 (which is the merge of 0 and 1)
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
    (is (fuzzy= 0.05 0.25 (last (first (hclust-lw M :ward)))))          ; merge 1
    (is (fuzzy= 0.05 1.25 (last (first (second (hclust-lw M :ward)))))) ; merge 2
    (is (fuzzy= 0.05 2.42 (last (second (hclust-lw M :ward)))))         ; merge 3
    (is (fuzzy= 0.05 19.88 (last (hclust-lw M :ward))))                 ; merge 4
    ))

