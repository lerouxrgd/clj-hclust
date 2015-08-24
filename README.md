# clj-hclust

A [hierarchical clustering][hc] and visualisation library for clojure based on core.matrix.

## Installation

Add the following dependency to your `project.clj` file:

```clj
[clj-hclust "0.1.0"]
```

## Usage

All clustering results are stored in vectors with the form `[left right distance]`.
A leaf is simply `[id-self id-self 0]` where `id-self` is its index in the matrix.

Here is a simple example:

```clj
(require '[clj-hclust.lance-williams :as lw]
         '[clojure.core.matrix :as m])

(def M (m/matrix
        [[0.00 0.50 2.24 3.35 3.00]
         [0.50 0.00 2.50 3.61 3.04]
         [2.24 2.50 0.00 1.12 1.41]
         [3.35 3.61 1.12 0.00 1.50]
         [3.00 3.04 1.41 1.50 0.00]]))

(-> M
    (lw/hclust-lw :single-link))
    
;;=> [[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.41] 2.24]
```

Under the hood core.matrix is used with `:vectorz` as default implementations. 
You can use another implementation by adding corresponding dependency to your project file and selecting implementation using `set-current-implementation`.
Note that the selected implementation must support `emap!` from the core.matrix API.

## Algorithms

### Lance-Williams

The [Lance–Williams algorithms][lance-williams] are an infinite family of agglomerative [hierarchical clustering][hc] algorithms which are represented by a recursive formula for updating cluster distances at each step (each time a pair of clusters is merged).

The namespace `clj-hclust.lance-williams` defines a multimethod that allows for custom implementations of such algorithms:

```clj
(defmulti lw-update
  (fn [dij dik djk state i j k] 
    (:lw-updater state)))
```

Where the parameter `state` is a map containing `:clusters`, the vector of the current merges.

The multimethod implementations provided are:

* `:single-link` uses min when merging
* `:complete-link` uses max when merging
* `:ward` minimizes the total within-cluster variance when merging 

## Visualisation

```clj
(require '[clj-hclust.visualisation :as viz])
```

### SVG format

A simple [batik][apache-batik] based `JFrame` visualisation is available (note that it's possible to get just the SVG string):

```clj
(require '[clj-hclust.batik :as b])

(-> M
    (lw/hclust-lw :single-link)
    (viz/hclust->svg)
    (b/svg-jframe 600 100))
```

<svg version="1.0" xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns="http://www.w3.org/2000/svg"><path d="M388.39285714285717,10V5H500" fill="none" stroke-width="1.5px" stroke="#CCC"></path><path d="M388.39285714285717,10V15H500" fill="none" stroke-width="1.5px" stroke="#CCC"></path><g font="10px sans-serif" transform="translate(500,5)"><circle fill="#FFF" r="5" stroke-width="1.5px" stroke="#4682B4"></circle><text dx="8" dy="3" text-anchor="start">0</text></g><g font="10px sans-serif" transform="translate(500,15)"><circle fill="#FFF" r="5" stroke-width="1.5px" stroke="#4682B4"></circle><text dx="8" dy="3" text-anchor="start">1</text></g><path d="M250.0,30V25H500" fill="none" stroke-width="1.5px" stroke="#CCC"></path><path d="M250.0,30V35H500" fill="none" stroke-width="1.5px" stroke="#CCC"></path><g font="10px sans-serif" transform="translate(500,25)"><circle fill="#FFF" r="5" stroke-width="1.5px" stroke="#4682B4"></circle><text dx="8" dy="3" text-anchor="start">2</text></g><g font="10px sans-serif" transform="translate(500,35)"><circle fill="#FFF" r="5" stroke-width="1.5px" stroke="#4682B4"></circle><text dx="8" dy="3" text-anchor="start">3</text></g><path d="M185.2678571428572,37.5V30H250.0" fill="none" stroke-width="1.5px" stroke="#CCC"></path><path d="M185.2678571428572,37.5V45H500" fill="none" stroke-width="1.5px" stroke="#CCC"></path><g font="10px sans-serif" transform="translate(500,45)"><circle fill="#FFF" r="5" stroke-width="1.5px" stroke="#4682B4"></circle><text dx="8" dy="3" text-anchor="start">4</text></g><path d="M0.0,23.75V37.5H185.2678571428572" fill="none" stroke-width="1.5px" stroke="#CCC"></path><path d="M0.0,23.75V10H388.39285714285717" fill="none" stroke-width="1.5px" stroke="#CCC"></path></svg>

SVG visualisation parameters can be customized:

```clj
(-> M
    (lw/hclust-lw :single-link)
    (viz/hclust->svg {:names ["Rochefort" "Milady" "Athos" "Portos" "Aramis"]
                      :circle-style {:r 10}
                      :text-style {:dx 16 :dy 6 :font "14px sans-serif"}})
    (b/svg-jframe 700 150))
```

### Newick format

A [newick format][newick] visualisation is also available:

```clj
(-> M
    (lw/hclust-lw :single-link)
    (viz/hclust->newick))

;;=> "(((0:0,1:0):0.5,((2:0,3:0):1.12,4:0):1.41):2.24);"
```

## License

Copyright &copy; 2015 Romain Leroux

This project is licensed under the [Eclipse Public License 1.0][license].

[hc]: https://en.wikipedia.org/wiki/Hierarchical_clustering

[lance-williams]: https://en.wikipedia.org/wiki/Ward%27s_method#Lance.E2.80.93Williams_algorithms

[apache-batik]: https://xmlgraphics.apache.org/batik/

[newick]: https://fr.wikipedia.org/wiki/Newick

[license]: http://www.eclipse.org/legal/epl-v10.html
