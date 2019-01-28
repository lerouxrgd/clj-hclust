# clj-hclust

A [hierarchical clustering][hc] and visualisation library for clojure based on core.matrix.

[![Clojars Project](https://img.shields.io/clojars/v/clj-hclust.svg)](https://clojars.org/clj-hclust)

## Usage

All clustering results are stored in vectors with the form `[left right distance]`.
A leaf is simply `[id-self id-self 0]` where `id-self` is its index in the matrix.

Here is a simple example:

```clj
(require '[clj-hclust.core :as hc]
         '[clojure.core.matrix :as m])

(def M (m/matrix
        [[0.00 0.50 2.24 3.35 3.00]
         [0.50 0.00 2.50 3.61 3.04]
         [2.24 2.50 0.00 1.12 1.41]
         [3.35 3.61 1.12 0.00 1.50]
         [3.00 3.04 1.41 1.50 0.00]]))
    
(hc/hclust-lw M :single-link)

;;=> [[[0 0 0] [1 1 0] 0.5] [[[2 2 0] [3 3 0] 1.12] [4 4 0] 1.41] 2.24]
```

Under the hood core.matrix is used with `:vectorz` as default implementations. 
You can use another implementation by adding corresponding dependency to your project file and selecting implementation using `set-current-implementation`.
Note that the selected implementation must support `emap!` from the core.matrix API.

## Algorithms

### Lance-Williams

The [Lance–Williams algorithms][lance-williams] are an infinite family of agglomerative [hierarchical clustering][hc] algorithms which are represented by a recursive formula for updating cluster distances at each step (each time a pair of clusters is merged).

The multimethod `lw-updater` allows for custom implementations of such algorithms.
Available implementations are:

* `:single-link` uses min when merging
* `:complete-link` uses max when merging
* `:ward` minimizes the total within-cluster variance when merging 

## Visualization

### SVG format

A simple [batik][apache-batik] based `JFrame` visualisation is available (note that it's possible to get just the SVG string):

```clj
(require '[clj-hclust.batik :as b])

(-> M
    (hc/hclust-lw :single-link)
    (hc/hclust->svg)
    (b/svg-jframe 600 100))
```

SVG visualisation parameters can be customized:

```clj
(-> M
    (hc/hclust-lw :single-link)
    (hc/hclust->svg {:names ["Rochefort" "Milady" "Athos" "Portos" "Aramis"]
                     :circle-style {:r 10}
                     :text-style {:dx 16 :dy 6 :font "14px sans-serif"}})
    (b/svg-jframe 700 150))
```

### Newick format

A [newick format][newick] visualisation is also available:

```clj
(-> M
    (hc/hclust-lw :single-link)
    (hc/hclust->newick))

;;=> "(((0:0,1:0):0.5,((2:0,3:0):1.12,4:0):1.41):2.24);"
```

## License

Copyright &copy; Romain Leroux

This project is licensed under the [Eclipse Public License 1.0][license].

[hc]: https://en.wikipedia.org/wiki/Hierarchical_clustering

[lance-williams]: https://en.wikipedia.org/wiki/Ward%27s_method#Lance.E2.80.93Williams_algorithms

[apache-batik]: https://xmlgraphics.apache.org/batik/

[newick]: https://en.wikipedia.org/wiki/Newick_format

[license]: http://www.eclipse.org/legal/epl-v10.html
