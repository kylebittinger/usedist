# usedist 0.1.0

Initial release!

For the first release, we've moved over all the distance matrix-related
functions from the `qiimer` package.  Utility functions `dist_setNames` and
`dist_make` were added to the existing functions `dist_subset, `dist_get`, and
`dist_groups`.

The other area of focus for our inital release was a set of distance-to-centroid
functions.  These functions operate on a set of items that are placed into
several groups, and they apply algebraic formulas to find the group centroid
distances based on the distance matrix.  If you had a set of points in normal
Euclidean space, you could easily find the centroid positions and compute the
distances from there.  The centroid functions included in the package are more
useful for non-Euclidean distances, where it is less striaghtforward to locate
the centroids in space.  The centroid functions were inspired by work in
microbial ecology, where it is typical to work with non-Euclidean distances
between samples.
