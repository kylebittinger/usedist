# usedist 0.3.0

We've made two major updates for this release.

The centroid functions have gained a keyword argument, `squared`. The final
step in computing distance to group centroids involves taking a square root.
Sometimes, we end up with a negative number inside the square root. Normally,
this produces `NaN` as a result. However, if `squared` is set to `TRUE`, we
don't take the square root and the result is always a real number. The default
setting is `squared = FALSE`, which gives the distance as you'd expect. Thanks
to Sam Ross for helpful advice on this topic.

We added a new function, `pivot_to_numeric_matrix()`. This function takes a
data frame in long format and converts to a matrix suitable for distance
calculations. Long-format data frames are commonly used with functions in the
`tidyverse`, and proper conversion to a matrix requires a few non-obvious
steps. The packages `dplyr`, `tidyr`, and `tibble` are needed to run the
function, and have been added as suggested packages for `usedist`.

During development, we had implemented an additional function to create a
distance matrix directly from a data frame in long format. However, we found
that it was nearly as convenient to use `pivot_to_numeric_matrix()` and
`dist_make()` together to achieve the same result. We added an example to the
README file to illustrate this.

# usedist 0.1.0

Initial release!

For the first release, we've moved over all the distance matrix-related
functions from the `qiimer` package.  Utility functions `dist_setNames` and
`dist_make` were added to the existing functions `dist_subset`, `dist_get`, and
`dist_groups`.

The other area of focus for our initial release was a set of
distance-to-centroid functions.  These functions operate on a set of items
that are placed into several groups, and they apply algebraic formulas to find
the group centroid distances based on the distance matrix.  If you had a set
of points in normal Euclidean space, you could easily find the centroid
positions and compute the distances from there.  The centroid functions
included in the package are more useful for non-Euclidean distances, where it
is less straightforward to locate the centroids in space.  The centroid
functions were inspired by work in microbial ecology, where it is typical to
work with non-Euclidean distances between samples.
