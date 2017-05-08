# usedist

This package provides useful functions for distance matrix objects in R.

## Installation

You can install usedist from github with:

```R
# install.packages("devtools")
devtools::install_github("usedist/kylebittinger")
```

## Example

Let's say we have a distance object representing the distances between six rows
of data:

```R
vals <- matrix(rnorm(30), nrow=6)
rownames(vals) <- c("A", "B", "C", "D", "E", "F")
d <- dist(vals)
```

The `usedist` package allows us to select the distances only for rows B, A, and
F, *in that order*:

```R
dist_subset(d, c("B", "A", "F"))
```

This can be enormously helpful when arranging a distance matrix to match a data
frame, for instance with the `adonis` function in `vegan`.

We can extract distances between specified pairs of items, for example distances
between rows A-to-C and B-to-E:

```R
origin_row <- c("A", "B")
destination_row <- c("C", "E")
dist_get(d, origin_row, destination_row)
```

If items are arranged in groups, we can make a data frame listing the distances
between items in group 1 and group 2:

```R
item_groups <- rep(c("Control", "Treatment"), each=3)
dist_groups(d, item_groups)
```

Also, we provide a function to compute user-defined distances between
rows in the data matrix:

```R
bray_curtis_distance <- function (x1, x2) sum(abs(x1 - x2)) / sum(x1 + x2)
dist_make(d, bray_curtis_distance)
```


