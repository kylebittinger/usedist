
<!-- README.md is generated from README.Rmd. Please edit that file -->
usedist
=======

This package provides useful functions for distance matrix objects in R.

Installation
------------

You can install usedist from github with:

``` r
# install.packages("devtools")
devtools::install_github("usedist/kylebittinger")
```

Utility functions
-----------------

Let's say we have a distance object representing the distances between six rows of data:

``` r
vals <- matrix(rnorm(30), nrow=6)
rownames(vals) <- c("A", "B", "C", "D", "E", "F")
vals
```

    ##         [,1]       [,2]        [,3]       [,4]       [,5]
    ## A  0.5244946  1.5128781 -1.21602889 -1.0436840 -0.3957575
    ## B -0.8356313 -0.9271118 -0.01845765  0.1583214 -0.1283055
    ## C  0.8635594  0.5730635 -0.27285275 -2.4057078 -0.3737200
    ## D -0.7854537  0.5654362  0.05767699  0.2072611  0.5684050
    ## E -1.6309083 -1.3014575 -0.18939698  0.5354129  0.3820710
    ## F  0.3673909 -1.0765648  1.49786007 -0.7767631  0.4379558

``` r
d <- dist(vals)
d
```

    ##          A        B        C        D        E
    ## B 3.279332                                    
    ## C 1.934785 3.440489                           
    ## D 2.594303 1.650398 3.247115                  
    ## E 4.088861 1.097498 4.354833 2.098460         
    ## F 3.855051 2.227971 3.068413 2.661704 2.935224

The `usedist` package allows us to select the distances for rows B, C, F, and D, *in that order*:

``` r
library(usedist)
```

``` r
dist_subset(d, c("B", "C", "F", "D"))
```

    ##          B        C        F
    ## C 3.440489                  
    ## F 2.227971 3.068413         
    ## D 1.650398 3.247115 2.661704

This is helpful when arranging a distance matrix to match a data frame, for instance with the `adonis` function in `vegan`.

We can extract distances between specified pairs of items, for example distances between rows A-to-C and B-to-E. To do this, we provide two vectors of items: one for the item of origin, and another for the destination.

``` r
origin_row <- c("A", "B")
destination_row <- c("C", "E")
dist_get(d, origin_row, destination_row)
```

    ## [1] 1.934785 1.097498

If items are arranged in groups, we can make a data frame listing the distances between items in group 1 and group 2:

``` r
item_groups <- rep(c("Control", "Treatment"), each=3)
dist_groups(d, item_groups)
```

    ##    Item1 Item2    Group1    Group2                         Label Distance
    ## 1      A     B   Control   Control                Within Control 3.279332
    ## 2      A     C   Control   Control                Within Control 1.934785
    ## 3      A     D   Control Treatment Between Control and Treatment 2.594303
    ## 4      A     E   Control Treatment Between Control and Treatment 4.088861
    ## 5      A     F   Control Treatment Between Control and Treatment 3.855051
    ## 6      B     C   Control   Control                Within Control 3.440489
    ## 7      B     D   Control Treatment Between Control and Treatment 1.650398
    ## 8      B     E   Control Treatment Between Control and Treatment 1.097498
    ## 9      B     F   Control Treatment Between Control and Treatment 2.227971
    ## 10     C     D   Control Treatment Between Control and Treatment 3.247115
    ## 11     C     E   Control Treatment Between Control and Treatment 4.354833
    ## 12     C     F   Control Treatment Between Control and Treatment 3.068413
    ## 13     D     E Treatment Treatment              Within Treatment 2.098460
    ## 14     D     F Treatment Treatment              Within Treatment 2.661704
    ## 15     E     F Treatment Treatment              Within Treatment 2.935224

Also, we provide a function to compute user-defined distances between rows in the data matrix:

``` r
bray_curtis_distance <- function (x1, x2) sum(abs(x1 - x2)) / sum(x1 + x2)
dist_make(vals, bray_curtis_distance)
```

    ##               A             B             C             D             E
    ## B    -2.7295793                                                        
    ## C    -1.6143743    -1.8602607                                          
    ## D -1204.1513684    -2.0780341    -5.5293697                            
    ## E    -2.9596676    -0.5632791    -2.1333748    -2.1835388              
    ## F   -39.0032103    -3.3582714    -5.4531192     5.0314896    -3.0087266

Centroid functions
------------------

The `usedist` package contains functions for computing the distance to group centroid positions. This is accomplished without finding the location of the centroids themselves, though it is assumed that some high-dimensional Euclidean space exists where the centroids can be situated. References for the formulas used can be found in the function documentation.

To illustrate, let's create a set of points in 2-dimensional space. Four points will be centered around the origin, and four around the point (3, 0).

``` r
pts <- data.frame(
  x = c(-1, 0, 0, 1, 2, 3, 3, 4),
  y = c(0, 1, -1, 0, 0, 1, -1, 0),
  Item = LETTERS[1:8],
  Group = rep(c("Control", "Treatment"), each=4))

library(ggplot2)
ggplot(pts, aes(x=x, y=y)) +
  geom_point(aes(color=Group)) +
  geom_text(aes(label=Item), hjust=1.5)
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

Now for the trick: we'll take the distances between points, then figure out the distances to the group centroids using only the point-to-point distances.

``` r
pts_data <- pts[,1:2]
rownames(pts_data) <- pts$Item
pts_distances <- dist(pts_data)
pts_distances
```

    ##          A        B        C        D        E        F        G
    ## B 1.414214                                                      
    ## C 1.414214 2.000000                                             
    ## D 2.000000 1.414214 1.414214                                    
    ## E 3.000000 2.236068 2.236068 1.000000                           
    ## F 4.123106 3.000000 3.605551 2.236068 1.414214                  
    ## G 4.123106 3.605551 3.000000 2.236068 1.414214 2.000000         
    ## H 5.000000 4.123106 4.123106 3.000000 2.000000 1.414214 1.414214

First, we use the function `dist_between_centroids` to calculate the distance between the centroids of the two groups. In our example, we expect to get a distance of 3.

``` r
dist_between_centroids(
  pts_distances, c("A", "B", "C", "D"), c("E", "F", "G", "H"))
```

    ## [1] 3

Now, we use the function `dist_to_centroids` to calculate the distance from each individual point to the group centroids. The within-group distances should all be equal to 1.

``` r
dist_to_centroids(pts_distances, pts$Group)
```

    ##    Item CentroidGroup CentroidDistance
    ## 1     A       Control         1.000000
    ## 2     B       Control         1.000000
    ## 3     C       Control         1.000000
    ## 4     D       Control         1.000000
    ## 5     E       Control         2.000000
    ## 6     F       Control         3.162278
    ## 7     G       Control         3.162278
    ## 8     H       Control         4.000000
    ## 9     A     Treatment         4.000000
    ## 10    B     Treatment         3.162278
    ## 11    C     Treatment         3.162278
    ## 12    D     Treatment         2.000000
    ## 13    E     Treatment         1.000000
    ## 14    F     Treatment         1.000000
    ## 15    G     Treatment         1.000000
    ## 16    H     Treatment         1.000000

Double-checking the between-group distances is left as an exercise for the reader.
