library(ISLR2)
library(tidyverse)

# Ex. 7.
arrests_ex <- USArrests

# Correlation-based distance:
dist_cor <- as.dist(1 - cor(t(scale(arrests_ex))))
# Squared Euclidean distance
dist_euc <- dist(scale(arrests_ex)) ^ 2 

# Approximate summary of proportion between correlation-based and Euclidean distances:
summary(dist_cor / dist_euc)

# Assuming that dist_cor ~ median_proportion * dist_euc...
summary(dist_cor - median(dist_cor / dist_euc) * dist_euc)
#... we can see that mean difference between dist_cor and median_proportion * dist_euc is only 0.06.

# Ex. 8.

# a)
arrests_ex %>%
  prcomp(., scale = T) -> pca_arrests_1
pca_arrests_1$sdev ^ 2 / sum(pca_arrests_1$sdev ^ 2)
# 0.62006039 0.24744129 0.08914080 0.04335752

# b)
apply((as.matrix(scale(arrests_ex)) %*% pca_arrests_1$rotation) ^ 2, 2, sum) /
  sum(apply(as.matrix(scale(arrests_ex)) ^ 2, 2, sum))
# 0.62006039 0.24744129 0.08914080 0.04335752

# Ex. 9.
# a)
arrests_ex %>%
  dist() %>%
  hclust(method = 'complete') -> hclust_arrests_1
hclust_arrests_1 %>% plot()

# b)
hclust_arrests_1 %>%
  cutree(k = 3)

# c)
arrests_ex %>%
  scale() %>%
  dist() %>%
  hclust(method = 'complete') -> hclust_arrests_2
hclust_arrests_2 %>% plot()

hclust_arrests_2 %>%
  cutree(k = 3)

# d)
arrests_ex %>%
  dist() %>%
  ClustGeo::withindiss(part = cutree(hclust_arrests_1, k = 3)) # 959.29

arrests_ex %>%
  dist() %>%
  ClustGeo::inertdiss() # 7116.16

arrests_ex %>%
  scale() %>%
  dist() %>%
  ClustGeo::withindiss(part = cutree(hclust_arrests_2, k = 3)) # 1.69

arrests_ex %>%
  scale() %>%
  dist() %>%
  ClustGeo::inertdiss() # 3.92

# Both total inertion and inertion inside clusters are way lower for scaled dataset.
# Variables should be scaled if they are of different measures.

# Ex. 10.

# a)
set.seed(1)
x <- matrix(rnorm(60 * 50), ncol = 50)
x[1:20, ] <- x[1:20, ] + 5
x[21:40, ] <- x[21:40, ] + 10
x[41:60, ] <- x[41:60, ] + 15
group <- c(rep(1, 20), rep(2, 20), rep(3, 20))

# b)
x %>%
  prcomp(., scale = T) -> pca_x_1
pca_x_1 %>%
  factoextra::fviz_pca_biplot()

# c)

x %>%
  kmeans(., 3, nstart = 20) -> kmeans_x_1
table(group, kmeans_x_1$cluster)
# All observations are properly classified.

# d)

x %>%
  kmeans(., 2, nstart = 20) -> kmeans_x_2
table(group, kmeans_x_2$cluster)
# 1/3 of observations are incorrectly classified. All of them were clustered with observations from another group.

# e)

x %>%
  kmeans(., 4, nstart = 20) -> kmeans_x_3
table(group, kmeans_x_3$cluster)
# 10 observations are incorrectly classified. All of them are in additional cluster.

# f)

x[, 1:2] %>%
  kmeans(., 3, nstart = 20) -> kmeans_x_4
table(group, kmeans_x_4$cluster)
# All observations are properly classified.
  
# g)

x %>%
  scale() %>%
  kmeans(., 3, nstart = 20) -> kmeans_x_5
table(group, kmeans_x_5$cluster)
# All observations are properly classified, just like in b).

# Ex. 11.
# Ex. 12.
# I see no reason to reinvent the wheel.

# Ex. 13.
# a)
gene_ex <- read.csv('https://www.statlearning.com/s/Ch12Ex13.csv', header = F)

# b)
as.dist(1 - cor(t(scale(gene_ex)))) %>%
  hclust(method = 'complete') -> hclust_gene_1
hclust_gene_1 %>% plot()
as.dist(1 - cor(t(scale(gene_ex)))) %>%
  hclust(method = 'average') -> hclust_gene_2
hclust_gene_2 %>% plot()
as.dist(1 - cor(t(scale(gene_ex)))) %>%
  hclust(method = 'single') -> hclust_gene_2
hclust_gene_2 %>% plot()
# Complete linkage provides two visible clusters. Average and single provide less readable results.

# c)
gene_ex %>%
  prcomp() -> pca_gene_1
pca_gene_1$rotation %>%
  apply(MARGIN = 1, FUN = sum) %>%
  abs() %>%
  order(decreasing = T) %>%
  head()











