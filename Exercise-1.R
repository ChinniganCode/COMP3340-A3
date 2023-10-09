library("cccd")
library(datasets)


redwine_dataset <- read.csv("datasets/winequality-red.csv", header=TRUE, sep=";")
#
 head(redwine_dataset)
matrix <- as.matrix(redwine_dataset)
head(matrix)
print("RUN TRAINING")
 rng_graph <- rng(x=matrix, method="euclidean", algorithm = 'cover_tree', k=2, open=TRUE, r = 0.1)
layout <- layout_with_fr(rng_graph)
plot(rng_graph, layout = layout)
