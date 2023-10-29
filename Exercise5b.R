library(arules)
library(igraph)
library("mstknnclust")
print_line <- function(text) {
  cat(text, "\n")
}


save_csv <- function(matrix) {
  filename <- deparse(substitute(matrix))
  saveTo <- paste0("outputs/", filename, ".csv")
  write.csv(matrix, file = saveTo)
}

equal_width_binning <- function(df, bins=2) {
  for (column_name in colnames(df)) {
    if (column_name != "quality") {
      df[[column_name]] <- as.integer(cut(df[[column_name]], breaks = bins, labels = FALSE))
    }
  }
  return(df)
}


calc_jaccard_measure <- function(A, B) {
  return((1 - (sum(A != B) / sum(A | B))))
}
calc_mst <- function(dist_matrix) {
  graph <- graph.adjacency(
    as.matrix(dist_matrix), mode = "undirected", weighted = TRUE)
  mst <- mst(graph)
  E(mst)$weight <- round(E(mst)$weight, 3)
  return(mst)
}
get_formatted_matrix <- function(dataset) {
  dataset_matrix <- as.matrix(
    dataset,
    nrow = nrow(data),
    ncol = ncol(data))
  #remove the target column
  dataset_matrix <- dataset_matrix[
    , colnames(dataset_matrix) != "quality"
  ]
  colnames(dataset_matrix) <- 1:ncol(dataset_matrix)
  return(dataset_matrix)
}

calc_jaccard_row <- function(input_matrix) {
  jaccard_measure_matrix <- matrix(
    0,
    nrow = nrow(input_matrix),
    ncol = nrow(input_matrix),
    dimnames = list(rownames(input_matrix), rownames(input_matrix))
  )
  for (x in 1:nrow(input_matrix)) {
    for (y in 1:nrow(input_matrix)) {
      jaccard_measure_matrix[x, y] <-
        calc_jaccard_measure(input_matrix[x,], input_matrix[y,])
    }
  }
  return(jaccard_measure_matrix)
}

save_graph <- function(graph, filename) {
  saveTo <- paste0("outputs/", filename, ".graphml")
  write_graph(graph, file = saveTo, format = "graphml")
}

# red_dataset <- head(read.csv("datasets/winequality-red.csv", header = TRUE, sep = ";"), 500)
red_dataset <- read.csv("datasets/winequality-red.csv", header = TRUE, sep = ";")

classifications <- red_dataset$quality
smallest_quality <- min(classifications)
largest_quality <- max(classifications)

red_disc <- equal_width_binning(red_dataset)
formatted_matrix <- get_formatted_matrix(red_disc)

print_line("Ex. 1.a. Jaccard Matrix (Row-wise):")
jaccard_row <- calc_jaccard_row(formatted_matrix)

save_csv(jaccard_row)
# neat matrix for plotting
mm <- matrix(formatted_matrix, nrow=nrow(formatted_matrix), ncol=nrow(formatted_matrix))

cg <- generate.complete.graph(1:nrow(mm),jaccard_row)

color_palette <- colorRampPalette(c("red", "green"))(largest_quality - smallest_quality + 1)
vertex_colors <- color_palette[classifications - smallest_quality + 1]



mstree <- generate.mst(cg)


plot(mstree$mst.graph,
     vertex.size = 4,
     vertex.label = classifications,
     vertex.color=vertex_colors,
     layout=igraph::layout.fruchterman.reingold(mstree$mst.graph, niter=10000),
     # layout=igraph::layout.fruchterman.reingold(mstree$mst.graph, niter=10000, area=nrow(formatted_matrix)^3),
     # edge.label= round(E(mstree$mst.graph)$weight, 3),
     main="MST",)
save_graph(mstree$mst.graph, "MST-Graph")

knntree <- generate.knn(cg, suggested.k=5)

plot(knntree$knn.graph,
     vertex.size = 4,
     vertex.label = classifications,
     vertex.color=vertex_colors,
     layout=igraph::layout.fruchterman.reingold(knntree$knn.graph, niter=10000),
     # layout=igraph::layout.fruchterman.reingold(knntree$knn.graph, niter=10000, area=nrow(formatted_matrix)^3),
     # edge.label= E(knntree$knn.graph)$weight,
     main="KNN",)
save_graph(knntree$knn.graph, "kNN-Graph")

results <- mst.knn(jaccard_row, 5)
plot(results$network, vertex.size=4,
     vertex.color=igraph::clusters(results$network)$membership,
     layout=igraph::layout.fruchterman.reingold(results$network, niter=10000),
     main=paste("MST-kNN \n Clustering solution \n Number of clusters=",results$cnumber,sep="" ))
save_graph(results$network, "ClusterGraph")