# jaccard measure
# knn graph

library(igraph)
library(cccd)
install.packages("FNN")
library(FNN)

red_dataset <- read.csv("datasets/winequality-red.csv", header = TRUE, sep = ";")
white_dataset <- read.csv("datasets/winequality-white.csv", header = TRUE, sep = ";")

save_csv <- function(matrix) {
  filename <- deparse(substitute(matrix))
  saveTo <- paste0("outputs/", filename, ".csv")
  write.csv(matrix, file = saveTo)
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
  #make the row names the year column
  formatted_matrix <- dataset_matrix[, -1]
  return(formatted_matrix)
}

calc_jaccard_measure <- function(A, B) {
  return(sum(A != B) / sum(A | B))
}

calc_mst <- function(dist_matrix) {
  graph <- graph.adjacency(
    as.matrix(dist_matrix), mode = "undirected", weighted = TRUE)
  mst <- mst(graph)
  E(mst)$weight <- round(E(mst)$weight, 3)
  return(mst)
}

print_line <- function(text) {
  cat(text, "\n")
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

calc_jaccard_col <- function(input_matrix) {
  jaccard_measure_matrix <- matrix(
    0,
    nrow = ncol(input_matrix),
    ncol = ncol(input_matrix),
    dimnames = list(
      colnames(input_matrix),
      colnames(input_matrix)))
  for (x in 1:ncol(input_matrix)) {
    for (y in 1:ncol(input_matrix)) {
      jaccard_measure_matrix[x, y] <-
        calc_jaccard_measure(input_matrix[, x], input_matrix[, y])
    }
  }
  return(jaccard_measure_matrix)
}



get_graph_layout <- function(graph) {
  layout <- layout_with_fr(graph)
  return(layout)
}
save_graph <- function(graph, filename) {
  saveTo <- paste0("outputs/", filename, ".graphml")
  write_graph(graph, file = saveTo, format = "graphml")
}
plot_graph <- function(graph) {
  filename <- deparse(substitute(graph))
  layout <- layout_with_fr(graph)
  plot(
    graph,
    vertex.size=2,
    layout = layout,
    main = filename,
    edge.label = NA,
    vertex.label = NA
    # edge.label = E(graph)$weight,
    # vertex.label = V(graph)$name
  )
  save_graph(graph, filename)
  #graph
}
#==================================================

formatted_matrix <- get_formatted_matrix(red_dataset)
print_line("Formatted Matrix:")
formatted_matrix

print_line("Ex. 1.a. Jaccard Matrix (Row-wise):")
jaccard_row <- calc_jaccard_row(formatted_matrix)
save_csv(jaccard_row)
#jaccard_row

print_line("Ex 1.b. Jaccard Matrix (Column-wise):")
jaccard_col <- calc_jaccard_col(formatted_matrix)
save_csv(jaccard_col)
#jaccard_col

print_line("Ex 3. MST for Jaccard Measure (Column-wise):")
jaccard_mst_col <- calc_mst(jaccard_col)
plot_graph(jaccard_mst_col)

print_line("Ex 3. MST for Jaccard Measure (Row-wise):")
jaccard_mst_row <- calc_mst(jaccard_row)
plot_graph(jaccard_mst_row)
