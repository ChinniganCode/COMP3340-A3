library(arules)
library(igraph)
print_line <- function(text) {
  cat(text, "\n")
}


save_csv <- function(matrix) {
  filename <- deparse(substitute(matrix))
  saveTo <- paste0("outputs/", filename, ".csv")
  write.csv(matrix, file = saveTo)
}
binarize_based_on_median <- function(df) {
  for (column_name in colnames(df)) {

    if (column_name != "quality") {
      median_value <- median(df[[column_name]], na.rm = TRUE)
      df[[column_name]] <- ifelse(df[[column_name]] > median_value, 1, 0)
    }
  }
  return(df)
}
calc_jaccard_measure <- function(A, B) {
  return(1 - (sum(A != B) / sum(A | B)))
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
  plot(graph,
       layout = layout,
       vertex.size = 2,
       vertex.label.color="black",
       vertex.label.cex = 0.6,
       edge.label.cex = 0.6,
       label.cex=0.8,
       color="black",
       edge.label= E(graph)$weight,
       vertex.label = NA,
       main = filename)
  save_graph(graph, filename)
  #graph
}

red_dataset <- read.csv("datasets/winequality-red.csv", header = TRUE, sep = ";")

red_disc <- binarize_based_on_median(red_dataset)
red_disc
formatted_matrix <- get_formatted_matrix(red_disc)

print_line("Ex. 1.a. Jaccard Matrix (Row-wise):")
jaccard_row <- calc_jaccard_row(formatted_matrix)
save_csv(jaccard_row)
#jaccard_row

print_line("Ex 3. MST for Jaccard Measure (Row-wise):")
jaccard_mst_row <- calc_mst(jaccard_row)
plot_graph(jaccard_mst_row)
