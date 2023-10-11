library("cccd")
library("igraph")

concerete_dataset <- read.csv("datasets/Concrete_Data.csv", header = TRUE, sep = ",")

get_species_rng <- function(dataset) {
  proteins_dist_matrix <- dist(dataset, method = "euclidean")
  proteins_matrix <- as.matrix(proteins_dist_matrix)
  rng_protiens <- compute_rng(proteins_matrix)
  write.csv(proteins_matrix, "outputs/proteins_matrix.csv")
  return(rng_protiens)
}

get_samples_rng <- function(dataset) {
  samples_dist_matrix <- dist(t(dataset), method = "euclidean")
  samples_dist_matrix <- as.matrix(samples_dist_matrix)
  write.csv(samples_dist_matrix, "outputs/samples_dist_matrix.csv")
  rng_samples <- compute_rng(samples_dist_matrix)
  return(rng_samples)
}


compute_rng <- function(distance_matrix) {
  num_rows <- nrow(distance_matrix)
  #init empty matrix
  rng <- matrix(0, nrow = num_rows, ncol = num_rows)

  # 1 to n-1
  for (i in seq_len(num_rows - 1)) {
    #i+1 to n
    for (j in seq(i + 1, num_rows)) {
      pair_dist <- distance_matrix[i, j]

      #vector of all dists from i to other points, ex i,j
      dists_from_i <- distance_matrix[i, -c(i, j)]
      #vector of all dists from j to other points, ex i,j
      dists_from_j <- distance_matrix[j, -c(i, j)]

      #if any point is closer to i or j than pair -> not rng
      is_closer <- any(dists_from_i < pair_dist & dists_from_j < pair_dist)
      #if none closer -> rng
      if (!is_closer) {
        #mirror for undir
        rng[i, j] <- pair_dist
        rng[j, i] <- pair_dist
      }
    }
  }

  return(rng)
}

# Compute RNG for samples
samples_rng <- get_samples_rng(concerete_dataset)
g1 <- graph_from_adjacency_matrix(samples_rng, mode = "undirected", weighted = TRUE)
V(g1)$label <- colnames(concerete_dataset)
E(g1)$weight <- round(E(g1)$weight, digits = 2)

plot(g1, vertex.size = 5, edge.width = 0.5, edge.label = E(g1)$weight, main = "samples_rng")


species_rng <- get_species_rng(concerete_dataset)
g2 <- graph_from_adjacency_matrix(species_rng, mode = "undirected", weighted = TRUE)
E(g2)$weight <- round(E(g2)$weight, digits = 2)
layout <- layout_with_fr(g2)
plot(g2, layout = layout, vertex.size = 2, vertex.label = NA, edge.width = 1, edge.label = NA, main = "species_rng")

