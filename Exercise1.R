library("cccd")
library("igraph")

concerete_dataset <- read.csv("datasets/Concrete_Data.csv", header = TRUE, sep = ",")
#concerete_dataset <- concerete_dataset[1:25, ]

get_species_rng <- function(dataset) {
  proteins_dist_matrix <- dist(dataset, method = "euclidean")
  proteins_matrix <- as.matrix(proteins_dist_matrix)
  rng_protiens <- compute_rng(proteins_matrix)
  write.csv(proteins_matrix, "outputs/proteins_matrix.csv")
  return(rng_protiens)
}

get_samples_rng <- function(dataset) {
  write.csv(dataset, "outputs/samples_subset.csv")
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

      # print(paste0("pair_dist:", toString(pair_dist)))
      # print(paste0("distsfromi:", toString(dists_from_i)))
      # print(paste0("distsfromj:", toString(dists_from_j)))
      #if any point is closer to i or j than pair -> not rng
      is_closer <- FALSE
      for (k in 1:length(dists_from_i)) {
        comp_i <- dists_from_i[k] < pair_dist
        comp_j <- dists_from_j[k] < pair_dist

        # print(paste("Comparison of [", dists_from_i[k], "] is less than: [", pair_dist, "] IS:",comp_i))
        # print(paste("Comparison of [", dists_from_j[k], "] is less than: [", pair_dist, "] IS:", comp_j))

        if (comp_i & comp_j) {
          is_closer <- TRUE
          break
        }
      }
      # print(paste("i:", i, "j:", j, "iscloser:", is_closer))

      #if none closer -> rng
      if (!is_closer) {
        #print(paste("none closer= pair_dist:", pair_dist, "i:", i, "j:", j))
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
g1 <- as.undirected(g1)
write.graph(g1, file = "outputs/samples_rng.graphml", format = "graphml")
layout <- layout_with_fr(g2)
plot(g1,
     layout = layout,
     edge.curved = 0.2,
     vertex.size = 5,
     label.color="black",
     label.cex=0.2,
     color="black",
     vertex.color = "red",
     edge.label= E(g1)$weight,
     main = "species_rng")


species_rng <- get_species_rng(concerete_dataset)
write.csv(species_rng, "outputs/species_rng.csv")
node_lables <-
g2 <- graph_from_adjacency_matrix(species_rng, mode = "undirected", weighted = TRUE, diag = TRUE)
E(g2)$weight <- round(E(g2)$weight, digits = 2)
V(g2)$label <- seq_len(vcount(g2))
g2 <- as.undirected(g2)
write.graph(g2, file = "outputs/species_rng.graphml", format = "graphml")

layout <- layout_with_fr(g2)
plot(g2,
     layout = layout,
     edge.curved = 0.2,
     vertex.size = 5,
     label.color="black",
     label.cex=0.2,
     color="black",
     vertex.color = "red",
     edge.label= E(g2)$weight,
     main = "species_rng")
