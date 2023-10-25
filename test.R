
x <- matrix(runif(500),ncol=2)

# g <- rng(x)
# ## Not run:
# plot(g, edge.width=0.5 main="rng")

## End(Not run)

## Example using 'open':
g <- graph.full(5,directed=FALSE)

g1 <- rng(x=get.adjacency(g,sparse=FALSE),open=TRUE)
ecount(g1)
g2 <- rng(x=get.adjacency(g,sparse=FALSE),open=FALSE)
graph.isomorphic(g2,g)
plot(g2)