orthosmooth.randtest <-
function(x, MEM, nrepet = 999, by = 7, length = 7){
  R2 <- as.vector(cor(x, MEM)^2)
  debut <- seq(1,by = by, length = length)
  fin <- seq(by, by = by, length = length)
  R2.smooth <- colSums(sapply(1:length, function(x) R2[debut[x]:fin[x]]))
  sim <- matrix(0, nrepet, length)
  for(i in 1:nrepet){
    R2.sim <- as.vector(cor(sample(x), MEM)^2)
    sim[i, ] <- colSums(sapply(1:length, function(x) R2.sim[debut[x]:fin[x]]))
  }
  return(list(obs=R2.smooth, sim=sim))
}
