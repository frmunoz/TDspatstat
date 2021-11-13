plotsmooth <-
function(obj){
  par(mar = c(3,2,1.5,0.5))
  themax <- which.max(obj$obs)
  thecol <- rep("grey80", 7)
  thecol[themax] <- 'grey40'
  debut <- seq(1,by = 7, length = 7)
  fin <- seq(7,by = 7, length = 7)
  mp <- barplot(obj$obs, col = thecol, ylim = c(0,1), axes = FALSE)
  axis(2,at=c(0,1),las=1,tcl=-0.3)
  axis(1, at = mp[,1], label = 1:7, tcl = 0, padj = -1)
  mtext(1, text = "Smoothed MEMs", padj = 3)
  mtext(2, text = expression(R^2), padj = -1)
  qu95 <- apply(obj$sim, 2, quantile, 0.95)
  points(mp[,1], qu95, ty = 'b', pch = 3, lty = 3)
  text(mp[themax], obj$obs[themax], obj$pvalue[themax], pos = 3)
}
