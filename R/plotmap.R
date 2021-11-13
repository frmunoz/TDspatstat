plotmap <-
function()
{
  par(mar = rep(0.1, 4))
  sp::plot(pcw$map, xlim = c(-80.1, -79), ylim = c(8.7,9.5), col ='grey80', pbg ='grey40', bg ='grey40', border ='grey40')
}
