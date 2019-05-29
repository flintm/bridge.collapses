my.kernel.interp <- function(data,to=2000,t.interp=0:2000){
  T.kernel <- density(data,
                      bw = 'nrd',
                      adjust = 2,
                      from = 0, to = to)
  T.kernel.interp <- approxExtrap(T.kernel$x,T.kernel$y, t.interp)
}