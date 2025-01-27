

n_paramaters_var <- function(p){
  p+p^2
}


n_paramaters_hmm <- function(p,n){
  n-1+n*(n-1)+n*p*2
}





layout(1)
par(mar = c(3,3,0,0))
plot.new()
plot.window(xlim = c(1,20), ylim = c(0,400))
plot(x = 1:20, y = n_paramaters_var(1:20),
     type = "l",
     axes = FALSE, 
     xlab = "vars",
     ylab = "pars", 
     lwd = 2)
lines(x = 1:20, y = n_paramaters_hmm(1:20,2), 
      type = "l", lwd = 2)
lines(x = 1:20, y = n_paramaters_hmm(1:20,3), 
      type = "l", lwd = 2)
lines(x = 1:20, y = n_paramaters_hmm(1:20,4), 
      type = "l", lwd = 2)
lines(x = 1:20, y = n_paramaters_hmm(1:20,5), 
      type = "l", lwd = 2)
axis(1, at = 1:20, labels = 1:20 )
axis(2, at = seq(0,400, 50), labels = seq(0,400, 50))