horner <- function(c, x){
  res <- c[1]
  i <-0
  
  for(k in c[2:length(c)]){
    res <- x*res + k
    i <- i + 2
  }
  return(cat("Resultado: ", res, "\nNumero de operaciones: ", i))
}
x0 <- -2
c <- c(2,0,-3,3,-4)
horner(c,x0)
