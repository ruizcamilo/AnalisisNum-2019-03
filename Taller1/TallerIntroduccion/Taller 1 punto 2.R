raiz <- function(n, E, x)
{
  y <- 0.5*( x + n / x);
  k<-abs(x-y)
 
  while(k > E){
    x <- y
    y <- (1/2)*(x+(n/x))
    k <- abs(x-y)
  }
  return(cat("El resultado es: ", y, " con error de ", E))
}
#Evalua la raiz de 7 con un error permitido de 1x10^-8
raiz(7,0.0000000000000001, 100)