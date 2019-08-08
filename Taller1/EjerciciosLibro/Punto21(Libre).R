Steffensen = function(f,x0,tol)
{
  Errores = c()
  Iteraciones = c()
  Errori = c()
  Errorj = c()
  cat(formatC(c("i","x_i","f(x)","Error est."), width = -15, format = "f", flag = " "),"\n")
  maxi = 1000
  i = 1
  while (i <= 1000)
  {
    Iteraciones = c(Iteraciones,i)
    x1 = x0 - ((f(x0))^2)/(f(x0+f(x0))-f(x0))
    Error = abs(x1 - x0)
    Errores = c(Errores, Error)
    x0 = x1
    cat(formatC( c(i,x0,f(x0),Error), digits = 8, width = -15, format = "f", flag = " "), "\n")
    if(Error < tol)
      break;
    i = i + 1
    
  }
  cat("Cero de funcion: ", x0, " con error <=", Error, "Iteraciones: ", i)
  plot(Iteraciones,Errores, type = "l", xlab = "No. Iteraciones",ylab="Error")
  #Errores Ei vs Ei+1
  for(b in 1:i){
    if(b!=i){
      Errori[b]=Errores[b]
      Errorj[b]=Errores[b+1]  
    }
  }
}
#Se usa steffenson modificando la funcion original
f = function(x) 5*x+2*(exp(0.1*x))-200
Steffensen(f, 0, 1e-8)