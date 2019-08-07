FalsaPos= function(f, x0, x1, maxiter, tol)
{
  f0 = f(x0)
  f1 = f(x1)
  Iteraciones = c()
  Errores = c()
  Errori = c()
  Errorj = c()
  i = 1
  last = x1
  cat(formatC(c("i","x_i","f(x)","Error est."), width = -15, format = "f", flag = " "),"\n")
  while(i <= maxiter)
  {
    x2 =(x0*f1-x1*f0)/(f1-f0)
    f2 = f(x2)
    if(abs(f2)<= tol)
    {
      break
    }
    Iteraciones = c(Iteraciones, i)
    Error = abs(x1 - x0)
    Errores = c(Errores, Error)
    if(sign(f2) == sign(f0))
    {
      x0 = x2
      last = x2
      f0 = f2
    }
    else
    {
      x1 = x2
      last = x2
      f1 = f2
    }
    cat(formatC( c(i,x2,f(x2),Error), digits = 8, width = -15, format = "f", flag = " "), "\n")
    i = i+1
  }
  cat("Cero de funcion: ", x2, ", error <=", abs(x2 - last), ", iteraciones: ", i)
  plot(Iteraciones,Errores, type = "l", xlab = "Iteraciones",ylab="Error")
  #Errores Ei vs Ei+1
  for(b in 1:i){
    if(b!=i){
      Errori[b]=Errores[b]
      Errorj[b]=Errores[b+1]  
    }
  }
  plot(Errori,Errorj, type = "l", xlab = "Error i",ylab="Error i+1")
}
f = function(x) exp(x)-pi*x;
FalsaPos(f,0,1,1000,1e-8)