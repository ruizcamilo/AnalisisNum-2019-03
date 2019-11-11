oneLagrange_pol <- function(dataX, index, x)
{
  #dataX (vector-double): puntos donde se conoce la funcion
  #index (int): numero que indica que termino estamos calculando
  #x (double): punto en el que se evalua el polinomio
  L_i <- 1.                   #inicializamos el coeficiente del i-esimo termino
  for(i in 1:length(dataX))
  {
    if(i != index)
    {
      #evaluamos el coeficiente de 
      L_i <- L_i*( (x - dataX[i] )/(dataX[index] - dataX[i]) )
    }
  }
  return(L_i)  #regresamos el coeficiente index-esimo 
}
Eval_pLagrange <- function(dataX, dataY, x)
{
  #dataX (vector-double): puntos donde se conoce la funcion
  #dataY (vector-double): puntos donde se conoce el valor de la funcion
  #x (double): punto en el que se evalua el polinomio
  f_aprox <- 0.         #inicializamos el polinomio
  for(i in 1:length(dataX))
  {
    #calculamos iterativamente el polinomio de Laprange
    f_aprox <- f_aprox + dataY[i]*oneLagrange_pol(dataX, i, x)
  }
  return(f_aprox)  #regresamos el valor del polinomio en el punto
}  
Lagrange <- function(dataX, dataY, m, a, b )
{
  #dataX (vector): puntos a evaluar donde se conoce la funcion
  #dataY (vector): valor de la funcion conocida en los puntos dataX
  #m (int):       numero de valores a evaluar
  #a,b (double): limite inferior y superior del dominio a interpolar
  
  soporte <- seq(a, b, length = m)   #construimos puntos para probar la                                         
  f_soporte <- soporte*0             #reservamos memoria para guardar los                                                               
  #cat(f_soporte)
  #cat(soporte)
  for(i in 1:length(soporte))
  {
    #para cada punto en el soporte se evalua el polinomio
    f_soporte[i] <- Eval_pLagrange(dataX, dataY , soporte[i]  )
    
  }
  return(f_soporte)      #regresamos el vector con los valores interpolados
}

datax = c(6, 8, 10, 12, 14, 16, 18)
datay = c(7, 9, 12, 18, 21, 19, 15, 10)
resultado = Lagrange(datax, datay, 7, 6, 18)
resultado
x = Eval_pLagrange(datax, datay, 13)
x
