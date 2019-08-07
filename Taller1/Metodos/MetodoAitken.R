aitken = function(f, m, x0, tol)
{
  
  intIni = x0
  
  
  eje_x = c()
  eje_y = c()
  errores = c()
  k =0
  g<-parse(text=f)
  fx = function(x){eval(g[[1]])}
  d.<-D(parse(text=f ), "x")
  df<-function(x) eval(d.)
  
  cat( "\n", formatC ( c( "Iteracion     ", "Cero        ", "Error     "), width = 2, format = "d", flag = " "  ), "\n")
  
  repeat
  {
    k = k + 1
    x1 = x0 - m*(fx(x0)/df(x0))
    dx = abs(x1-x0)
    error = dx/x1
    errores[k] = error
    
    cat( formatC( c(k, x1, dx), digits = 7, width = -15, format = "f", flag = "  "  ), "\n" )
    
    if (dx < tol)
    {
      iter = c(1:k)
      cont_n = 0;
      cont_e = 0;
      repeat
      {
        eje_x[cont_n] = errores[cont_e]
        eje_y[cont_n] = errores[cont_e+1]
        cont_n = cont_n + 1
        cont_e = cont_e + 1;
        
        if (cont_n == k)
        {
          break;
        }
      }
      
      cat("\nRaiz: ", x1,
          " con valor inicial", intIni,
          ", multiplicidad", m ,
          ", Error <=",formatC(dx, digits = 7,  width = -15, format = "f", flag = "  "))
      
      #plot(eje_x, eje_y, main = "Convergencia Aitken", xlab= "Error Iteracion Actual", ylab = "Error Iteracion + 1", type = 'o' )
      plot(iter, errores, main = "Medicion del error Aitken", xlab= "Iteraciones", ylab = "Errores", type = 'o' )
      
      break;
    }
    x0 = x1
    
  }
}


limInf = 0
limSup = 2
limMitad = 1
tolerancia = 1e-8
f = function(x) ((exp(1)^x) - (pi*x))

aitken("2.7182^x-3.1415*x", 1, 2, tolerancia)

aitken("2.7182^x-3.1415*x", 1, 0, tolerancia)