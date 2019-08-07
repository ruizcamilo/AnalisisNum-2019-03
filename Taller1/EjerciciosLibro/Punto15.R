riemann <- function(f, a, b, n) {
  i = 1
  delta = (b-a)/n
  x = a + i*delta
  area = 0
  
  repeat
  {
    area <- area + f(x)*delta
    i = i + 1
    if (i == n) break;
    x = a + i*delta
  }
  return (area)
}

areaCurva <- function(fun, i, f, tol, rect, n)
{
  numSum=0
  sumatoria = 0
  x0 = riemann(fun, i, f, n)
  sumatoria =  sumatoria + x0
  cat ("area" , sumatoria, "\n")
  i = f
  f =  f + rect
  
  repeat
  {
    x1 = riemann(fun, i, f, n)
    numSum=numSum+1
    sumatoria = sumatoria + x1
    cat("Sumatoria # ", numSum, "\n")
    
    cat ("area" , sumatoria, "\n")
    error = abs(x1-x0)
    i = f
    f = f + rect
    x0 = x1
    
    if (sumatoria >  2 | error < tol)
    {
      return (sumatoria)
    }
    
  }
}
#Funcion integrada
f = function(x) -(exp(x))+5*x
cat("Area total: ",areaCurva(f, 0, 0.1, 0.00000001, 0.1, 10000))
