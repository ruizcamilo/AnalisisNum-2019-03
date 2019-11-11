require(PolynomF)
x=c(1,2,5,6,7.5,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30)
y=c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)
plot(x,y, pch=19, cex=0.5, col = "red", asp=1) 
i= 1
min = i
max = i+1
bool = 0
cont = 1
repeat
{
  m = (y[i+1]-y[i])/(x[i+1]-x[i])
  cont = cont +1
  #cat("bool", bool, "m", m,"i", i, "j", i+1, "cont", cont-1,"<>0\n")
  if (i == 1 && m > 0)
  {
    bool = 1
  }
  else if(i == 1 && m < 0)
  {
    bool = 0
  }
  if (m < 0)
  {
    if (bool == 0)
    {
      j = i + 1
      max= j
      i = j
    }
    else 
    {
      datx = x[min:max]; daty = y[min:max]
      polyAjuste = poly_calc(datx, daty)
      cat("bool", bool, "min", min, "max", max, "cont", cont-1,"<0\n")
      curve(polyAjuste,from=x[min],to=x[max],add=T, lwd=1,col="blue")
      min = max
      cont = 1
      bool = 0
      i = max
      j = i + 1
      max= j
    }
  }
  else
  {
    if (bool == 0)
    {
      datx = x[min:max]; daty = y[min:max]
      polyAjuste = poly_calc(datx, daty)
      cat("bool", bool, "min", min, "max", max, "cont", cont-1,">0\n")
      curve(polyAjuste,from=x[min],to=x[max],add=T, lwd=1,col="blue")
      min = max
      cont = 1
      bool = 1
      i = max
      j = i + 1
      max= j
    }
    else
    {
      j = i + 1
      max= j
      i = j
    }
  }
  if (cont == 3)
  {
    datx = x[min:max]; daty = y[min:max]
    polyAjuste = poly_calc(datx, daty)
    cat("bool", bool, "min", min, "max", max, "cont", cont-1,"<>0\n")
    curve(polyAjuste,from=x[min],to=x[max],add=T, lwd=1,col="blue")
    min = max
    cont = 1
    i = max
    j = i + 1
    max= j 
    
    if(m < 0)
    {
      bool = 0 
    }
    else
    {
      bool = 1
    }
  }
  
  if (i==length(x))
  {
    max = i
    datx = x[min:max]; daty = y[min:max]
    polyAjuste = poly_calc(datx, daty)
    cat("bool", bool, "min", min, "max", max, "cont", cont-1,"<>0\n")
    curve(polyAjuste,from=x[min],to=x[max],add=T, lwd=1,col="blue")
    break;
  }
}
