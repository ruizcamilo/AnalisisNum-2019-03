library(Matrix)
library(PolynomF)
#interpolacion
##Puntos 
x=c(14.6, 14.7, 14.6, 14.8, 15.2, 15.6, 15.7, 17.0, 17.6, 17.5, 17.3, 16.8, 15.4, 14.8, 14.4, 14.5, 15.0, 15.1, 15.0, 14.9, 14.6, 14.3, 14.0, 13.9, 13.8, 13.5, 13.1, 13.0, 13.3, 13.2, 13.1, 12.9, 12.4, 11.9, 11.7, 11.6, 11.3, 10.9, 10.7, 10.6, 10.1, 9.7, 9.4, 9.3, 9.6, 9.9, 10.1, 10.2, 10.3,10, 9.5, 9.10 ,8.6, 7.5, 7.0, 6.7, 6.6, 7.70, 8.00, 8.10, 8.40,              9.00, 9.30, 10, 10.2, 10.3)                                                                                                       
y=c(14.7, 14.0, 13.4, 12.3, 11.0, 10.5, 10.2, 8.20, 7.10, 6.70, 6.60, 6.80, 8.30, 8.80, 9.30, 8.80, 6.30, 5.50, 5.00, 4.70, 4.60, 4.50, 4.90, 5.40, 5.80, 6.90, 8.20, 7.60, 5.80, 4.50, 4.30, 3.90, 4.20, 5.70, 7.00, 7.90, 8.20, 7.30, 6.70, 5.10, 4.60, 4.7, 5.0, 5.5, 7.2, 7.8, 8.60, 9.40, 10.0,10.7, 11, 10.7, 9.9, 9.0, 9.1, 9.3, 9.7, 11.7, 12.3, 12.5, 13.0,              13.9, 14.9, 16, 16.4, 16.8)
vectorm = c()
#z = seq(1,65, by = 2)
#x = x[-z]
#y = y[-z]
plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama")

i = 1
repeat
{
  m = (y[i+1]-y[i])/(x[i+1]-x[i])
  vectorm[i] = m
  i = i + 1
  if (length(x) == i)
    break;
}

i = 1
repeat
{
  if (abs(vectorm[i]-vectorm[i+1]) > -0.01) 
  {
    borrar = i 
    x = x[-borrar]
    y = y[-borrar]
  }
  i = i + 1
  if(i == length(vectorm))
  {
    break;
  }
}

length(x)
plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama")

i= 1
min = 1
max = i+1
bool = 0
cont = 1
repeat
{
  m = (y[i+1]-y[i])/(x[i+1]-x[i])
  cont = cont +1
  cat("bool", bool, "m", m,"i", i, "j", i+1, "cont", cont-1,"<>0\n")
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
  if (cont == 2)
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
  
  if (i==ceiling(length(x)/2)-1)
  {
    datx = x[min:max]; daty = y[min:max]
    polyAjuste = poly_calc(datx, daty)
    cat("x", i, "y", j, "min", min, "max", max, "cont", cont,"<>0\n")
    curve(polyAjuste,from=x[min],to=x[max],add=T, lwd=1,col="blue")
    break;
  }
}

i= ceiling(length(x)/2)
min = i
max = i+1
bool = 0
cont = 1
repeat
{
  m = (y[i+1]-y[i])/(x[i+1]-x[i])
  cont = cont +1
  cat("bool", bool, "m", m,"i", i, "j", i+1, "cont", cont-1,"<>0\n")
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
  if (cont == 2)
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
  
  if (i==ceiling(length(x)))
  {
    break;
  }
}
