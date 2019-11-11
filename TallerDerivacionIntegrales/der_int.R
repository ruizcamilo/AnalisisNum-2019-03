#----------------
#     a y b
#----------------

f = function(x)
{
  x*cos(x)
}

h = c(0.1, 0.01, 0.001, 0.0001)

derivada = function(i, x0, h)
{
  n = length(h)
  m = 1
  
  original = D(parse(text="x*cos(x)"), "x")
  fp = function(x) eval(original)
  fp(x0)
  
  cat("h \t D \t Error \n")
  repeat
  {
    ip= (i(x0+h[m])-i(x0))/(h[m])
    E = abs(ip-fp(1.8))
    cat(h[m], "\t", ip, "\t", E, "\n")
    m=m+1
    if(m>n)
    {
      break;
    }
  }
}

derivada(f, 1.8, h)

#----------------
#       c
#----------------

derivada2 = function(i, x0, h)
{
  n = length(h)
  m = 1
  
  original = D(parse(text="x*cos(x)"), "x")
  fp = function(x) eval(original)
  fp(x0)
  ho = 0
  val = 0 ; 
  repeat
  {
    ip= (i(x0+h[m])-i(x0))/(h[m])
    E = abs(ip-fp(1.8))
    cat(h[m], "\t", ip, "\t", E, "\n")
    m=m+1
    val = E*10^3
    if(val >= 1 && val < 10)
    {
      ho = h[m]
    }
    if(m>n)
    {
      break;
    }
    
  }
  cat("\n El valor del h que proporciona una precisió de 10^-4 es: ", ho)
  
}
derivada2(f, 1.8, h)

##d(3 puntos)
i = 0
f = function(x) x*cos(x)
g = "x*cos(x)"
xo = 1.8
h = 0.1
g = parse(text = g) # parse devuelve tipo "expression"
g. = D(g,"x") 
fp = function(x){eval(g.)} # convertir f' a función
original = fp(1.8)

repeat
{
  d = (-f(xo+2*h)+4*f(xo+h)-3*f(xo))/(2*h)
  h = h /10
  i = i + 1
  e = abs(original - d)
  
  if(i == 5)
  {
    break;
  }
  cat("La derivada es ", d, " El error de truncamiento es ", e, "\n")
}

##e(3 puntos modificada)
i = 0
f = function(x) x*cos(x)
g = "x*cos(x)"
xo = 1.8
h = 0.1
g = parse(text = g) # parse devuelve tipo "expression"
g. = D(g,"x") 
fp = function(x){eval(g.)} # convertir f' a función
original = fp(1.8)

repeat
{
  d = (-f(xo+h)+4*f(xo)-3*f(xo-h))/(2*h)
  h = h /10
  i = i + 1
  e = abs(original - d)

  if(i == 5)
  {
    break;
  }
  
  cat("La derivada es ", d, " El error de truncamiento es ", e, "\n")
}

##f(5 puntos) comparar con los anteriores
i = 0
f = function(x) x*cos(x)
g = "x*cos(x)"
xo = 1.8
h = 0.1
g = parse(text = g) # parse devuelve tipo "expression"
g. = D(g,"x") 
fp = function(x){eval(g.)} # convertir f' a función
original = fp(1.8)

repeat
{
  d = (f(xo-2*h)-8*f(xo-h)+8*f(xo+h)-f(xo+2*h))/(12*h)
  h = h /10
  i = i + 1
  e = abs(original - d)
  
  if(i == 5)
  {
    break;
  }
  
  cat("La derivada es ", d, " El error de truncamiento es ", e, "\n")
}

##g justificacion es que o(h^2)
i = 0
f = function(x) x*cos(x)
g = "cos(x)-x*sin(x)"
xo = 1.8
h = 0.1
g = parse(text = g) # parse devuelve tipo "expression"
g. = D(g,"x") 
fp = function(x){eval(g.)} # convertir f' a función
original = fp(1.8)

repeat
{
  d = (-f(xo+3*h)+4*f(xo+2*h)-5*f(xo+h)+2*f(xo))/(h^2)
  h = h /10
  i = i + 1
  e = abs(original - d)
  
  if(i == 5)
  {
    break;
  }
  
  cat("La derivada es ", d, " El error de truncamiento es ", e, "\n")
}

##i
f = function(x) x*exp(x)
r = 5.436563656918091
h = 0.1
i = 0
x = c()
y = c()

repeat
{
  d =(f(1+h)-f(1))/h
  e = abs(r-d)
  h = h / 10
  i = i + 1
  x[i] = h
  y[i] = e
  if(i == 16)
  {
    break;
  }
}

plot(x,y)#poner lineas 

##j
buscar = function(I, t, buscado, tam)
{
  i = 1
  
  repeat
  {
    
    
    if(t[i] == buscado)
    {
      return(I[i])
    }
    
    i = i + 1
    
    if(i == tam + 1)
    {
      break;
    }
  }
}


f = function(I, t, h, x0, tot, R, L)
{
  i = 1
  
  repeat
  {
  
    d = (buscar(I, t, xo+h, 5)-buscar(I, t, xo+h-0.01, 5))/(0.01)
    h = h + 0.01
    i = i + 1
    
    if(i == tot + 1)
    {
      break;
    }
    
    cat("La derivada es ", d, " El voltaje en ", t[i-1], " es", L*d+R*I[i-1],"\n")
  }
}

t = c(1.00,1.01,1.02,1.03,1.04)
I = c(3.10,3.12,3.14,3.18,3.24)
h = 0.01
xo = 1
R = 0.142
L = 0.98
tot = 5
f(I, t, h, xo, tot, R, L)
