# DD Calcula derivadas de orden superior, recibe una expresion-
#Necesaria en Taylor
#Calcula la derivada de ornen n , de la variable name, donde la fcion es expr

DD <- function(expr, name, order = 1) {

    if(order < 1) stop("'order' must be >= 1") 
      if(order == 1) D(expr, name) 
      else DD(D(expr, name), name, order - 1)
  }
#Llamado a la fcion, donde halla la 1era derivada de e^x
DD(expression(exp(x)), "x", 1)

#---------------------------------------------
# Polinomio de Taylor de orden n, alrededor de x=a 

taylorT = function(f, x0, a, n){ # f es tira 
  # parse devuelve una expresión
  g = parse(text=f)
  # convertir en función 
  fx = function(x){eval(g[[1]])} 
  # almacenar los sumandos 
  smds = rep(NA, length=n+1) 
  for(k in 1:n){ 
    g. = DD(g,"x", k) 
    fp = function(x) eval(g.)
    smds[k]=1/factorial(k)*(x0-a)^k *fp(a)
  } 
  smds[n+1] = fx(a) 
  sum(smds)
}
#Taylor de orden 2 de e^x alrededor de 2
taylorT("exp(x)", 2.1, 2, 2)
