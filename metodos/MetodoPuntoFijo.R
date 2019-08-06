#metodo punto fijo
#Declaracion de funciones
puntofijo =function(g, x0, tol, maxIteraciones){
  k = 1
  # iteración hasta que abs(x1 - x0) <= tol o se alcance maxIteraciones
  repeat{
    x1 = g(x0)
    dx = abs(x1 - x0)
    x0 = x1
    #Imprimir estado
    cat("x_", k, "= ", x1, "\n")
    k = k+1
    #until
    if(dx< tol|| k > maxIteraciones) break;
  }
  # Mensaje de salida
  if( dx > tol ){
    cat("No hubo convergencia ")
    #return(NULL)
  } else{
    cat("x* es aproximadamente ", x1, " con error menor que ", tol)
  }
}
#Prueba
x0=1;  tol=1.e-8 ; N=100
f=function(x) exp(x)-(pi*x)
puntofijo(f,x0,tol,N)
