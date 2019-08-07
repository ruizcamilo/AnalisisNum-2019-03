    
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
#metodo bisección
# la funcion que aplica el metodo de biseccion
biseccion = function(f, xa, xb, tol){
  if( sign(f(xa)) == sign(f(xb)) ){ stop("f(xa) y f(xb) tienen el mismo signo") }
  # a = min(xa,xb)
  # b = max(xa,xb)
  a = xa; b = xb
  k = 0
  #Par imprimir estado
  cat("----------------------------------------------------------\n")
  cat(formatC( c("a","b","m","Error est."), width = -15, format = "f", flag = " "), "\n")
  cat("----------------------------------------------------------\n")
  repeat{
    m = a + 0.5*(b-a)
    if( f(m)==0 ){ cat("Cero de f en [",xa,",",xb,"] es: ", m ) }
    if( sign(f(a)) != sign(f(m)) ){
      b = m
    } else { a = m }
    dx = (b-a)/2
    # imprimir estado
    cat(formatC( c(a,b,m,dx), digits=7, width = -15, format = "f", flag = " "), "\n")
    k = k+1
    #until
    if( dx < tol ){
      cat("----------------------------------------------------------\n\n")
      cat("Cero de f en [",xa,",",xb,"] es approx: ", m, "con error <=", dx)
      break;
    }
  } #repeat
}
#Metodo de newton
newton1 = function(f, fp, x0, tol, maxiter){
  k = 0
  # Imprimir estado
  cat("---------------------------------------------------------------------------\n")
  cat(formatC( c("x_k"," f(x_k)","Error est."), width = -20, format = "f", flag = " "), "\n")
  cat("---------------------------------------------------------------------------\n")
  repeat{
    correccion = f(x0)/fp(x0)
    x1 = x0 - correccion
    dx = abs(x1-x0)
    # Imprimir iteraciones
    cat(formatC( c(x1 ,f(x1), dx), digits=15, width = -15, format = "f", flag = " "), "\n")
    x0 = x1
    k = k+1
    # until
    if(dx <= tol || k > maxiter ) break;
  }
  cat("---------------------------------------------------------------------------\n")
  if(k > maxiter){
    cat("Se alcanzó el máximo número de iteraciones.\n")
    cat("k = ", k, "Estado: x = ", x1, "Error estimado <= ", correccion)
  } else {
    cat("k = ", k, " x = ", x1, " f(x) = ", f(x1), " Error estimado <= ", correccion) }
}
# MAIN----------------------------------------------------------------------------
x0=1;  tol=1.e-8 ; N=100
v_a = 0 ; v_b = 3
f=function(x) exp(x)-(pi*x) 
g=function(x) exp(x)
h=function(x) pi*x
plot(h,type="l",col="red")
plot(g,add=TRUE)

puntofijo(f,x0,tol,N)
fp = function(x) exp(x)-pi
newton1(f,fp, x0, tol, N)
biseccion(f, v_a, v_b, tol)
