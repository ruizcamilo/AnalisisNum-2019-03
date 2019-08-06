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
#Prueba
x0=1;  tol=1.e-8 ; N=100
v_a = 0 ; v_b = 3
f=function(x) exp(x)-(pi*x) 
biseccion(f, v_a, v_b, tol)
