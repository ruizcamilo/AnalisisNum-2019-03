#Una formula iterativa de convergencia cuadratica podría ser la raíz
root <- function(value, n, x_0, err) {
  x = x_0
  deltax_k = 1
  it = 0
  while (abs(deltax_k) > err){
    deltax = ( ( value/(x**(n-1)) ) - x )/n
    x = x + deltax_k
    it = it + 1
  }
  return (x)
}
##Prueba
index = 4
num = 845
x_0 = 7
err = 1e-8
cat("Con los valores iniciales el resultado es:", raiz(num, index , x_0 , err))
