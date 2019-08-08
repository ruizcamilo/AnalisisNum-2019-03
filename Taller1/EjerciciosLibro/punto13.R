#Una formula iterativa de convergencia cuadratica podría ser la raíz
root <- function(value, n, x_0, err) {
  x_k = x_0
  deltax_k = 1
  it = 0
  while (abs(deltax_k) > err){
    deltax_k = ( ( value/(x_k**(n-1)) ) - x_k )/n
    x_k = x_k + deltax_k
    it = it + 1
  }
  return (x_k)
}
##Prueba
index = 4
num = 845
x_0 = 7
err = 1e-8
cat("Con los valores iniciales el resultado es:", raiz(num, index , x_0 , err))
