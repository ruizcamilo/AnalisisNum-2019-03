---
title: "Taller Interpolación"
author: "Sebastian Roberts, Camilo Ruiz, Alex Barreto"
date: "06/09/2019"
output:
  html_document:
    df_print: paged
  pdf_document: default
---

#4. Con la función f(x) = lnx construya la interpolación de diferencias divididas en x0 = 1; x1 = 2 y estime el error en [1; 2]

```{r}
newtonInterpolacion = function(x, y, a) {
  n = length(x)
  A = matrix(rep(NA, times = n^2), nrow = n, ncol = n)
  A[,1] = y
  for (k in 2:n) {
    A[k:n, k] = (A[k:n, k-1] - A[(k-1):(n-1), k-1] ) / (x[k:n] - x[1:(n-k+1)])
  }
  # Imprimir matriz de diferencias divididas
  print(A)
  # Evaluar
  smds = rep(NA, length = n)
  smds[1] = 1 #x = x[1],..., x[n] pues n = length(x)
  for (k in 2:n) {
    smds[k] = (a - x[k-1])*smds[k-1] # hasta x[n-1]
  }
  return(sum(diag(A)*smds) )
}
arithmetic.mean <- function(x) {return(sum(x)/length(x))}
f = function(x) log(x)
x1 = c(1,2)
logaritmo = f(x1)
#Traza de la función Logaritmica
plot(x1,logaritmo,type="l", col="blue",xlab = "x", ylab = "f(x)", 
     main = "Ln e interpolación")
#Traza de la recta
curve(log,1,2,add = T, col="red")
x2 = seq(1,2,by=0.1)
interpolados = c()
for (i in x2) {
  interpolados = c(interpolados,newtonInterpolacion(x1,logaritmo,i));
}
# Tabla de Interpolados
print(interpolados)
reales = f(x2)
errores = c()
for (i in 1:length(reales)) {
  errores = c(errores,abs(reales[i]-interpolados[i]))  
}
# Tabla de errores
print(errores)
#Promedio de error 
prom = arithmetic.mean(errores)
print(prom)
