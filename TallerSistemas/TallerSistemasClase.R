---
  title: "Taller Sistemas de Ecuaciones - Camilo Ruiz - Alex Barreto - Sebastian Roberts"
output:
  html_notebook: default
pdf_document: default
---
  

require(pracma)
require(Matrix)
require(BB)
crearMatrix = function()
{
  datos = sample(1:20,36,replace=TRUE) 
  
  A = matrix(datos,nrow = 6,ncol = 6)
  
  while(1/rcond(A) < 1000)
  {
    datos = sample(1:20,36,replace=T) ## DAtos de la matrix aleatorios
    A = matrix(datos,nrow = 6,ncol = 6)
  }
  
  return(A)
}
#Matriz 1: Matriz 6x6, con numer de condición mayor a 1000
A<-crearMatrix()
A
b = matrix(c(1,2,3,4,5,6), nrow=6, byrow=TRUE)
L = tril(A,k=-1,diag = FALSE)
U = triu(A,k=1,diag = FALSE)
D = diag(diag(A))
I=diag(1,nrow = nrow(A)) # Matriz identidad de 3x3
D1 <- solve(D,I) # Matriz inversa de A
T1 = D1 %*% U
T2 = (I + (L %*% D1))
T2<- solve(T2,I) # Matriz inversa de A
MatTG = T1*T2
normaG = norm(MatTG, type = c( "I"))
print("Gauss")
itersolve(A,b,tol=1e-9,method="Gauss-Seidel")
print("Convergencia Gauss")
print(MatTG)
print(normaG)
MatTJ = (-D1)%*%(L+U)
normaJ = norm(MatTJ, type = c("I"))
print("Jacobi")
itersolve(A,b,tol=1e-4,method="Jacobi")
print("Convergencia Jacobi")
print(MatTJ)
print(normaJ)
