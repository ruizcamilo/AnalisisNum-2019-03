---
  title: "Taller Sistemas de Ecuaciones - Camilo Ruiz"
output:
  html_notebook: default
pdf_document: default
---
  
install.packages("pracma")
install.packages("BB")
install.packages("Matrix")
require(pracma)
require(Matrix)
require(BB)

##Ejercicios
A = matrix(c(-8.1, -7, 6.123, -2, -1, 4,
             -3, -1, 0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
#1. Para el siguiente ejercico, instale el paquete "pracma" 
#a. Revise las siguientes funciones con la matriz del ejercicio 2
#b. Evalue la matriz de transición para el método $\textbf{SOR}$
#Eye crea matrices de las medidas que se le envían como parametros
D1<-eye(4,4)
#Ones crea matrices de las medidas que se le envían como parametros de solo unos
D2<-ones(4,4)
#Zeros crea matrices de las medidas que se le envían como parametros de solo ceros
D3<-zeros(4,4)
D1
D2
D3
#Matriz de transicion: T 
#Diagonal D
#Parte triangular inferior a la matriz L
#Parte triangular superior a la matriz L
#T = -D(^-1)*(L + U)

diago <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

b = matrix(c(1.45,3,5.12,-4), nrow=4, byrow=TRUE)
D = diago(A)
L = tril(A,k=-1,diag = FALSE)
U = triu(A,k=1,diag = FALSE)

T = (-solve(D))%*%(L+U)
T

#----------------------------------------------------------
#2. Dada la siguiente matriz, utilice las funciones del paquete para descomponer la matriz $A=L+D+U$ (Jacobi)
#A
#Lu saca la matriz triangular inferior
#Ludec saca la matriz triangular superior
#Diag saca la diagonal
ti = lu(A)
L <- ti$L
L
U <-ludec$U
U
D <- diag(diag(A))
D
#Multiplicamos las dos triangulares
A = L %*% U
A
#b. Utilice la función itersolve(A, b, tol , method = "Gauss-Seidel")
# y solucionar el sistema asociado a la matriz $A$ con $b=[1.45,3,5.12,-4]^{t}$ con una tolerancia de $1e^-9$
b <- matrix(c(1.45, 3, 5.12, 4.0), nrow = 4, ncol = 1, byrow = TRUE)
tol = 1e-9
#Se podría usar Gauss-Seidel,Jacobi o Richardson
itersolve(A, b, tol , method = "Gauss-Seidel")
#c. Genere 5 iteraciones del método de Jacobi, calcular error relativo
#  para cada iteracion
jacobiPr <- function(A,b, x0, lim)
{
  x_k = matrix(x0)
  it = 0
  repeat
  {
    inn = matrix(b-((L+U)%*%x_k))
    D1 = (solve(D))
    xk1 = D1%*%inn
    cat("Error ",it," ",norm(xk1-x_k,"F")/norm(x_k),"\n")
    x_k = xk1
    it = it + 1
    if(it == lim)
      break
  }
  cat("Solucion a 5 iteraciones: ",x_k,"\n")
}
x0 = c(1,2,1,1)
jacobiPr(A, b, x0, 5)
#------------------------------------------------------------
#3. Sea el sistema $AX=b$ 
#a. Implemente una función en R para que evalue las raíces del polinomio característico asociado a la matriz $A$
#b. Use el teorema de convergencia para determinar cuál método iterativo es más favorable.
#c. Evalue la matriz de transición para cada caso y en el caso del método de relajación determine el valor óptimo de $\omega$
#d. Teniendo en cuenta lo anterio resolver el sistema
#A
A = matrix(c(5, -2, 8, 1,
             3.33 ,4, -1, 6, 
             6,12, -3, -1, 
             21, 5, 8, 4), nrow=4, byrow=TRUE)

#A
  polinomc<-charpoly(A,info = TRUE)
  #charpoly computa el polinomio caracteristico (lista de componentes cp)
  polinomc$cp
  cat("Las raices del polinomio caracteristico de A son:\n")
  roots(polinomc$cp)
#B
L = tril(A,k=-1,diag = FALSE)
U = triu(A,k=1,diag = FALSE)

L[lower.tri(L,diag=TRUE)] <- 0
U[upper.tri(U, diag = TRUE)] <- 0

D = diag(diag(A))
# Matriz identidad de 3x3
I=diag(1,nrow = nrow(A))
# inversa de A
D1 <- solve(D,I) 
T1 = D1 %*% U
T2 = (I + (L %*% D1))
# Matriz inversa de A
T2<- solve(T2,I) 
MatTG = T1+T2
normaG = norm(MatTG, type = c( "I"))
cat("Convergencia Gauss")
normaG
MatTJ = (-D1)%*%(L+U)
normaJ = norm(MatTJ, type = c("I"))
cat("Convergencia Jacobi")
normaJ

#C
cat("Matriz de trans. Gauss: ")
MatTG
cat("Matriz de trans. Jacobi: ")
MatTJ

#D
A = matrix(c(4, -1, -1, -1, -1, 4,
             -1, -1, -1, -1, 4, -1,
             -1, -1, -1, 4), nrow=4, byrow=TRUE)
b = c(1, 5, 1.5,-2.33)

X <- itersolve(A, b, method = "Jacobi")
print(X)
X <- itersolve(A, b, tol = 1e-9 , method = "Gauss-Seidel")
print(X)
solucion<- solve(A,b)
print(solucion)

#--------------------------------------------------------------------------
#3.a. Pruebe el siguiente algoritmo con una matriz
#$A_{3}$, modifiquelo para quue $a_{ii}=0$ para todo $i$
 
M = matrix(c(8,1,19,4,18,10,11,13,2),nrow=3)

tril1 <- function(M, k) {
  if (k == 0) {
    M[upper.tri(M, diag = TRUE)] <- 0
  } else {
    M[col(M) == row(M)] <- 0
  }
  return(M)
}
trilmej <- function(M, k) {
  if (k == 0) {
    M[upper.tri(M)] <- 0
  } else {
    M[col(M) >= row(M) + k + 1] <- 0
  }
  return(M)
}
cat("Al eliminar la diagonal:\n")
tril1(M,1)
trilmej(M,1)


#b. Implemente una función en R para que dada una matriz
# $A$ se obtenga una matriz diagonal $D$ donde en la diagonal
# estan los mismo elementos de A
diagonal <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}
cat("Matriz diagonal con elementos de M:\n")
diagonal(M)
  
#------------------------------------------------------------------------
#4. Cree una función que cuente el número de multiplicaciones en el 
#   método directo de Gauss Jordan, para resolver un sistema de $n$ ecuaciones y pruebelo para $n=5$
gauss = function(A, b)
{ 
  mult = 0
  n = nrow(A) # = ncol(A) para que sea cuadrada
  
  # matriz ampliada
  Ab = cbind(A,b)
  print(Ab)
  # Eliminación
  for (k in 1:(n-1)){ # desde columna k=1 hasta k=n-1
    if(Ab[k,k]==0){ # intercambio de fila
      fila = which(Ab[k, ]!=0)[1]
      Ab[c(k, fila), ] = Ab[c(fila, k), ]
    }
    
    # Eliminación columna k
    for (i in (k+1):n){# debajo de la diagonal
      # Fi = Fi - a_ik/a_kk * Fk, i=k+1,...,n
      Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
      mult = mult + 2*(ncol(Ab))
    }
  }
  
  # Sustitución hacia atrás-------------------------
  # b(i) = A[i, n+1]
  x = rep(NA, times=n)
  x[n] = Ab[n, n+1]/Ab[n,n] # xn = bn/a_nn
  mult = mult + n+1
  
  for(i in (n-1):1 ){
    x[i]= (Ab[i, n+1] - sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i]
    mult = mult + 2*(n-2)
  }
  #cat(x, "\n")
  cat("Numero de multiplicaciones:", mult, "\n")
  return(x)
}

A = matrix(c( 0, 2, 3, 3, 3,
              -5, -4, 1, 4, 5,
              0, 0, 0, 3, 7,
              -4, -7, -8, 9,7,
              3, 4, 5, 5, 6), nrow=5, byrow=TRUE)
b = matrix(c(1,0,0,0,1), nrow=5, byrow=TRUE)
gauss(A,b)

#------------------------------------------------------------------------
#5. Dado el siguiente sistema:
A = matrix(c(2, 0, 1,
             beta,2 , -1,
             -1, 1, alpha), nrow=3, byrow=TRUE)
B = matrix (c(1,2,1),nrow=3, byrow=TRUE)
#a. Encuentre el valor de $\alpha$ y $\beta$ para asegura la convergencia por el método de Jacobi
Ab = cbind(A,B)
print(Ab)
#b. Genere una tabla que tenga 10 iteraciones del método de Jacobi con vector inicial $x_{0}=[1,2,3]^t$
#c. Grafique cada ecuación y la solución
library("plot3D")
x = 0
y = 0
z = 0
diag1 <- function(M) {
  M[col(M)!=row(M)] <- 0
  return(M)
}

jacobiPr2 <- function(A,b, x0, tol){
  x_k = matrix(x0)
  D = diag1(A)
  L = tril(A,k=-1,diag = FALSE)
  U = triu(A,k=1,diag = FALSE)
  it = 1
  repeat
  {
    inn = matrix(b-((L+U)%*%x_k))
    D1 = (solve(D))
    xk1 = D1%*%inn
    cat("Error ",it," ",norm(xk1-x_k,"F")/norm(x_k),"\n")
    x_k = xk1
    x[[it]] = x_k[1]
    y[[it]] = x_k[2]
    z[[it]] = x_k[3]
    cat("Solucion iteracion ",it,": ",x[[it]]," ",y[[it]]," ",z[[it]],"\n")
    it = it + 1
    if(it == tol)
      break
  }
  lines3D(x, y, z, colvar = z, col = NULL, add = FALSE, theta = 20, phi = 20)
  cat("Solucion a ", tol ," iteraciones: ",x_k,"\n")
}
x1 = c(1,2,3)
jacobiPr2(A, B, x1, 10)

#------------------------------------------------------------------------
#6. Instalar el paquete Matrix y descomponga la matriz $A$ 
#  (del punto dos) de la forma $LU$ y la factorizarla como $A=QR$

  
  A = matrix(c(-8.1, -7, 6.123, -2, -1, 4,
               -3, -1, 0, -1, -5, 0.6,
               -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
  A
  descLU <- lu( A )
  lu_Desc <- expand(descLU)
  L <- lu_Desc$L
  U <- lu_Desc$U
  
  cat("Matriz L.")
  L
  cat("Matriz U.")
  U
  #Al multiplicar L y U se verifica
  cat("Matriz Original A. ")
  L %*% U
  
  #Factorizacion QR con Gram-Schmidt
  QR <- gramSchmidt(A)
  (Q <- QR$Q)
  (R <- QR$R)
  cat("Q:\n")
  Q
  cat("R:\n")
  R
  cat("Q*R:\n")
  Q %*% R 
  
#-----------------------------------------------------------------------
#7. a. Determinar numéricamente la intersección entre la circunferencia $x^2 + y^2 = 1$ y la recta $y = x$. Usamos una aproximación inicial $(1,1)$. Utilice el pauqte BB y  la función BBsolve() del paquete,grafique la solución
#   b Analizar y comentar el siguinte código
#A
  library(BB)
  sistema_no_lineal = function(x) {
    n = length(x)
    F = rep(NA, n)
    F[1] = x[1] - x[2]#x-y
    F[2] = x[1]^2 + x[2]^2 -1#x^2+y^2-1
    F
  }
  x0 = c(1,1) # n initial starting guess
  sol = BBsolve(par=x0, fn=sistema_no_lineal)
  sol$par
  
  plot(sol$par)
  plot(sistema_no_lineal)
#B
  trigexp = function(x) {
    
    #Tamaño del vector que llega por parámetro
    n = length(x)
    #se crea un vector F vacío
    F = rep(NA, n)
    
    #Se enuncian las ecuaciones del sistema
    F[1] = 3*x[1]^2 + 2*x[2] - 5 + sin(x[1] - x[2]) * sin(x[1] + x[2])
    #Se crea una secuencia de 2 hasta n-1
    tn1 = 2:(n-1)
    #Se evalúan tn1 ecuaciones
    F[tn1] = -x[tn1-1] * exp(x[tn1-1] - x[tn1]) + x[tn1] *
      ( 4 + 3*x[tn1]^2) + 2 * x[tn1 + 1] + sin(x[tn1] -
                                                 x[tn1 + 1]) * sin(x[tn1] + x[tn1 + 1]) - 8
    #Se evalúa la última ecuación n
    F[n] = -x[n-1] * exp(x[n-1] - x[n]) + 4*x[n] - 3
    #Se retorna F
    F
  }
  n = 10000
  p0 = runif(n) 
  #RUNIF es una funcion que genera n numeros aleatorios entre 0 y 1
  #se halla la solcuión del sistema trigexp usando BBsolve de la librería BB, utilizando n valores iniciales
  sol = BBsolve(par=p0, fn=trigexp)
  #Muestra el vector solución del sistema para cada n valores iniciales
  sol$par
  
#----------------------------------------------------------------------
#8. Demuestre y realice varias pruebas que la matriz 
#   de transición por el método de Gauss-Seidel esta dada por $T=(-D^{-1}U)(I+LD^{-1})^{-1}$
  N <- 3
  A = matrix(c(-8.1, -7, 6.123, -2,
               -1, 4,-3, -1,
               0, -1, -5, 0.6,
               -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
  A
  x0 <- rep(0, N)
  b = c(4,5,6,8)
  itersolve(A, b, tol=1e-9 , method = "Gauss-Seidel")
  
  L = tril(A,k=-1,diag = FALSE)
  U = triu(A,k=1,diag = FALSE)
  L[lower.tri(L,diag=TRUE)] <- 0
  U[upper.tri(U, diag = TRUE)] <- 0
  #print (A)
  D = diag(diag(A))
  I=diag(1,nrow = nrow(A)) # Matriz identidad de 3x3
  D1 <- solve(D,I) # Matriz inversa de A
  T1 = D1 %*% U
  T2 = (I + (L %*% D1))
  T2<- solve(T2,I) # Matriz inversa de A
  MatTG = T1+T2
  MatTG
  T3 = -solve(D)
  T4 = T3%*%U
  T5= solve(D)
  T6 = L%*%T5
  T7 = I + T6
  T8 = solve(T7)
  T = T8+T4#T4%*%T8
  T