Horner<- function (func, g, x0){
  res<-func[1]
  n<-0
  for(i in 2:(g+1)){
    res<- res*x0 + func[i]
    n<-n+2
  }
  cat("El resultado del polinomio es: ", res, " en ",n,"operaciones")
}
func<-c(2,0,-3,3,-4)
Horner(func,4,-2)