#m = cantidad de digitos decimales
#n = cantidad de enteros normalizados en potencia de 10
error <- function (num, m, n){
  numero<- abs(num)
  
  while(num>1){
    num<-num/10
    n=n+1
  }
  E<-(num-trunc(num*10^4)/10^4)
  izq<-1*10^(n-m)
  der<-1*10^(n-m)
  cat("E se encuentra en el intervalo: ",izq,"<",E,"<",der)
}
error(536.78,4, 0)