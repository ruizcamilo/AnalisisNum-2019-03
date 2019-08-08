polar <- function (theta, r, color=4){
  y <- 0
  x <- 0
  ejex <- 1
  
  for (i in 1:length(r)){
    if(is.nan(r[i])== T){
      r[i] <- 0
    }
  }
  
  dim <- seq(0, 2*pi, by=pi/300) 
  angulo <- seq(-max(dim),max(dim),by=dim[2]-dim[1])
  y <- r*sin(theta)
  x <- r*cos(theta)
  plot.new()
  plot.window(xlim = c(-max(r), max(r)), ylim = c(-max(r), max(r)), asp = 1)
  
  aux <- max(r)
  # Dibuja los ejes.
  while (aux > 0){
    fi <- aux*sin(angulo)
    cir <- aux*cos(angulo)
    points(cir,fi,pch="-",col="gray",cex=0.3)
    text(ejex+0.2,-0.2,ejex,col="gray")
    ejex <- ejex + 1
    aux <- aux - 1
  }
  
  abline(v=((max(cir)+min(cir))/2),col="gray")
  abline(h=((max(cir)+min(cir))/2),col="gray")
  segments(-max(r)+0.5,-max(r)+0.5,max(r)-0.5,max(r)-0.5,col="gray")
  segments(-max(r)+0.5,max(r)-0.5,max(r)-0.5,-max(r)+0.5,col="gray")
  
  points(x,y,pch=20,col=color,cex=1)
  
}

f<-function(x) 3*sin(x)^3-1-4*sin(x)*cos(x)
fp<-function(x) 9*cos(x)*sin(x)^2+4*sin(x)^2-4*cos(x)^2

newtonP = function(f, fp, x0, tol, maxiter)
{
  dim <- seq(0, 2*pi, by=pi/300) 
  r=3*sin(dim)^3-1
  r2=4*sin(dim)*cos(dim)
  polar(dim,r,"green")
  par(new=TRUE)
  polar(dim,r2,"red")
  title(main="Gráficas de las Funciones Polares (Originales)")
  
  points(0.5,1.29, col = "green", pch = 20)
  
  dim <- seq(0, pi/2, by=pi/300) 
  r=3*sin(dim)^3-1
  r2=4*sin(dim)*cos(dim)
  polar(dim,r,"green")
  par(new=TRUE)
  polar(dim,r2,"red")
  title(main="Gráficas de las Funciones Polares en intervalo theta = [0, pi/2]")
  
  x<-c()
  y<-c()
  
  k = 0
  dx = 0
  # Imprimir estado
  cat("---------------------------------------------------------------------------\n")
  cat(formatC( c("x_k"," f(x_k)","Error est."), width = -20, format = "f", flag = " "), "\n")
  cat("---------------------------------------------------------------------------\n")
  repeat{
    correccion = f(x0)/fp(x0)
    x1 = x0 - correccion
    dy = dx
    dx = abs(x1-x0)
    
    if(k>=1)
    {
      x<-c(x,dy/x0)
      y<-c(y, dx/x0)
      
    }
    
    
    
    # Imprimir iteraciones
    cat(formatC( c(x1 ,f(x1), dx/x1), digits=15, width = -15, format = "f", flag = " "), "\n")
    k = k+1
    # until
    if(dx <= tol || k > maxiter )
    {
      break;
    } 
    x0 = x1
  }
  cat("---------------------------------------------------------------------------\n")
  if(k > maxiter){
    cat("Se alcanzo el maximo numero de iteraciones.\n")
    cat("k = ", k, "Estado: x = ", x1, "Error estimado <= ", y[k-1])
  } else {
    cat("k = ", k, " x = ", x1, " f(x) = ", f(x1), " Error estimado <= ", y[k-1]) }
  
  points(0.5,1.29, col = "green", pch = 20)
  
  plot(f, xlim = c(0,x[1]), ylim = c(0,y[1]), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Newton", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(x, y, type = "l")
  
  x<-c(x,y[k-1])
  iteraciones<-c(1:k)
  plot(f, xlim = c(0,iteraciones[k]), ylim = c(0,x[1]), col = "white", main = "Iteraciones vs Errores", sub = "Newton", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, x, type = "l")
}

newtonP(f, fp, pi/2, 1e-5, 100)

