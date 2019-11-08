
library(deSolve)
library(shiny)

#Interfaz de Usuario
#-----------
ui <- fluidPage(
  
  # Application title
  titlePanel("               Modelo de propagacion de un virus informatico"),
  
  sidebarLayout
  (
    sidebarPanel
    (
      selectInput("modelo",
                  "Seleccione un modelo epidemologico:",
                  choices = c("SI", "SIR"), selected = "SI"),
      selectInput("tipoEcuacion", "Metodo de Solucion:"
                  , choices = c("EDO", "EDF"), selected = "EDO" ),
      
      conditionalPanel
      (
        condition = "input.modelo == 'SI' && input.tipoEcuacion == 'EDO'",
        radioButtons(inputId = "metodoSI", "Seleccione:", c("Euler", "Runge Kutta "))
      ),
      conditionalPanel
      (
        condition = "input.modelo == 'SI'&& input.tipoEcuacion != 'seleccionar'",
        numericInput(inputId = "numPoblacion", label = "Poblacion:", value = 10),     
        numericInput(inputId = "numInfec", label = "Infectados (I):", value = 1),
        numericInput(inputId = "beta", label = "Tiempo De llegada (Beta):", value = 0.2275)
      ),
      conditionalPanel
      (
        condition = "input.modelo == 'SIR'&& input.tipoEcuacion != 'seleccionar'",
        numericInput(inputId = "numPoblacion", label = "Poblacion (S)::", value = 10),       
        numericInput(inputId = "numInfec", label = "Infectados (I):", value = 1),
        numericInput(inputId = "numRecuper", label = "Recuperados (R) :", value = 0),
        numericInput(inputId = "beta", label = "Tiempo promedio de llegada del virus:", value = 0,25),
        numericInput(inputId = "gama", label = "Tasa de recuperacion:", value = 0.1)
      ) 
    ),
    mainPanel
    (
      plotOutput("model")
    )
  )
)
server = function(input, output)
{
  output$model <- renderPlot({
    
    if(input$modelo == "SI" && input$metodoSI == "Euler" && input$tipoEcuacion == "EDO")
    {
      init <- c(S = (input$numPoblacion-input$numInfec), I = input$numInfec, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$beta, gama = 0)
      init = init/N
      N = 1
      
      SI_EULER_EDO(init, param, N)
    }
    else if(input$modelo == "SIR" && input$tipoEcuacion == "EDO")
    {
      init <- c(S = (input$numPoblacion-input$numInfec), I = input$numInfec, R = input$numRecuper)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$beta, gama = input$gama)
      init = init/N
      N = 1
      
      SIR_EDO(init, param, N)
    }
    else if(input$modelo == "SI" && input$metodoSI == "Runge Kutta" && input$tipoEcuacion == "EDO")
    {
      init <- c(S = (input$numPoblacion-input$numInfec), I = input$numInfec, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$beta, gama = 0)
      init = init/N
      N = 1
      
      SI_EDO_RUNGE(init, param, N)
    }
    else if(input$modelo == "SI" && input$tipoEcuacion == "EDF")
    {
      init <- c(S = (input$numPoblacion-input$numInfec), I = input$numInfec, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$beta, gama = 0)
      init = init/N
      N = 1
      
      SI_EDF(init, param, N)
    }
    else if(input$modelo == "SIR" && input$tipoEcuacion == "EDF")
    {
      init <- c(S = (input$numPoblacion-input$numInfec), I = input$numInfec, R = input$numRecuper)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$beta, gama = input$gama)
      init = init/N
      N = 1
      
      SIR_EDF(init, param, N)
    }
  })
}

#Fcion qie grafica SI por EDO (Euler)
SI_EULER_EDO = function(init, param, N)
{
  
  #crear la funciÃÂ³n con las EDO
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <- beta * S * I - gama * I
           dR <- gama * I
           
           #.    
           return(list(c(dS, dI, dR)))
         })
    
  }
  
  #intervalo de tiempo y resoluciÃÂ³n
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funciÃÂ³n 'EDO'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
  #mostrar 10 primeros datos
  head(out, 10)
  #grÃÂ¡fica
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I", main = "SI",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #aÃÂ±adir leyenda de lÃ?neas
  legend(40, 0.7, c("Susceptibles", "Infectados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
}

#Fcion qie grafica SIR 
SIR_EDO = function(init, param, N)
{
  #crear la funciÃÂ³n con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <-  beta * S * I - gama * I
           dR <- gama * I
           #.    
           return(list(c(dS, dI, dR)))
         })
  }
  
  #intervalo de tiempo y resoluciÃÂ³n
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funciÃÂ³n 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #grÃÂ¡fica
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I.R", main = "Modelo SIR ",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #aÃÂ±adir leyenda de lÃ?neas
  legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
}

#Fcion qie grafica SI por EDO (Runge Kutta)
SI_EDO_RUNGE = function(init, param, N)
{
  #crear la funciÃÂ³n con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <- beta * S * I - gama * I
           dR <- gama * I
           
           #.    
           return(list(c(dS, dI, dR)))
         })
  }
  #intervalo de tiempo y resoluciÃÂ³n
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funciÃÂ³n 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "rk4")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #grÃÂ¡fica
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I", main = "Modelo SI bÃÂ¡sico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #aÃÂ±adir leyenda de lÃ?neas
  legend(40, 0.7, c("Susceptibles", "Infectados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
}
#SI-EDF
SI_EDF = function(init, param, N)
{
  #crear la funciÃÂ³n con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <- beta * S * I - gama * I
           dR <- gama * I
           
           #.    
           return(list(c(dS, dI, dR)))
         })
  }
  
  #intervalo de tiempo y resoluciÃÂ³n
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funciÃÂ³n 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #S
  beta = param[1]
  infectados = out$I
  A = matrix(rep(0, times = 70^2), nrow = 70, ncol = 70)
  i = 1
  t = 1
  
  A[i,1] = t*beta*infectados[t+1]
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = t*beta*infectados[t+1]
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==70)
    {
      break;
    }
  }
  
  A[70,70] = 1
  b = c(rep(0,70))
  b[1] = init[1]
  b[70] = out$S[71]
  
  ysus = solve(A, b)
  num1 = ysus[64]
  num2 = 4.207208e-05
  num1
  num2
  abs(num1-num2)/num1
  #plot(c(1:70),ysus)
  
  #I
  beta = param[1]
  alfa = param[2]
  susceptile = out$S
  A = matrix(rep(0, times = 70^2), nrow = 70, ncol = 70)
  i = 1
  t = 1
  
  A[i,1] = -t*beta*susceptile[t+1]+t*alfa
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = -t*beta*susceptile[t+1]+t*alfa
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==70)
    {
      break;
    }
  }
  
  A[70,70] = 1
  b = c(rep(0,70))
  b[1] = init[2]
  b[70] = out$I[71]
  
  yinfec = solve(A, b)
  num1 = yinfec[66]
  num2 = 1.067680e-04
  num1
  num2
  abs(num1-num2)/num1
  #plot(c(1:70),yinfec)
  yinfec
  
  #-------------------------------
  
  out = matrix(rep(0, times = 70*2), nrow = 70, ncol = 2)
  out
  times = times[-1]
  out[,1] = ysus
  out[,2] = yinfec
  matplot(x = times, y = out, type = "l",
          xlab = "Tiempo", ylab = "S, I", main = "Modelo SI bÃ¡sico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  
  legend(40, 0.7, c("Susceptibles", "Infectados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
}

#SIR-EDF
SIR_EDF = function(init, param, N)
{
  #crear la funciÃÂ³n con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <-  beta * S * I - gama * I
           dR <- gama * I
           #.    
           return(list(c(dS, dI, dR)))
         })
  }
  
  #intervalo de tiempo y resoluciÃÂ³n
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funciÃÂ³n 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param)
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  #S
  beta = param[1]
  infectados = out$I
  A = matrix(rep(0, times = 70^2), nrow = 70, ncol = 70)
  i = 1
  t = 1
  
  A[i,1] = t*beta*infectados[t+1]
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = t*beta*infectados[t+1]
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==70)
    {
      break;
    }
  }
  
  A[70,70] = 1
  b = c(rep(0,70))
  b[1] = init[1]
  b[70] = out$S[71]
  
  ysus = solve(A, b)
  num1 = ysus[64]
  num2 = 4.207208e-05
  num1
  num2
  abs(num1-num2)/num1
  #plot(c(1:70),ysus)
  
  #I
  beta = param[1]
  alfa = param[2]
  susceptile = out$S
  A = matrix(rep(0, times = 70^2), nrow = 70, ncol = 70)
  i = 1
  t = 1
  
  A[i,1] = -t*beta*susceptile[t+1]+t*alfa
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = -t*beta*susceptile[t+1]+t*alfa
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==70)
    {
      break;
    }
  }
  
  A[70,70] = 1
  b = c(rep(0,70))
  b[1] = init[2]
  b[70] = out$I[71]
  
  yinfec = solve(A, b)
  num1 = yinfec[66]
  num2 = 1.067680e-04
  num1
  num2
  abs(num1-num2)/num1
  #plot(c(1:70),yinfec)
  yinfec
  
  #R
  beta = param[1]
  alfa = param[2]
  infectados = out$I
  A = matrix(rep(0, times = 70^2), nrow = 70, ncol = 70)
  i = 1
  t = 1
  
  A[i,1] = -t*alfa*infectados[t+1]
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = -t*alfa*infectados[t+1]
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==70)
    {
      break;
    }
  }
  
  A[70,70] = 1
  b = c(rep(0,70))
  b[1] = init[3]
  b[70] = out$R[71]
  
  yrecup = solve(A, b)
  num1 = yrecup[60]
  num2 = 0.99970638
  num1
  num2
  abs(num1-num2)/num1
  #plot(c(1:70),yrecup)
  
  #-------------------------------
  
  out = matrix(rep(0, times = 70*3), nrow = 70, ncol = 3)
  out
  times = times[-1]
  out[,1] = ysus
  out[,2] = yinfec
  out[,3] = yrecup
  matplot(x = times, y = out, type = "l",
          xlab = "Tiempo", ylab = "S, I, R", main = "Modelo SIR ",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  
  legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
}
shinyApp(ui = ui, server = server)
