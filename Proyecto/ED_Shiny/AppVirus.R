#Elaborado por Alex Barreto, Sebastian Roberts, Camilo Ruiz
library(deSolve)
library(shiny)
library(shinyWidgets)

#Interfaz de Usuario
#-----------
ui <- fluidPage(
  
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),
  
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
      ),
      conditionalPanel
      (
        condition = "input.modelo == 'SI'&& input.tipoEcuacion != 'seleccionar'",
        sliderInput(inputId = "numPoblacion", label = "Poblacion:",min=10,max=100, value = 10,step = 1),     
        sliderInput(inputId = "numInfec", label = "Infectados (I):",min=2,max=99, value = 2,step = 1),
        sliderInput(inputId = "beta", label = "Tiempo De llegada (Beta):",min=0.2,max=0.7, value = 0.25),
        sliderInput(inputId = "intercept",label = "Intercepto con X: ",
                    min = -100,max = 100,value = 10)
      ),
      conditionalPanel
      (
        condition = "input.modelo == 'SIR'&& input.tipoEcuacion != 'seleccionar'",
        sliderInput(inputId = "numPoblacion", label = "Poblacion:",min=10,max=100, value = 10,step = 1),     
        sliderInput(inputId = "numInfec", label = "Infectados (I):",min=2,max=99, value = 2,step = 1),
        sliderInput(inputId = "numRecuper", label = "Recuperados (R) :",min=0,max=99, value = 0,step=1),
        sliderInput(inputId = "beta", label = "Tiempo De llegada (Beta):",min=0.2,max=0.7, value = 0.25,step = 0.01),
        sliderInput(inputId = "gama", label = "Tasa de recuperacion:",min=0,max=0.5, value = 0.1,step=0.01)
      ) 
    ),
    mainPanel
    (
      plotOutput("model"),
      plotOutput("lineplot")
    )
  )
)
server = function(input, output)
{
  output$model <- renderPlot({
    
    if(input$modelo == "SI" &&input$tipoEcuacion == "EDO")
    {
      init <- c(S = (input$numPoblacion-input$numInfec), I = input$numInfec, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$beta, gama = 0)
      init = init/N
      N = 1
      plot_SIEDO(init, param, N)
      times <- seq(0, 70, by = 1)
      #Grafica de pendiente donde m es el valor de beta 
      output$lineplot <- renderPlot({
        x<- times
        y <- times*input$beta + input$intercept
        plot(x,y, main="Pendiente en relacion con tiempo de llegada ")
      })
    }
    else if(input$modelo == "SIR" && input$tipoEcuacion == "EDO")
    {
      init <- c(S = (input$numPoblacion-input$numInfec), I = input$numInfec, R = input$numRecuper)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$beta, gama = input$gama)
      init = init/N
      N = 1
      
      plot_SIREDO(init, param, N)
    }
    else if(input$modelo == "SI" && input$tipoEcuacion == "EDF")
    {
      init <- c(S = (input$numPoblacion-input$numInfec), I = input$numInfec, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$beta, gama = 0)
      init = init/N
      N = 1
      
      plot_SIEDF(init, param, N)
      times <- seq(0, 70, by = 1)
      #Grafica de pendiente donde m= el valor de beta 
      output$lineplot <- renderPlot({
        x<- times
        y <- times*input$beta + input$intercept
        plot(x,y, main="Pendiente en relacion con tiempo de llegada")
      })
    }
    else if(input$modelo == "SIR" && input$tipoEcuacion == "EDF")
    {
      init <- c(S = (input$numPoblacion-input$numInfec), I = input$numInfec, R = input$numRecuper)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$beta, gama = input$gama)
      init = init/N
      N = 1
      
      plot_SIREDF(init, param, N)
    }
  })
}

#Fcion qie grafica SI por EDO (Euler)
plot_SIEDO = function(init, param, N)
{
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <- beta * S * I - gama * I
           dR <- gama * I
           
           #resultados de las tasas de cambio    
           return(list(c(dS, dI, dR)))
         })
    
  }
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funciÃ³n 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I", main = "SI",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #Etiquetas de los ejes
  legend(40, 0.7, c("Susceptibles", "Infectados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  }

#Fcion qie grafica SIR 
plot_SIREDO = function(init, param, N)
{
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <-  beta * S * I - gama * I
           dR <- gama * I
           #resultados de las tasas de cambio    
           return(list(c(dS, dI, dR)))
         })
  }
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funciÃ³n 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I.R", main = "Modelo SIR ",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #Etiquetas de los ejes
  legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
}
#SI-EDF
plot_SIEDF = function(init, param, N)
{
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <- beta * S * I - gama * I
           dR <- gama * I
           
           #resultados de las tasas de cambio    
           return(list(c(dS, dI, dR)))
         })
  }
  times <- seq(0, 70, by = 1)
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
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
  yinfec
  
  #-------------------------------
  
  out = matrix(rep(0, times = 70*2), nrow = 70, ncol = 2)
  out
  times = times[-1]
  out[,1] = ysus
  out[,2] = yinfec
  matplot(x = times, y = out, type = "l",
          xlab = "Tiempo", ylab = "S, I", main = "Modelo SI",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  
  legend(40, 0.7, c("Susceptibles", "Infectados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
}

#SIR-EDF
plot_SIREDF = function(init, param, N)
{
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <-  beta * S * I - gama * I
           dR <- gama * I
           #resultados de las tasas de cambio    
           return(list(c(dS, dI, dR)))
         })
  }
  
  times <- seq(0, 70, by = 1)
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