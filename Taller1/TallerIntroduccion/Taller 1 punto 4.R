#Punto 4
error = function(vel, time, eVel, eTime){
  errorAbsoluto <- (vel*eVel) + (time*eTime)
  distancia <- vel*time
  errorRelativo <- (eVel/vel)+(eTime/time)
  cat("Distancia recorrida : ", distancia, " con error de + o -", errorAbsoluto,
      " \nPorcentaje de error relativo: ", errorRelativo*100, "%")
}

error(4,5,0.1,0.1)



