
funcion<-function(x){return(sin(x))}

obtenerDeltaX<-function(intervalo, particiones){
  return((intervalo[2]-intervalo[1])/particiones)#especificar cifras
}
obtenerDeltaY<-function(inicial, final){
  return(funcion(final)-funcion(inicial))
}
Xi<-function(intervalo, i, deltaX){
  return(intervalo[1]+i*deltaX)#especificar cifras
}
calcularLongitudSegmento<-function(deltaX,deltaY){
  return(sqrt((deltaX^2)+(deltaY^2)))
}
calcularLongitudDeCurva<-function(intervalo, particiones){
  contadorLongitud<-0.0000
  deltaX<-obtenerDeltaX(intervalo, particiones)
  i<-0.00000
  while(i<particiones){
    xi<-Xi(intervalo, i, deltaX)
    xj<-xi+deltaX
    deltaY<-obtenerDeltaY(xi,xj)
    contadorLongitud<-contadorLongitud+calcularLongitudSegmento(deltaX,deltaY)
    print(contadorLongitud)
    i=i+1
  }
  return(contadorLongitud)
}

intervalo<-c(0,2*pi)
particiones<-999
resultado<-calcularLongitudDeCurva(intervalo, particiones)
print(resultado)
intervalos=seq(-4*pi,4*pi,0.01)
plot (intervalos,funcion(intervalos) ,type="l",col="black")

intervalos=seq(0,2*pi,0.01)
lines (intervalos,funcion(intervalos),col="red")





