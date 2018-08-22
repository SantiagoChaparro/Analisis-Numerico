fDeX <- function(x)
{
  return(log(x+2)-sin(x))
}


funcionRecursiva<- function(n,vectorSolucion)
{
  if(fDeX(vectorSolucion[1]<=0.00000001))
  {
    return (vectorSolucion[1])
  }
  else
  {
    return (vectorSolucion[2]-(fDeX(vectorSolucion[2])*(vectorSolucion[1]-vectorSolucion[2])/fDeX(vectorSolucion[1])))
  }
  
}


