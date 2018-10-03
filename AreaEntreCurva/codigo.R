
funcion1<- function(x){
  
  return (sin(x))
  
}
  
  funcion2<- function(x){
    return (cos(x))
  }

calcularDeltaDeX<-function(xmin,xmax,numeroDeParticiones)
{
  return ((xmax-xmin)/numeroDeParticiones)
}


sumatoriasReiman<- function(xmin, xmax,numeroDeParticiones){
  deltaDeX<- calcularDeltaDeX(xmin, xmax, numeroDeParticiones)
  intervalos<- seq(xmin,xmax, by=deltaDeX) 
  sumatoria=0;
  aux=1
  while(aux<length(intervalos)-1)
  {
    if( funcion1(intervalos[aux])>funcion2(intervalos[aux] )&& funcion1(intervalos[aux+1])>funcion2(intervalos[aux+1]))
     {
      print("funcion1 mayor")
      sumatoria=sumatoria+( ( funcion1(intervalos[aux])-funcion2(intervalos[aux])+funcion1(intervalos[aux+1])-funcion2(intervalos[aux+1]) )/2)*deltaDeX                                                        
    }
    else
    {
      if( funcion1(intervalos[aux])<funcion2(intervalos[aux] )&& funcion1(intervalos[aux+1])<funcion2(intervalos[aux+1]))
      {
        print("funcion2 mayor")
        sumatoria=sumatoria+( ( funcion2(intervalos[aux])-funcion1(intervalos[aux])+funcion2(intervalos[aux+1])-funcion1(intervalos[aux+1]) )/2)*deltaDeX
      }
        else
        print("cambio")
      
    }
    
    print (sumatoria)
    aux=aux+1
  

  
  }
  x<- seq(-1*pi,2*pi,0.1)
  plot (intervalos,funcion1(intervalos),type="l",col="red")
  lines (intervalos,funcion2(intervalos),col="green")
  x.shade<-seq(xmin,xmax,0.01)
  polygon(c(x.shade,rev(x.shade)),c(sin(x.shade),cos(rev(x.shade))),col="red")
  return (sumatoria)
}
xmin<-0
xmax<-5
numeroDeParticiones<-999
sumatoriasReiman(xmin, xmax,numeroDeParticiones)
