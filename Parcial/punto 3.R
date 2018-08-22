
error<- function (x0,x){
  Xlen<- length(x0)
  cont2<-1
  
  while(cont2<=Xlen)
  {
    if(abs(x0[cont2]-x[cont2])<=(1/10000))
    {
      print("no dio")
      return (0)
    }
    cont2<-cont2+1
  }
  print("dio el error")
  return (1)

    
  
}



jacobi<-function(t,x0, b, aDiagonal, iteraciones){
  cont<-0
  diagonalInversa<-solve(aDiagonal)
  diagonalInversa
  while(cont<iteraciones && error(x0,x)==0){
    print (cont)
    r<-t%*%x0
    c<-diagonalInversa%*%b
    x<-r+c
    x0<-x
    
    cont<-cont+1
    print (x0)
  }
  print("Iteraciones para que diera con un error de 1/10000: ")
  print (cont)
  return (x0)
}



x0<-c(1,1,1)
b<-c(69,47,68)
b

a<-matrix(c(8,2,2,9,7,8,2,2,6),3)
a
as.vector(a)
diagonal<-diag(a)
diagonal
aDiagonal<-diag(diagonal , sqrt(length(a)) , sqrt(length(a)) )
aDiagonal
solve(aDiagonal)

t<-matrix(solve(aDiagonal)%*%(aDiagonal-a),sqrt(length(a)),sqrt(length(a)))
t
info<-(eigen(t))
info
valoresPropios<-c(info['values'])

valoresPropios<-unlist(valoresPropios,use.names = FALSE)
valoresPropios




if(max(valoresPropios)<1 && max(valoresPropios*(-1))<1){
  iteraciones<-25
  x<-jacobi(t,x0, b,aDiagonal, iteraciones)
  print("EL MÉTODO CONVERGE AL VECTOR SOLUCIÓN...")
  print(x)
}else{
  

  print("EL MÉTODO NO CONVERGE PARA ESTE SISTEMA DE ECUACIONES")
  iteraciones<-25

  
}




