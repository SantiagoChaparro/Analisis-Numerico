


a<-matrix(c(2,5,1,7),2)
a

b<-matrix(c(3,5,9,1,5,8,1,2,3),3)

b

aL<- sqrt(length(a))

bL<- sqrt(length(b))


i<-1
j<-1
sumaA<-0
cont<-1
while (i<=aL)
{
  
  while (j<=aL)
  {

    if(j>= cont)
    {
      print (cont)
      print(a[i,j])
      sumaA=sumaA+a[i,j]
    }
    j<-j+1
  }
  cont<-cont+1
  i<-i+1
  j<-1
}



i<-1
j<-1
sumaB<-0
cont<-1
while (i<=bL)
{
  
  while (j<=bL)
  {
    
    if(j>= cont)
    {
      print (cont)
      print(b[i,j])
      sumaB=sumaB+b[i,j]
    }
    j<-j+1
  }
  cont<-cont+1
  i<-i+1
  j<-1
}





print("La suma de la matriz a es:")
print (sumaA)


print ("La suma de la matriz b es: ")
print (sumaB)