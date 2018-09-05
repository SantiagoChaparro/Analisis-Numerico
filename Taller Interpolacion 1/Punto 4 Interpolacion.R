lagrange = function(x,y,a){
  n = length(x)
  if(a < min(x) || max(x) < a) stop("No está interpolando")
  X = matrix(rep(x, times=n), n, n, byrow=T)
  mN = a - X; diag(mN) = 1
  mD = X - t(X); diag(mD) = 1
  Lnk = apply(mN, 1, prod)/apply(mD, 2, prod)
  sum(y*Lnk)
}

require(PolynomF)

x = c( 35, 45 , 55,65, 75)
y = c(35, 48, 70, 40, 22)


#lagrange(x[0:3],y[0:3], 55)


polyAjuste = poly.calc(x,y)
polyAjuste
cont <- 0
suma<-0
while(cont<=55)
{
  suma<-suma+ polyAjuste(cont)
  print(cont)
  print(polyAjuste(cont))
  cont<- cont+1
}
suma
curve(polyAjuste,0,80)

cont <- 36
suma<-0
while(cont<55)
{
  lagrange(x[0:4],y[0:4], cont)
}


