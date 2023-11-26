#Proceso de Vasicek
vasicek <- function(a,b,sigma,X0,T,N){
  dt = T/N
  X <- numeric(N+1)
  X[1] = X0
  for(i in 1:N){
    Z = rnorm(1,0,1)
    X[i+1] = X[i] + a*(b-X[i])*dt + sigma*sqrt(dt)*Z 
  }
  return(X)
}


#Ejemplo

#T = 10
#N = 1000
#T_ = seq(0,T,length.out=N+1)

#b = 0.75
#s = 0.1

#plot(T_,vasicek(0.1,b,s,1,T,N), main = "Proceso de Vasicek",
#     xlab = "t",ylab = "Xt",type = "l",col = "#FF8811",lwd=2,ylim = c(0,1.2) )
#lines(T_,vasicek(1,b,s,0,T,N),type = "l", col = "#7CBF5F",lwd=2)

#legend(6, 1.5, legend=c(TeX(sprintf(r'($b = 2, \sigma = 0.1$)')),TeX(sprintf(r'($b= 0.5, \sigma = 0.3$)'))),
#       col=c("#FF8811",  "#7CBF5F"), lty=1, cex=1,bty = "n")


