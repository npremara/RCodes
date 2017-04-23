library(MASS)

rbivariateMInc <- function(sg,mx,my,sdx,sdy,rho) 
{
  Sigma= matrix(c(sdx,rho,rho,sdy), 2)
  xx=mvrnorm(sg, mu = c(mx,my), Sigma)
  x <- xx[,1]
  y <- xx[,2]
  return(data.frame(x,y))
}

# Use Wishart distribution 
library(MCMCpack)
rbivariateMOut <- function(sg,mx,my,sdx,sdy,rho) 
{
  Sigma= matrix(c(sdx,rho,rho,sdy), 2)
  xx=mvrnorm(sg, mu = c(mx,my), Sigma)
  df=1000
  SigmaW=riwish(df, Sigma)*(df-2-1)
  xx=mvrnorm(sg, mu = c(mx, my), SigmaW)
  x <- xx[,1]
  y <- xx[,2]
  return(data.frame(x,y))
}