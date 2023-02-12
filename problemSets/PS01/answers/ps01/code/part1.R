ksfunc <- function(dat) {
  # create empirical distribution of observed data
  ECDF <- ecdf(dat)
  empiricalCDF <- ECDF(dat)
  
  # generate statistic
  D <- max(abs(empiricalCDF - pnorm(dat)))
  
  # generate p-value from base R function
  res <- ks.test(empiricalCDF, pnorm(dat))
  
  # generate p-value
  k <- 1
  fun <- function(n, x){
    exp(((-((2*n)-1)^2)*(pi^2))/((8)*(x^2)))
  }
  mul <- ((sqrt(2*pi))/(D))
  ans <- 0
  tol <- 1e-06
  
  while(TRUE) {
    next_term <- fun(k, D)
    next_term <- next_term*mul
    ans <- ans + next_term
    if(abs(ans)<tol){
      break
    }
    k <- k+1
  }
  
  ans <- ans*mul
  results <- data.frame(stat = D, p.value = res$p.value, func = ans)
  return(results)
  
}

#Creating dataset
set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)

#Calling function
tab1 <- ksfunc(data)
tab1


getAnywhere(ks.test.default)

install.packages("stargazer")
library(stargazer)

stargazer(tab1, type = "latex", title = "KS-test")



