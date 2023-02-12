# creating linear likelihood function:

linearl <- function(theta, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  sigma2 <- theta[k+1]^2
  beta <- theta[1:k]
  e <- y - X%*%beta
  logl <- -.5*n*log(2*pi) - .5*n*log(sigma2) - ((t(e) %*% e) / (2*sigma2))
  return(-logl)
}

# create data

set.seed(123)
data_ex <- data.frame(x = runif(200, 1, 10))
data_ex$y = 0 + 0.275*data_ex$x + rnorm(200, 0, 1.5)

# find parameters that specify point

linear.MLE <- optim(fn=linearl, 
                    par=c(1,1,1), 
                    hessian = TRUE, 
                    y = data_ex$y, 
                    X = cbind(1, data_ex$x), 
                    method = "BFGS")
linear.MLE$par

# test against LM
reg1 <- lm(data_ex$y ~ data_ex$x)

stargazer(reg1, type = "latex", title = "Regression")
