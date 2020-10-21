##llr_functions

llr <- function(x,y,z,omega){
  fits <- sapply(z,compute_f_hat,x,y,omega)
  return(fits)
}

compute_f_hat <- function(z,x,y,omega){
  Wz <- make_weight_matrix(z,x,omega)
  X <- make_predictor_matrix(x)
  f_hat = c(1,z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return(f_hat)
}

make_predictor_matrix <- function(x){
  n <- length(x)
  ones <- rep(1,n)
  X <- cbind(ones,x)
  return(X)
}

W <- function(r){
  abs.r <- abs(r)
  if(abs.r<1){
    (1-abs.r ** 3) **3
  } else {
    0
  }
}

make_weight_matrix <- function(z,x,omega){
  r <- abs(x-z)/omega
  w <- sapply(r,W)
  Wz <- w
  return(Wz)
}

n <- 15
x <- rnorm(n)
y <- rnorm(x + rnorm(n))
z <- seq(-1,1,length.out = 100)
omega <- 1
llr(x,y,z,omega)