arModels <- function(x){
  reg <- list()
  reg1 <- lm(x[2:n] ~ x[1:(n-1)])
  reg2 <- lm(x[3:n] ~ x[1:(n-2)] + x[2:(n-1)])
  reg3 <- lm(x[4:n] ~ x[1:(n-3)] + x[2:(n-2)] + x[3:(n-1)])  
  list(reg1, reg2, reg3)
}


arModelSum <- function(x){
  s <- x[1:(n-7)] + x[2:(n-6)] + x[3:(n-5)] + x[4:(n-4)] + x[5:(n-3)] + x[6:(n-2)] + x[7:(n-1)]
  lm(x[8:n] ~ s)
}
