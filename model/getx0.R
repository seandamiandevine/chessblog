getx0 <- function (nParams, LB, UB, obfunc, maxIter) { 
  
  iter = 1
  while(iter < maxIter) {
    x0 = sapply(1:nParams, function(p) runif(1, LB[p], UB[p]))
    loglik = obfunc(x0)
    if(loglik != 1e+10) break
    iter = iter+1
  }
  
  if(iter==maxIter) warning('Reached max iter!')
  return(x0)
  
}


