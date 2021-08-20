fitmod <- function(diff, ELO, outcomes, nIter=1000,  
                   nParams=5, LB=NULL, UB=NULL, 
                   maxX0 = 1000, tol=25, 
                   verbose=T) {
  source('model/thMod.R')
  source('model/getx0.R')
  
  if(is.null(LB)) LB = c(-50, -50, -50, -15, 0) # lower bound
  if(is.null(UB)) UB = c(50, 50, 50, 15, 2)     # upper bound
  
  obfunc = function(x) thmod(x, diff=diff, ELO=ELO, outcomes=outcomes)
  bestLL = 1e10
  bestpars = c()
  bestConv = NA
  tolCount = 0
  for(i in 1:nIter){
    if(verbose) cat('iteration:', i, '|| getting starting pars...\n')
    x0 = getx0(5, LB, UB, obfunc, maxX0)
    opt = nlminb(start = x0, objective = obfunc, lower = LB, upper = UB) 
    if(opt$objective < bestLL & opt$convergence==0) {
      bestLL = opt$objective
      bestpars = opt$par
      bestConv = opt$convergence
      cat('found new minimum!\n')
    } else {
      tolCount = tolCount+1
      if(tolCount > tol) break
    }
    if(verbose) cat('iteration:', i, '|| converged?', opt$convergence,'|| this -loglik = ', opt$objective, '|| best -loglik = ', bestLL, '\n')
  } # end for loop
  if(bestLL==1e10) warning('could not find suitable pars.')
  out=list(par = bestpars, loglik = bestLL, conv=bestConv)
  names(out$par) = c('bias', 'b_elo', 'b_ratingDiff', 'b_wO', 'lambda')
  return(out)
}



