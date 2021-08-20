thmod <- function(params, diff, ELO, outcomes, toreturn='logl'){
  
  Wbar = c(0) # list of exponentially weighted past games 
  CP = c()    # choice probabilities 
  
  for(t in 1:length(diff)) {
    # predict choice probability from regression parameters
    delta = params[1] + params[2]*ELO[t] + params[3]*diff[t] + params[4]*Wbar[t]
    P = 1 / (1 + exp(delta)) # untransform log odds
    
    if(toreturn=='logl') CP[t] <- ifelse(outcomes[t] == 1, P, 1-P)
    if(toreturn=='cp') CP[t] <- P
    
    Wbar[t+1] <- params[5]*Wbar[t] + outcomes[t] # update list by weighting it by decay parameter lambda
  }
  
  logl = -sum(log(CP))         # use negative log to find minima later
  if(logl == Inf) logl = 1e10  # catch, but exclude, impossible values
  
  if(toreturn=='logl') return(logl)
  if(toreturn=='cp') return(CP)
}
