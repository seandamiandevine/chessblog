transformParams <- function(params, LB, UB) { 
  
  transformedParams = c()
  for(paramIdx in 1:length(params)) {
    transformedParams[paramIdx] = LB[paramIdx] + (UB[paramIdx]-LB[paramIdx])/(1+exp(-params[paramIdx]))
  }
  
  return(transformedParams)
  
}
