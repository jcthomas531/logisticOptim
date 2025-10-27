cd_fit  <- function(beta0, X, Y, lpdf, dlpdf) {
  
  #calculate the gradient at given point beta0
  #gradient is just a vector of all of the partial derivs
  #get partial deriv at this beta wrt all x_i, take the norm and eval against tolerance
  gradient <- rep(NA, ncol(X))
  for (i in 1:ncol(X)) {
    gradient[i] <- dlpdf(beta0, i, X, Y)
  }
  betaEst <- beta0
  #choosing same tolerance he used in example, using l2 norm
  #although could also set this to maximum... not sure which is better
  while(norm(gradient, type = "2") > 1e-8) {
    #keeping track of all the betas not necessary, but this is the way i first thought of
    updateHolder <- vector(mode = "list", length = ncol(X) + 1)
    updateHolder[[1]] <- betaEst
    for (i in 1:ncol(X)) {
      #get partial deriv wrt ith component
      dfi <- dlpdf(updateHolder[[i]], i, X, Y)
      #update beta
      updateHolder[[i+1]] <- updateHolder[[i]]
      updateHolder[[i+1]][i,] <- updateHolder[[i+1]][i,] - dfi
      #are we moving in correct direction? ie is likelihood getting bigger, if not...
      while (lpdf(updateHolder[[i+1]], X, Y) < lpdf(updateHolder[[i]], X, Y)) {
        #half step
        dfi <- dfi/2
        updateHolder[[i+1]] <- updateHolder[[i]]
        updateHolder[[i+1]][i,] <- updateHolder[[i+1]][i,] - dfi
      }
    }
    #beta estimate for this iteration
    betaEst <- updateHolder[[ncol(X)+1]]
    #deriv at this estimate
    for (i in 1:ncol(X)) {
      gradient[i] <- dlpdf(betaEst, i, X, Y)
    }
    
  }
  return(betaEst)
}














