




cd_fit  <- function(beta0, X, Y, lpdf, dlpdf) {
  
  #eval b0 at logpdf
  
  #find deriv of 
  
  
}




eval0 <- lpdf(beta0, X, Y)



updateHolder <- vector(mode = "list", length = ncol(x) + 1)
updateHolder[[1]] <- beta0
#deriv with respect to x1
i <- 1
df1 <- dlpdf(beta0, i, X, Y)
updateHolder[[1 + i]] <- updateHolder[[1]][i,] - df1
#are we moving in the correct direction?
lpdf(updateHolder[[1 + i]], X, Y) > lpdf(updateHolder[[1]], X, Y)
#deriv with respect to x2
i <- 2
df2 <- dlpdf(updateHolder[[1 + i]], i, X, Y)
updateHolder[[1 + i]] <- updateHolder[[i]][i,] - df1
#are we moving in the correct direction?
lpdf(updateHolder[[1 + i]], X, Y) > lpdf(updateHolder[[i]], X, Y)


#operationalize

updateHolder <- vector(mode = "list", length = ncol(x) + 1)
updateHolder[[1]] <- beta0
for (i in 1:ncol(X)) {
  
}



