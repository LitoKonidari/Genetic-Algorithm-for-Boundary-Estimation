#' Calculate Log Posterior
#'
#' Calculates components of the ratio to compare two posterior distributions
#' of the pixel assignments. Same as Dr. McKeough's function, but vectorized for speed. 
#' Original function:
#' https://github.com/kmckeough/LI-Segmentation-RPackage/blob/main/LIsegmentation/R/postRatio.R
#'
#' @param Z1 vector of matrix representation of top pixel assignments (-1,1)
#' @param Z2 vector of matrix representation of bottom pixel assignments (-1,1)
#' @param lambda posterior draws of LIRA output, one draw per row
#' @param param posterior draws of other parameters, one draw per row
#' @return the log posterior componants for the ratio calculation
#' @export
#'
postRatio <- function(Z1,Z2,lambda,param){
  
  n<-sqrt(length(Z1))
  Z1_array<-array(Z1,dim=c(n,n))
  Z2_array<-array(Z2,dim=c(n,n))
  
  ising1<-calcIsing(Z1_array)
  ising2<-calcIsing(Z2_array)
  
  lambda_mat <- as.matrix(lambda)
  
  tau0_mat <- matrix(param$tau0, nrow = nrow(lambda), ncol = ncol(lambda))
  sigma0_mat <- matrix(param$sigma0, nrow = nrow(lambda), ncol = ncol(lambda))
  
  tau1_mat <- matrix(param$tau1, nrow = nrow(lambda), ncol = ncol(lambda))
  sigma1_mat <- matrix(param$sigma1, nrow = nrow(lambda), ncol = ncol(lambda))
  
  matrix1 <- log(sqrt(2*pi*sigma0_mat))+(lambda_mat - tau0_mat)^2/(2*sigma0_mat)
  matrix2 <- log(sqrt(2*pi*sigma1_mat))+(lambda_mat - tau1_mat)^2/(2*sigma1_mat)
  
  log1 <- rep(0, nrow(param)) 
  log2 <- rep(0, nrow(param)) 
  
  for (ii in 1:nrow(param)) {
    log1[ii] <- -1*sum((1-Z1)/2*matrix1[ii,] + (1+Z1)/2*matrix2[ii,]) + param$beta[ii]*ising1
    log2[ii] <- -1*sum((1-Z2)/2*matrix1[ii,] + (1+Z2)/2*matrix2[ii,]) + param$beta[ii]*ising2
  }
  
  return(cbind(log1,log2))
  
}