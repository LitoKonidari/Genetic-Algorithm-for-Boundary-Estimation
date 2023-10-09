#' Generate Thresholded Images
#'
#' This is a quantile-based thresholding approach meant to uncover the different intensity
#' layers of the posterior LIRA mean. We construct a sequence ranging from 0.25 to 0.99, with 
#' a user-defined length corresponding to the number of thresholded images we want returned. 
#' For each element x in the sequence, we calculate the lower x-th quantile, namely g. Next, 
#' we set all the LIRA posterior mean values below g to -1 (background), and those above it 
#' to +1 (source).
#'
#' @param restored posterior lira mean
#' @param row number of thresholded images returned
#'
#' @return the thresholded images
#' @export
#'
threshold_pop <- function(restored, row) {
  n <- sqrt(length(restored))
  
  rand <- sort(seq(0.25, 0.99, length = row))
  
  init_pop <- matrix(rep(-1, times = length(rand)*n^2), nrow = length(rand), ncol = n^2)
  
  for(ii in 1:row){
    t <- quantile(restored, rand[ii])
    init_pop[ii, which(restored > t)] <- 1
  }
  return(init_pop)
}