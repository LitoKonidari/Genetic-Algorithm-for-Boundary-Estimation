#' Noisy Thresholded Images
#'
#' For each thresholded image, we flip the value of every pixel with a probability p.
#'
#' @param init_pop the thresholded images
#' @param p probability of flipping every pixel
#'
#' @return the noisy thresholded images
#' @export
#'
noisy_threshold_pop <- function(init_pop, p = 0.05) {
  row <- nrow(init_pop)
  col <- ncol(init_pop)
  # Inject random noise by flipping random pixels
  for (i in 1:row) {
    for (j in 1:col) {
      if (runif(1) < p) {
        init_pop[i,j] <- -init_pop[i,j]  # Flip the bit
      }
    }
  }
  return(init_pop)
}