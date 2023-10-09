#' Adjacent Mutation
#'
#' This function performs Adjacent Mutation. This approach involves flipping a pixel based on
#' the majority state of its adjacent pixels. Here, we consider pixels as adjacent if they share
#' an edge or corner. If the adjacent pixels have equally different states, the pixel remains the same.
#'
#' @param offspring the offspring to be mutated
#'
#' @return
#' @export
#'
#' @examples
mutate <- function(offspring) {
  offspring <- matrix(offspring, ncol = sqrt(length(offspring)), nrow = sqrt(length(offspring)))
  r <- nrow(offspring)
  c <- ncol(offspring)
  rate <- 1/(2*c)
  
  # Define the 3x3 window boundaries
  for (row in 1:r) {
    for (col in 1:c) {
      if (runif(1) < rate) {
        row_start <- max(1, row - 1)
        row_end <- min(r, row + 1)
        col_start <- max(1, col - 1)
        col_end <- min(c, col + 1)
        
        ones <- sum(offspring[row_start:row_end, col_start:col_end] == 1)
        zeros <- sum(offspring[row_start:row_end, col_start:col_end] == -1)
        length <- length(offspring[row_start:row_end, col_start:col_end])
        
        m <- sum(offspring[row_start:row_end, col_start:col_end])
        if (m > 0) {
          new_bit <- 1
        } else if (m < 0) {
          new_bit <- -1
        } else {
          new_bit <- offspring[row, col]
        }
        
        offspring[row,col] <- new_bit
      }
    }
  }
  return(c(offspring))
}