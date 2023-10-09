#' One-point Crossover
#'
#' @param parent1 
#' @param parent2 
#'
#' @return the pair of offsprings produced by the two parents
#' @export
#'
crossover <- function(parent1, parent2) {
  l <- length(parent1)
  
  # Randomly pick the crossover point
  point <- c(sample(1:(l-1), 1))
  
  # Turn the parent matrices into vectors for easier manipulation
  offspring1 <- c(parent1[1:point], parent2[(point+1):l])
  offspring2 <- c(parent2[1:point], parent1[(point+1):l])
  
  return(rbind(offspring1, offspring2))
}