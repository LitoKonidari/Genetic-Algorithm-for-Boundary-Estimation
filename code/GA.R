#' Genetic Algorithm
#'
#' @param niter number of iterations
#' @param lambda lira draws
#' @param param Gibbs parameter draws
#' @param restored_lira posterior lira mean
#' @param n_init number of individuals in initial populations
#' @param n_select number of individuals selected to be parents
#' @param draws Gibbs Z draws (binary images)
#'
#' @return best_solution the MAP found from the GA aka the boundary estimate
#' @export
#'
GA <- function(niter = 1000, lambda, param, restored_lira, n_init = 180, n_select = 90, draws) {
  
  if(n_select%%2 !=0){return('Error: n_select must be an even number')}
  
  cols <- dim(lambda)[2]
  
  n <- sqrt(cols)
  #Inital Population
  n_now <- n_init - nrow(draws)
  # Thresholded Images
  init <- threshold_pop(restored_lira, n_now)
  # Noisy thresholded Images
  init2 <- noisy_threshold_pop(init)
  
  initial <- as.matrix(rbind(init, init2, draws))
  
  # Run the genetic algorithm
  iter <- 0
  
  while (iter <= niter) {
    # Selection
    
    # Rank Selection
    parents <- selectTopN(initial, lambda, param, n_select)
    # A parent is now a row in the parents matrix above
    
    # One point Crossover; each pair of parents produces a pair of offspring
    n_off <- n_select/2
    
    offspring1 <- matrix(nrow = n_off, ncol = cols)
    offspring2 <- matrix(nrow = n_off, ncol = cols)
    for (i in 1:n_off) {
      # randomly divide the selected parents to parents 1 and 2 for crossover
      k <- sample(1:n_off, 1)
      parent1 <- parents[k,]
      j <- sample(setdiff(1:n_off, k), 1)
      parent2 <- parents[j,]
      
      ofspr <- crossover(parent1, parent2)
      
      offspring1[i,] <- ofspr[1,]
      offspring2[i,] <- ofspr[2,]
    }
    
    # Mutation
    for (l in 1:n_off) {
      mut1 <- mutate12(offspring1[l,])
      mut2 <- mutate12(offspring2[l,])
      
      offspring1[l,] <- mut1
      offspring2[l,] <- mut2
    }
    
    # Pool offspring & parents to form the next generation
    
    initial <- rbind(parents, offspring1, offspring2)
    
    iter <- iter + 1
    
  }
  
  
  best_solution <- selectMax(initial, lambda, param)
  
  return(best_solution)
}
