#' Rank Selection
#'
#' This function performs Rank Selection to the initial population using the ratio of the marginal 
#' posterior of pixel assignments to rank the individuals. We compare every individual with the first 
#' one, by calculating their fitness ratio. This process generates a sequence of ratios, which, when 
#' sorted in increasing order, forms a ranking for all individuals. Finally, we select the n individuals
#' that correspond to the highest ratios.
#'
#' @param init_pop the initial population
#' @param lambda lira draws
#' @param param Gibbs parameter draws
#' @param n number of individuals to select
#' @param ncores numbers of cores to use for parallelization
#'
#' @return the n selected individuals
#' @export
#'
selectTopN <- function(init_pop, lambda, param, n, ncores = 4) {

  top <- init_pop[1,]
  
  m <- nrow(init_pop)
  
  registerDoParallel(cores = ncores)
  ratios <-foreach(i=1:m,.combine= c, .export = c("postRatio", "ratioCheck", "calcIsing",
                                                  "brob", "registerDoParallel", "%dopar%",
                                                  "foreach")) %dopar% {
                                                    r <- postRatio(init_pop[i,], top, lambda, param)
                                                    ratioCheck(r[,1], r[,2])
                                                  }
  log_ratios <- numeric(m)
  for (i in 1:m) {
    if (ratios[[i]] == 0) {
      log_ratios[i] <- -Inf
    } else {
      log_ratios[i] <- log(ratios[[i]])
    }
  }
  
  order_ratios <- order(log_ratios, decreasing = TRUE)
  
  n_top <- order_ratios[1:n]
  top_n_solutions <- rbind(init_pop[n_top,])
  return(as.matrix(top_n_solutions))
}
