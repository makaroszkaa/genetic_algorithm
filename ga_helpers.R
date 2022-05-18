# Define evaluation function
eval_func <- function(x) {
  current_solution_survivalpoints <- x %*% dataset$pnts
  current_solution_weight         <- x %*% dataset$wt
  
  if (current_solution_weight > wt_lim){
    return(0)
  } else {
    return(-current_solution_survivalpoints)
  }
}

# Define monitor function to extract best solution
extract_best <- function(id){
  min_eval <- min(id$evaluations)
  flt_eval <- id$evaluations == min_eval
  best_chr <- id$population[flt_eval, ][1, ]
  return(as.integer(best_chr))
}