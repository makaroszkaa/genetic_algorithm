pkgs <- c("genalg", "ggplot2")
dpnd <- lapply(pkgs, library, character.only = T)

rm(dpnd, pkgs)

# Define dataset and variables
items   <- yaml::read_yaml('ga_knapsack_data.yaml')
dataset <- do.call(rbind.data.frame, items)
wt_lim  <- 20 
rownames(dataset) <- NULL

# Define gene configuration
chromosome <- c(1, 0, 0, 1, 1, 0, 0)
dataset[chromosome == 1, ]        ## what to take with you
cat(chromosome %*% dataset$pnts)  ## how much points does this chromosome give

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

# Design and run the model
iter     <- 100
ga_model <- rbga.bin(size           = 7, 
                     popSize        = 200, 
                     iters          = iter, 
                     mutationChance = 0.01,
                     elitism        = T, 
                     evalFunc       = eval_func)

summary(ga_model, echo = T)

# Extract best solution
best_chromosome <- extract_best(ga_model)

# Evaluate best solution
dataset[best_chromosome == 1, ]
cat(paste(best_chromosome %*% dataset$pnts, "/", sum(dataset$pnts)))

# Plot results
plot(ga_model)
