pkgs <- c("genalg", "ggplot2", "data.table")
dpnd <- lapply(pkgs, library, character.only = T)
source("ga_helpers.R")

# Read menu data
menu <- readRDS('menu_file.rds')

# Define limitations
wt_lim  <- 0.7 * 90 ## low protein diet is 0.6 - 0.8 grams per 1 kg of body mass
a_energ <- 25  * 90 ## adequate energy for mid-level activity

# TODO: randomly pick up products until you have reached adequate energy

# Make dataset
dataset <- menu[, c(1, 5, 2)]
colnames(dataset) <- c("name", "pnts", "wt")

# Define gene configuration
set.seed(123)
chromosome <- sample(x = c(0, 1), size = nrow(dataset), replace = T)
dataset[chromosome == 1, ]        ## example of what to include
cat(chromosome %*% dataset$pnts)  ## how much energy does it contain

# Design and run the model
iter     <- 100
ga_model <- rbga.bin(size           = nrow(dataset), 
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