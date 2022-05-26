pkgs <- c("genalg", "ggplot2", "data.table")
dpnd <- lapply(pkgs, library, character.only = T)
source("ga_helpers.R")

# Read menu data
menu <- readRDS('menu_file.rds')

# Define limitations
wt_lim  <- 0.7 * 90         ## low protein diet is 0.6 - 0.8 grams per 1 kg
a_energ <- (25  * 90) * 1.1 ## adequate energy for mid-level activity + 10%

# Subset foods from menu
sub_menu <- sample_sum(menu, a_energ, 160, T)

# Make dataset
dataset <- sub_menu[, c(1, 5, 2)]
colnames(dataset) <- c("name", "pnts", "wt")

# Design and run the model
iter     <- 100
ga_model <- rbga.bin(size           = nrow(dataset), 
                     popSize        = 200, 
                     iters          = iter, 
                     mutationChance = 0.01,
                     elitism        = T, 
                     evalFunc       = eval_func)

# Extract best solution
best_chromosome <- extract_best(ga_model)

# Evaluate best solution
dataset[best_chromosome == 1, ]
cat(paste(best_chromosome %*% dataset$pnts, "/", sum(dataset$pnts)))
cat(paste(paste0(" Total protein: ", sum(dataset[best_chromosome == 1, wt])),
          "\n",
          paste0("Total energy: ", sum(dataset[best_chromosome == 1, pnts]))))

# Extract quantities for daily menu
menu[cat %in% dataset[best_chromosome == 1, name], ]
