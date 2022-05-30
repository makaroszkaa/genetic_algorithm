pkgs <- c("genalg", "ggplot2", "data.table")
dpnd <- lapply(pkgs, library, character.only = T)
source("ga_helpers.R")

# Read static dictionaries
menu      <- readRDS('menu_file.rds')
ckd_stage <- yaml::read_yaml("ckd_stage.yaml")

# Dynamic inputs
body_mass <- 90 ## body weight
cur_stage <- 4  ## current ckd stage
ckd_stage <- rbindlist(ckd_stage)
ckd_stage <- ckd_stage[stage == cur_stage, ]

# Define limitations
wt_lim  <- body_mass * ckd_stage$pros
sz_lim  <- ckd_stage$phosp
pt_lim  <- ckd_stage$potas
a_energ <- (25  * body_mass) * 1.1 ## adequate energy for mid-level activity

# Subset foods from menu
sub_menu <- sample_sum(menu, a_energ, 12, T)

# Make dataset
col_nms <- c("cat", "cals", "pros", "phosp", "potas")
dataset <- sub_menu[, .SD, .SDcols = col_nms]
colnames(dataset) <- c("name", "pnts", "wt", "sz", "pt")

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
cat(paste(best_chromosome %*% dataset$pnts, "/", sum(dataset$pnts), "\n"))

cat(paste(paste0(" Total protein: ", sum(dataset[best_chromosome == 1, wt])),
          "\n",
          paste0("Total phosphorus: ", sum(dataset[best_chromosome == 1, sz])),
          "\n",
          paste0("Total potassium: ", sum(dataset[best_chromosome == 1, pt])),
          "\n",
          paste0("Total energy: ", sum(dataset[best_chromosome == 1, pnts])),
          "\n"))

# Extract quantities for daily menu
qty_out <- menu[cat %in% dataset[best_chromosome == 1, name], ]
qty_col <- c("cat", "mass")
qty_out <- qty_out[, .SD, .SDcols = qty_col]
qty_out
