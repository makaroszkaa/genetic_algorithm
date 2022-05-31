args <- commandArgs(trailingOnly = T)
suppressWarnings(suppressMessages(library("genalg")))
suppressWarnings(suppressMessages(library("data.table")))
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
sub_menu <- sample_sum(menu, a_energ, as.numeric(args), F)

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
df_best <- dataset[best_chromosome == 1, ]

# Prepare output
res_enrg <- paste(best_chromosome %*% dataset$pnts, "/", sum(dataset$pnts))
res_out  <- 
  paste(paste0("protein: ", sum(dataset[best_chromosome == 1, wt])),
        paste0("phosphorus: ", sum(dataset[best_chromosome == 1, sz])),
        paste0("potassium: ", sum(dataset[best_chromosome == 1, pt])),
        paste0("energy: ", sum(dataset[best_chromosome == 1, pnts])))

# Extract quantities for daily menu
qty_out <- menu[cat %in% dataset[best_chromosome == 1, name], ]
qty_col <- c("cat", "mass")
qty_out <- qty_out[, .SD, .SDcols = qty_col]

# Make list for printing
out_l <- list("Energy result"        = res_enrg,
              "Summary"              = res_out,
              "Menu"                 = qty_out)
print(out_l)
