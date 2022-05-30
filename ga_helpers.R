# Define evaluation function
eval_func <- function(x) {
  cur_pnts <- x %*% dataset$pnts
  cur_wt   <- x %*% dataset$wt
  cur_sz   <- x %*% dataset$sz
  cur_pt   <- x %*% dataset$pt
  
  cond     <- any(cur_wt > wt_lim, cur_sz > sz_lim, cur_pt > pt_lim)
  
  if (cond){return(0)} else {return(-cur_pnts)}
}

# Define monitor function to extract best solution
extract_best <- function(id){
  min_eval <- min(id$evaluations)
  flt_eval <- id$evaluations == min_eval
  best_chr <- id$population[flt_eval, ][1, ]
  return(as.integer(best_chr))
}

# Define function which picks random foods from menu
sample_sum <- function(df, max_sum, randn, verbose = T){
  tmp <- data.table::as.data.table(data.frame(matrix(ncol = ncol(df), 
                                                     nrow = 0)))
  colnames(tmp)   <- colnames(df)
  accumulated_sum <- 0
  while (accumulated_sum <= a_energ){
    set.seed(randn)
    pick_food <- sample(df$cat, 1)
    pick_df   <- df[cat == pick_food, ]
    tmp       <- rbind(tmp, pick_df)
    randn     <- randn + 1
    accumulated_sum <- accumulated_sum + pick_df$cals
    
    # Drop picked line from df to avoid double entries of the same food
    df <- df[cat != pick_food, ]
    
    if (verbose){
      print(paste0("Accumulated energy sum is ", accumulated_sum, " calories"))
    }
  }
  return(tmp)
}

