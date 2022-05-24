pkgs <- c("ggplot2", "data.table")
dpnd <- lapply(pkgs, library, character.only = T)
rm(pkgs, dpnd)

# Define custom functions
my_summary <- function(i){return(round(mean(i), 2))}

# Read raw data
foods    <- yaml::read_yaml('food_dictionary.yaml')
raw_data <- fread("food.csv", stringsAsFactors = F)
raw_data <- raw_data[, c(1, 2, 12, 18, 23, 29, 30, 31)]
raw_data <- na.omit(raw_data)
col_name <- c("cat", "prod", "enrg", "pro", "carb", "fat", "mass", "units")
colnames(raw_data) <- col_name
raw_data <- as.data.table(apply(raw_data, 2, tolower))
rm(col_name)

# Check if there are categories not present in raw data
cats        <- as.character(unlist(foods))
names(cats) <- cats
not_present <- lapply(X   = cats,
                      FUN = function(i){
                        nrow(raw_data[raw_data$cat %like% i, ])
                      })
not_present <- not_present[not_present == 0]

# Convert numeric columns to allow future computations
cols <- c("enrg", "pro", "carb", "fat", "mass")
raw_data[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
rm(cols)

# Pick up foods from each of categories to make the menu of the day match the
# products available from shops.
fltr_data <- raw_data[cat %in% cats, ]
fltr_data <- dplyr::arrange(fltr_data, cat, desc(enrg), pro)


### ---               Compute mean nutrition facts for raw foods         --- ###

# Extract raw foods to exclude semi-products programmatically where possible
raw_foods <- fltr_data[grep("raw", fltr_data$prod), ]

# Check within group variation of each category to decide how to trim mean
# values of each.
tmp     <- copy(raw_foods)
tmp_smr <- tmp[, as.list(summary(enrg)), by = "cat"]

# Find out which tail is longer to allow the variation optimization
tmp_skewness <- tmp[, as.list(moments::skewness(enrg)), by = "cat"]
colnames(tmp_skewness)[2] <- "skew"
tmp_skewness$skew[is.nan(tmp_skewness$skew)] <- 0
tmp_skewness$skew <- round(tmp_skewness$skew, 2)

# Compute variation coefficients to identify categories with wrong data
tmp_cv <- tmp[, .(sd_e = sd(enrg), mn_e = mean(enrg)), by = cat]
tmp_cv <- merge(tmp_cv, tmp_skewness, by = "cat", all.x = T)
tmp_cv[is.na(tmp_cv)] <- 0
tmp_cv[, cv_e := round(sd_e / mn_e, 2)]
cv_cats <- tmp_cv$cat[tmp_cv$cv_e > 0.30]
names(cv_cats) <- cv_cats

# Trim categories which have variation more than 30 percent
trim_res <- lapply(
  X = cv_cats,
  FUN = function(i){
    temp_cv <- tmp_cv[cat == i, cv_e]
    while (temp_cv > 0.20){
      temp_df <- tmp[cat == i, ]
      temp_qt <- quantile(temp_df$enrg,
                          ifelse(tmp_cv[cat == i, skew] > 0, 0.9, 0.1))
      text_fl <- ifelse(test = tmp_cv[cat == i, skew] > 0, 
                        yes  = "enrg <= temp_qt",
                        no   = "enrg >= temp_qt")
      temp_df <- temp_df[eval(parse(text = text_fl)), ]
      temp_cv <- temp_df[, .(sd_e = sd(enrg), mn_e = mean(enrg)), by = cat]
      temp_cv[is.na(temp_cv)] <- 0
      temp_cv[, cv_e := round(sd_e / mn_e, 2)]
      temp_cv <- temp_cv[, cv_e]
      print(paste('variation of', i, temp_cv))
      text_fl <- ifelse(test = tmp_cv[cat == i, skew] > 0,
                        yes  = "!(cat == i & enrg >= temp_qt)",
                        no   = "!(cat == i & enrg <= temp_qt)")
      tmp     <- tmp[eval(parse(text = text_fl)), ]
    }
    return(temp_cv)
  }
)

# Group raw foods by category and compute mean
raw_foods <- copy(tmp)
raw_foods[, units := NULL]
raw_foods[, prod := NULL]
raw_foods <- raw_foods[, lapply(.SD, my_summary), by = cat]

rm(tmp, tmp_cv, tmp_smr, trim_res, tmp_skewness, cv_cats)

# TODO: find solution for categories which do not have raw attribute

# Check which categories did not fall into raw foods
idx     <- !(unique(fltr_data$cat) %in% unique(raw_foods$cat))
not_raw <- unique(fltr_data$cat)[idx]





# Filter products that are raw to exclude the semi-finished products
fltr_str <- paste0(
  "raw|bread|carrot|pasta|buckwheat|walnuts|butter|milk",
  "|semolina|cream"
)
fltr_raw <- fltr_data[grep(fltr_str, fltr_data$product), ]
grp_data <- fltr_raw[
  , .(mean_e = round(mean(unit_energy, trim = 0.01), 1), 
      mean_p = round(mean(unit_protein, trim = 0.01), 1)), 
  by = category
]







