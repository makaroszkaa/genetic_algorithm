pkgs <- c("ggplot2", "data.table")
dpnd <- lapply(pkgs, library, character.only = T)
rm(pkgs, dpnd)

# Define custom functions
my_summary <- function(i){return(round(mean(i), 2))}

# Read raw data
foods    <- yaml::read_yaml('food_dictionary.yaml')
nr_prods <- yaml::read_yaml('not_raw_products.yaml') ## not raw products
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


# Compute nutrition facts for raw foods ----------------------------------------

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


# Compute nutrition facts for not raw foods ------------------------------------

# Filter not raw products using product dictionary
nr_prods <- unlist(nr_prods)
not_raw  <- fltr_data[prod %in% nr_prods, !c("prod", "units"), with = F]
not_raw  <- not_raw[, lapply(.SD, my_summary), by = cat]


# Prepare menu -----------------------------------------------------------------

# Round servings to make an easier judgement if the servings are correct
menu_df      <- rbind(raw_foods, not_raw)
menu_df$mass <- round(menu_df$mass, 0)

# Limit all meat protein sources to 100 g serving per day
meats <- c("beef", "chicken", "lamb", "pork", "salmon", "tuna", "turkey")
menu_df$mass[menu_df$cat %in% meats] <- 100
menu_df <- dplyr::arrange(menu_df, cat)

# Change bread and butter serving since it is fixed by requirements
menu_df$mass[menu_df$cat == 'bread']  <- 250
menu_df$mass[menu_df$cat == 'butter'] <- 30

# Compute per-serving nutrition data
menu_df[, `:=` (pros = round(mass * pro / 100, 2),
                cars = round(mass * carb / 100, 2),
                fats = round(mass * fat / 100, 2),
                cals = round(mass * enrg / 100, 2))]

# Save menu as data.frame
menu_df <- menu_df[, c(1, 7:10, 6)]
saveRDS(menu_df, file = 'menu_file.rds')



