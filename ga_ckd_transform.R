pkgs <- c("ggplot2", "data.table")
dpnd <- lapply(pkgs, library, character.only = T)
rm(pkgs, dpnd)

foods    <- yaml::read_yaml('food_dictionary.yaml')
raw_data <- fread("food.csv", stringsAsFactors = F)
food_qty <- fread("food_quantities.csv", stringsAsFactors = F)
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

# Extract raw foods to exclude semi-products programmatically where possible
raw_foods <- fltr_data[grep("raw", fltr_data$prod), ]

# Check within group variation of each category to decide how to trim mean
# values of each.
tmp <- copy(raw_foods)
tmp[, as.list(summary(enrg)), by = "cat"]

tmp_cv <- tmp[, .(sd_e = sd(enrg), mn_e = mean(enrg)), by = cat]
tmp_cv[is.na(tmp_cv)] <- 0
tmp_cv[, cv_e := round(sd_e / mn_e, 2)]
tmp_cv$cat[tmp_cv$cv_e > 0.30]

# TODO: trim categories that have high variation
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

# Compute menu energy and protein
menu <- merge(grp_data, food_qty, by = "category", all = T)
menu[
  , energy := round(unit_energy * qty / 100, 1)
][
  , protein := round(unit_protein * qty / 100, 1)
]





