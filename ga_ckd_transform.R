pkgs <- c("ggplot2", "data.table")
dpnd <- lapply(pkgs, library, character.only = T)

foods    <- yaml::read_yaml('food_dictionary.yaml')
raw_data <- fread("food.csv", stringsAsFactors = F)
raw_data <- raw_data[, c(1, 2, 12, 18, 30)]
raw_data <- na.omit(raw_data)

colnames(raw_data) <- c("category", "product", "energy", "protein", "mass")

raw_data <- as.data.table(apply(raw_data, 2, tolower))

# Check if there are categories not present in raw data
cats        <- as.character(unlist(foods))
names(cats) <- cats
not_present <- lapply(X   = cats,
                      FUN = function(i){
                        nrow(raw_data[raw_data$category %like% i, ])
                      })
not_present <- not_present[not_present == 0]

# Normalize energy and protein to 100 g mass to be able to compare
cols <- c("energy", "protein", "mass")
raw_data[
  , (cols) := lapply(.SD, as.numeric), .SDcols = cols
][
  , unit_energy := round(100 * energy / mass, 1)
][
  , unit_protein := round(100 * protein / mass, 1)
]

# Pick up foods from each of categories to make the menu of the day match the
# products available from shops.
drop_cols <- c("energy", "protein", "mass")
fltr_data <- raw_data[category %in% cats, !drop_cols, with = F]
fltr_data <- fltr_data[unit_protein < 100, ]
fltr_data <- dplyr::arrange(fltr_data, category, desc(unit_energy), unit_protein)

# Filter products that are raw to exclude the semi-finished products
fltr_str <- "raw|bread|carrot|pasta|buckwheat|coffee|tea|walnuts|butter|milk"
fltr_raw <- fltr_data[grep(fltr_str, fltr_data$product), ]
grp_data <- fltr_raw[
  , .(mean_e = round(mean(unit_energy, trim = 0.01), 1), 
      mean_p = round(mean(unit_protein, trim = 0.01), 1)), 
  by = category
]










