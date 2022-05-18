pkgs <- c("genalg", "ggplot2", "data.table")
dpnd <- lapply(pkgs, library, character.only = T)
source("ga_helpers.R")

# Prepare a list of available foods from a dictionary of foods available from
# markets and nutrition data. This allows to create the dataset programmatically
foods    <- yaml::read_yaml('food_dictionary.yaml')
raw_data <- fread("nutrients_csvfile.csv", stringsAsFactors = F)

raw_data$Protein[raw_data$Protein == 't']   <- 0.1 ## replace unknown with min
raw_data$Calories[raw_data$Calories == 't'] <- 1.0 ## replace unknow with min

# Check which foods are not present
no_nutrition_facts <- as.character(unlist(foods))
names(no_nutrition_facts) <- no_nutrition_facts
not_present <- lapply(X = no_nutrition_facts,
                      FUN = function(i){
                        nrow(raw_data[raw_data$Food %like% i, ])
                      })
not_present <- not_present[not_present == 0]

# Define limitations
wt_lim  <- 0.7 * 90 ## low protein diet is 0.6 - 0.8 grams per 1 kg of body mass
a_energ <- 25  * 90 ## adequate energy for mid-level activity
