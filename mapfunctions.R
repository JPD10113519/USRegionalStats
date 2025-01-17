library(terra)
library(viridisLite)
library(glmnet)
library(dplyr)
library(future)
library(furrr)
library(doFuture)
library(parallel)

# Load and prepare geographic data
# Parameters:
#   map_path: Path to the county boundary shapefile
#   states: Vector of state abbreviations to include
# Returns:
#   SpatVector object with filtered counties
load_geographic_data <- function(map_path, states) {
  map <- vect(map_path)
  subset(map, map$STUSPS %in% states)
}

# Load and clean outcomes data
# Parameters:
#   outcomes_path: Path to the outcomes CSV file
# Returns:
#   Cleaned outcomes dataframe
load_outcomes_data <- function(outcomes_path) {
  outcomes <- read.csv(outcomes_path)
  # Remove counts and standard errors more efficiently using base R
  pattern <- "_se$|_n$"
  outcomes[, !grepl(pattern, names(outcomes))]
}

# Merge geographic and outcomes data
# Parameters:
#   geography: SpatVector object with geographic data
#   outcomes: Cleaned outcomes dataframe
# Returns:
#   SpatVector with merged data
merge_geo_outcomes <- function(geography, outcomes) {
  # Create composite keys for matching
  geo_key <- paste(as.numeric(geography$STATEFP), as.numeric(geography$COUNTYFP))
  outcomes_key <- paste(as.numeric(outcomes$state), as.numeric(outcomes$county))
  
  # Perform merge
  indices <- match(geo_key, outcomes_key)
  geo_data <- as.data.frame(geography)
  geo_data[!is.na(indices), names(outcomes)] <- outcomes[indices[!is.na(indices)], ]
  
  # Update geography values and return
  values(geography) <- geo_data
  geography
}

# Train model for region detection
# Parameters:
#   data: Combined geographic and outcomes data
#   region_states: Vector of state abbreviations defining the region of interest
#   exclude_cols: Vector of column indices to exclude from training
# Returns:
#   List containing model, coefficients, and predictions
train_region_model <- function(data, region_states, exclude_cols) {
  # Create binary target
  data$ROI <- ifelse(data$STUSPS %in% region_states, 1, 0)
  
  # Prepare training data
  train_data <- as.data.frame(data)[, -exclude_cols]
  x_matrix <- makeX(train_data, na.impute = TRUE)
  
  # Train model with cross-validation
  model_cv <- cv.glmnet(x_matrix, data$ROI, family = "binomial", alpha = 1, 
                        parallel = TRUE, nfolds = 5)
  
  # Get predictions using optimal lambda
  predictions <- predict(model_cv, newx = x_matrix, s = "lambda.min")
  
  list(
    model = model_cv,
    coefficients = coef(model_cv, s = "lambda.min"),
    predictions = predictions
  )
}

# Plot region predictions in RStudio
# Parameters:
#   geography: SpatVector object
#   predictions: Vector of predictions
#   n_regions: Number of top regions to highlight
plot_regions <- function(geography, predictions, n_regions = 217) {
  # Create plot in RStudio's plotting pane
  geography$predictions <- predictions
  plot(geography, col = plasma(2)[as.numeric(rank(-geography$predictions) <= n_regions) + 1],
       main = "Regional Analysis Results")
}

# Main workflow function
# Parameters:
#   map_path: Path to county boundary shapefile
#   outcomes_path: Path to outcomes data
#   states: States to include in analysis
#   region_states: States defining region of interest
#   exclude_cols: Columns to exclude from model training
# Returns:
#   List containing processed data and model results
run_regional_analysis <- function(map_path, outcomes_path, states, region_states, 
                                  exclude_cols = c(1:14, 6889:6891)) {
  
  # This is where we set up the graphics device!
  if (is.null(dev.list())) {
    pdf(file = NULL)  # Creates a default PDF device that writes to null
  }
  
  # Enable parallel processing
  nc <- detectCores() - 1  # Use all cores except one
  if(nc > 1) {
    cl <- makeCluster(nc)
    on.exit(stopCluster(cl))
  }
  
  # Load and process data
  geography <- load_geographic_data(map_path, states)
  outcomes <- load_outcomes_data(outcomes_path)
  merged_data <- merge_geo_outcomes(geography, outcomes)
  
  # Train model
  model_results <- train_region_model(merged_data, region_states, exclude_cols)
  
  # Generate plot
  plot_regions(merged_data, model_results$predictions)
  
  # Return results
  list(
    data = merged_data,
    model = model_results$model,
    coefficients = model_results$coefficients,
    predictions = model_results$predictions
  )
}

# Example usage:
 results <- run_regional_analysis(
   map_path = "cb_2023_us_county_500k",
   outcomes_path = "county_outcomes.csv",
   states = all.states,
   region_states = c("UT","ID","NV","WY")
 )
 
 

 
 
 # Clean up existing devices
 while (dev.cur() > 1) dev.off() 
 
 # Start fresh
 options(device = "RStudioGD")
 plot(results$data, col=plasma(2)[as.numeric(rank(-results$predictions)<=100)+1])
 
sum(1*(results$coefficients!=0))
 
#hist(as.numeric(results$coefficients)[which(results$coefficients!=0)])

max <- which(results$coefficients==max(results$coefficients[-1]))
rownames(coeffs)[max] #for NE: jail_pooled_female_p100
min <- which(results$coefficients==min(results$coefficients))
rownames(coeffs)[min] #for NE: kfr_top01_black_male_p1



