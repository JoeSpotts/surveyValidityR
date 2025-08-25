library(eRm)
library(dplyr)
library(purrr)
library(WrightMap)

# Extract construct identifiers from column names
constructs <- unique(sub("q.*", "", names(complete_efa)[-1]))

run_pcm_model <- function(data, construct) {
  # Filter columns for the current construct
  item_columns <- grep(construct, colnames(data), value = TRUE)
  construct_data <- data[, item_columns]
  
  # Exclude rows with any NA values in these items
  construct_data <- na.omit(construct_data)
  
  # Convert to numeric
  construct_data <- construct_data %>% mutate(across(everything(), as.numeric))
  
  # Run Partial Credit Model
  pcm_model <- eRm::PCM(construct_data)
  
  # Return the PCM model
  return(pcm_model)
}

# Create a list of PCM models
pcm_models <- map(constructs, ~run_pcm_model(sample_n(complete_efa, 100), .x))
names(pcm_models) <- constructs

# Function to extract PCM model information
extract_pcm_info <- function(pcm_model, data) {
  # Extract item difficulty parameters (eta) and their standard errors
  item_difficulty <- tryCatch({
    coef(pcm_model)
  }, warning = function(w) {
    message("Warning in coef(pcm_model): ", conditionMessage(w))
    NULL
  }, error = function(e) {
    message("Error in coef(pcm_model): ", conditionMessage(e))
    NULL
  })
  
  if (is.null(item_difficulty)) {
    return(NULL)
  }
  
  print("Item Difficulty Parameters:")
  print(item_difficulty)
  
  # Extract person parameters (thetas)
  person_parameters <- tryCatch({
    person.parameter(pcm_model)$theta.table
  }, warning = function(w) {
    message("Warning in person.parameter(pcm_model): ", conditionMessage(w))
    NULL
  }, error = function(e) {
    message("Error in person.parameter(pcm_model): ", conditionMessage(e))
    NULL
  })
  
  if (is.null(person_parameters)) {
    return(NULL)
  }
  
  print("Person Parameters:")
  print(person_parameters)
  
  # Calculate model fit information
  logLik_val <- as.numeric(logLik(pcm_model))
  num_params <- length(coef(pcm_model))
  sample_size <- nrow(data)
  
  aic_val <- -2 * logLik_val + 2 * num_params
  bic_val <- -2 * logLik_val + log(sample_size) * num_params
  
  model_fit <- list(
    logLik = logLik_val[1],
    AIC = aic_val,
    BIC = bic_val
  )
  
  print("Model Fit Information:")
  print(model_fit)
  
  # Organize into a list for this model
  model_info <- list(
    item_difficulty = item_difficulty,
    person_parameters = person_parameters,
    model_fit = model_fit
  )
  
  return(model_info)
}

# Function to save PCM model information
save_pcm_info <- function(model_info, file_prefix) {
  if (is.null(model_info)) {
    message("Model information is NULL, skipping save.")
    return(NULL)
  }
  
  # Save item difficulty parameters to CSV
  item_difficulty_df <- data.frame(Item = names(model_info$item_difficulty), Difficulty = model_info$item_difficulty)
  print(paste("Saving item difficulty parameters to", paste0(file_prefix, "_item_difficulty.csv")))
  write.csv(item_difficulty_df, paste0(file_prefix, "_item_difficulty.csv"), row.names = FALSE)
  
  # Save person parameters to CSV
  print(paste("Saving person parameters to", paste0(file_prefix, "_person_parameters.csv")))
  write.csv(model_info$person_parameters, paste0(file_prefix, "_person_parameters.csv"), row.names = FALSE)
  
  # Save model fit information to a text file
  model_fit_path <- paste0(file_prefix, "_model_fit.txt")
  model_fit <- model_info$model_fit
  print(paste("Saving model fit information to", model_fit_path))
  writeLines(
    c(
      paste("Log-Likelihood:", model_fit$logLik),
      paste("AIC:", model_fit$AIC),
      paste("BIC:", model_fit$BIC)
    ),
    con = model_fit_path
  )
  
  cat("Model information saved to files with prefix", file_prefix, "\n")
}

# Extract PCM information for each model
pcm_info <- map(pcm_models, ~extract_pcm_info(.x, complete_efa))

# Function to explore PCM model and save Wright Map
explore_pcm_model <- function(pcm_model, construct, output_dir, data) {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Extract item parameters
  item_params <- tryCatch({
    coef(pcm_model, simplify = TRUE)
  }, warning = function(w) {
    message("Warning in coef(pcm_model, simplify = TRUE): ", conditionMessage(w))
    NULL
  }, error = function(e) {
    message("Error in coef(pcm_model, simplify = TRUE): ", conditionMessage(e))
    NULL
  })
  
  if (is.null(item_params)) {
    return(NULL)
  }
  
  print("Item Parameters:")
  print(item_params)
  
  # Extract person abilities (thetas)
  person_params <- tryCatch({
    person.parameter(pcm_model)$theta.table
  }, warning = function(w) {
    message("Warning in person.parameter(pcm_model): ", conditionMessage(w))
    NULL
  }, error = function(e) {
    message("Error in person.parameter(pcm_model): ", conditionMessage(e))
    NULL
  })
  
  if (is.null(person_params)) {
    return(NULL)
  }
  
  person_thetas <- person_params[, "Person Parameter"]
  print("Person Thetas:")
  print(person_thetas)
  
  # Save the Wright Map
  wright_map_file <- file.path(output_dir, paste0("wright_map_", construct, ".png"))
  png(wright_map_file)
  wrightMap(thetas = person_thetas, itemSide = item_params)
  dev.off()
  
  # Additional exploration can be added here (e.g., summary, ICC plots)
  model_info <- extract_pcm_info(pcm_model, data)
  save_pcm_info(model_info, file.path(output_dir, construct))
}

# Function to run and explore PCM model for each construct
run_and_explore_pcm <- function(data, construct, output_dir) {
  pcm_model <- run_pcm_model(sample_n(data, 1000), construct)
  explore_pcm_model(pcm_model, construct, output_dir, data)
}

# Example usage for multiple constructs
constructs_to_analyze <- c("C21", "C22", "C23", "C24", "C25", "C26")
output_directory <- "analysis/"

walk(constructs_to_analyze, ~run_and_explore_pcm(complete_efa, .x, output_directory))
