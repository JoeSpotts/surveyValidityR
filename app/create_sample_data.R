library(fst)
library(MASS)  # For mvrnorm function to generate correlated data
library(tidyverse)

set.seed(223)  # For reproducibility

# Function to generate enhanced sample data with strong factor structure
generate_factor_structured_data <- function(n_respondents = 4000) {  # Increased sample size
  # Define constructs
  constructs <- tibble(
    constructID = c(8, 21, 23, 24, 25, 26, 31, 32, 33),
    constructName = c(
      "Jeffco Generations Vision", 
      "Standard 1: Welcoming all families into the school community",
      "Standard 3: Supporting student success",
      "Standard 4: Speaking up for every child",
      "Standard 5: Sharing power",
      "Standard 6: Collaborating with community",
      "Non-Standard: Technology Integration",
      "Non-Standard: School Safety",
      "Non-Standard: Remote Learning"
    ),
    # Increase factor strengths
    factor_strength = c(0.9, 0.92, 0.88, 0.85, 0.82, 0.87, 0.9, 0.85, 0.8),
    # Reduce cross-loadings
    cross_loading = c(0.1, 0.12, 0.15, 0.14, 0.16, 0.13, 0.1, 0.12, 0.15)
  )
  # Define additional "Standard" flag to simulate filtering
  constructs <- constructs %>%
    mutate(isStandard = ifelse(grepl("Standard", constructName), TRUE, FALSE))
  
  # Define questions (5 per construct)
  questions <- tibble(
    constructID = rep(constructs$constructID, each = 5),
    qid = 1:45,
    rCode = paste0("C", rep(constructs$constructID, each = 5), "_Q", rep(1:5, times = 9)),
    qText = paste("Sample question", 1:45, "for construct", rep(constructs$constructID, each = 5))
  )
  
  # Generate respondent IDs
  respondent_ids <- 1:n_respondents
  
  # Create demographic variables with specific distributions
  person_data <- tibble(
    respondentID = respondent_ids
  )
  
  # Add grade level variables (correlated)
  person_data <- person_data %>%
    mutate(
      # First determine the basic educational level type (randomly)
      ed_level_type = sample(1:3, n_respondents, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
      
      # Then derive specific grade flags based on that
      GradePreK = ifelse(ed_level_type == 1, 1, NA),
      GradeK2 = ifelse(ed_level_type == 1, sample(c(1, NA), n_respondents, replace = TRUE, prob = c(0.7, 0.3)), NA),
      Grade35 = ifelse(ed_level_type == 2, sample(c(1, NA), n_respondents, replace = TRUE, prob = c(0.8, 0.2)), NA),
      Grade68 = ifelse(ed_level_type == 2, sample(c(1, NA), n_respondents, replace = TRUE, prob = c(0.6, 0.4)), NA),
      Grade912 = ifelse(ed_level_type == 3, 1, NA),
      
      # Add simplified grade_level_bin for your requested demo
      grade_level_bin = case_when(
        !is.na(GradePreK) | !is.na(GradeK2) | !is.na(Grade35) ~ "0", # Lower school
        !is.na(Grade68) | !is.na(Grade912) ~ "1", # Upper school
        TRUE ~ NA_character_
      )
    )
  
  # Now add other demographic variables with meaningful distributions
  person_data <- person_data %>%
    mutate(
      # Add FRL status (more likely in certain grades)
      frl_score = case_when(
        !is.na(GradePreK) ~ rnorm(n(), mean = 0.4, sd = 0.2),
        !is.na(GradeK2) ~ rnorm(n(), mean = 0.35, sd = 0.2),
        !is.na(Grade35) ~ rnorm(n(), mean = 0.3, sd = 0.2),
        !is.na(Grade68) ~ rnorm(n(), mean = 0.25, sd = 0.2),
        !is.na(Grade912) ~ rnorm(n(), mean = 0.2, sd = 0.2),
        TRUE ~ rnorm(n(), mean = 0.3, sd = 0.2)
      ),
      frl_bin = ifelse(frl_score > 0.5, "1", "0"),
      
      # Add race info (categorical with distributions)
      race_score = rnorm(n(), mean = 0.5, sd = 0.2),
      race_bin = ifelse(race_score > 0.7, "1", "0"), # 1 = White, 0 = Of Color or Hispanic
      
      # Add special program flags
      iep_bin = sample(c("1", "0", NA), n_respondents, replace = TRUE, prob = c(0.15, 0.8, 0.05)),
      gt_bin = sample(c("1", "0", NA), n_respondents, replace = TRUE, prob = c(0.1, 0.85, 0.05)),
      ell_bin = sample(c("1", "0", NA), n_respondents, replace = TRUE, prob = c(0.12, 0.83, 0.05))
    )
  
  # Now the key part - generate responses with strong factor structure
  # We'll create latent factors for each construct, then derive item responses from these
  
  # First, set up correlation matrix for factors
  # Some factors will be more correlated with others
  n_constructs <- nrow(constructs)
  factor_corr_matrix <- diag(n_constructs)  # Start with identity matrix
  
  # Add correlations between factors
  for (i in 1:(n_constructs-1)) {
    for (j in (i+1):n_constructs) {
      # Base correlation - higher for related constructs
      base_corr <- 0.2
      
      # Increase correlation for related constructs
      if ((constructs$isStandard[i] && constructs$isStandard[j]) ||
          (i %in% c(7, 8, 9) && j %in% c(7, 8, 9))) {
        base_corr <- 0.4
      }
      
      # Adjust based on cross-loading parameters
      final_corr <- base_corr * (constructs$cross_loading[i] + constructs$cross_loading[j]) / 2
      
      # Ensure correlation is valid
      final_corr <- min(0.95, max(0.05, final_corr))
      
      # Set symmetric correlation
      factor_corr_matrix[i, j] <- final_corr
      factor_corr_matrix[j, i] <- final_corr
    }
  }
  
  # Generate latent factor scores for each respondent
  # These represent the "true" construct scores
  latent_factors <- MASS::mvrnorm(n_respondents, 
                                  mu = rep(0, n_constructs), 
                                  Sigma = factor_corr_matrix,
                                  empirical = TRUE)
  
  # Add demographic effects to latent factor scores
  # This creates systematic differences in scores by demographic group
  
  latent_factors_with_demo <- matrix(0, nrow = n_respondents, ncol = n_constructs)
  
  for (r in 1:n_respondents) {
    for (c in 1:n_constructs) {
      # Start with base latent factor score
      score <- latent_factors[r, c]
      
      # Add demographic effects
      
      # Grade level effects
      if (!is.na(person_data$GradePreK[r])) score <- score + 0.4
      if (!is.na(person_data$GradeK2[r])) score <- score + 0.3
      if (!is.na(person_data$Grade35[r])) score <- score + 0.2
      if (!is.na(person_data$Grade68[r])) score <- score - 0.1
      if (!is.na(person_data$Grade912[r])) score <- score - 0.3
      
      # FRL effect varies by construct
      if (person_data$frl_bin[r] == "1") {
        if (c %in% c(2, 3)) score <- score - 0.4  # Stronger negative for Standards 1 & 3
        else if (c %in% c(5, 6)) score <- score - 0.2  # Moderate for Standards 5 & 6
        else score <- score - 0.1  # Slight negative for other constructs
      }
      

      
      # Race effect
      if (person_data$race_bin[r] == "0") {  # Non-white
        if (c == 2) score <- score - 0.3  # Negative on welcoming
        else if (c == 4) score <- score - 0.4  # Negative on advocacy
        else if (c %in% c(5, 6)) score <- score - 0.2  # Negative on power, community
        else score <- score - 0.1  # Slight negative otherwise
      }
      
      latent_factors_with_demo[r, c] <- score
    }
  }
  
  # Now generate item responses from latent factors
  # Each item will load primarily on its own factor, with small cross-loadings
  
  # First, create a template for all responses
  response_template <- expand_grid(
    respondentID = respondent_ids,
    qid = 1:45
  ) %>%
    left_join(questions, by = "qid") %>%
    left_join(constructs, by = "constructID")
  
  # Add the latent factor values for each respondent
  response_data <- response_template %>%
    mutate(row_id = 1:n())  # Add a row identifier
  
  # Generate item responses based on latent factors with high factor loadings
  response_data$response_latent <- numeric(nrow(response_data))
  
  for (i in 1:nrow(response_data)) {
    # Get respondent and construct info
    resp_id <- response_data$respondentID[i]
    q_id <- response_data$qid[i]
    construct_id <- response_data$constructID[i]
    construct_index <- which(constructs$constructID == construct_id)
    
    # Increase primary loading on the item's own factor
    primary_loading <- constructs$factor_strength[construct_index]
    
    # Question-specific loading variation (some questions load better than others)
    q_variation <- (q_id %% 5) / 20  # Lower variation (0.0, 0.05, 0.1, 0.15, 0.2)
    
    # Combine for final primary loading (capped at 0.97)
    final_primary <- min(0.97, primary_loading + q_variation)
    
    # Random error component
    error_weight <- sqrt(1 - final_primary^2)*0.6
    
    # Calculate latent response from factor score (main factor) plus error
    main_factor_score <- latent_factors_with_demo[resp_id, construct_index]
    error_component <- rnorm(1, 0, 1)
    
    # Generate latent response as weighted combination of factor and error
    response_data$response_latent[i] <- final_primary * main_factor_score + error_weight * error_component
  }
  
  # Transform latent responses to 1-4 scale
  response_data <- response_data %>%
    mutate(
      # Transform to 1-4 scale
      response_value = pmin(4, pmax(1, round(2.5 + 0.8 * response_latent))),
      
      # Convert to character responses
      response = case_when(
        response_value == 1 ~ "Strongly Disagree",
        response_value == 2 ~ "Disagree",
        response_value == 3 ~ "Agree",
        response_value == 4 ~ "Strongly Agree",
        TRUE ~ NA_character_
      )
    )
  
  # Join with demographic data
  response_data <- response_data %>%
    left_join(person_data, by = "respondentID")
  
  # Add other required columns based on the original data structure
  response_data <- response_data %>%
    mutate(
      # Add required columns from original data
      qOrder = seq_len(nrow(questions))[match(qid, questions$qid)],
      displayNumber = ceiling(qOrder / 5),
      responseType = "selected",
      selectionType = "scale",
      demographicQ = NA_real_,
      reverseCode = ifelse(qid %% 7 == 0, 1, NA_real_),
      qShortText = paste("Short", rCode),
      qStem = "Please choose the response that best describes how you feel",
      qStemModifier = NA_character_,
      qFullText = paste(qText, ":", qStem),
      surveyTypeName = "FSP",
      EndYear = 2022,  # Match expected year filter
      surveyVersion = NA_character_,
      surveyWindow = NA_character_,
      surveyRequired = FALSE,
      anonymous = TRUE,
      surveyTypeID = 7,
      surveyFullName = "Fsp Survey - 2022 Sample",
      coreConcept = qShortText,
      questionType = NA_character_,
      displayedOnReports = NA_real_,
      displayNumberSub = NA_character_,
      EmployeeClassification = NA_character_,
      ShortQuestion = NA_character_,
      surveyID = 20220301,
      CDESchoolNumber = "0000",
      optionOrder = response_value,
      optionValence = 1,
      list_list_label = NA_character_,
      qShortText_opt = response,
      posFlag = 1
    )
  
  # Add some missing data (5%)
  n_missing <- round(0.001 * nrow(response_data))
  missing_indices <- sample(1:nrow(response_data), size = n_missing, replace = FALSE)
  response_data$response_value[missing_indices] <- NA
  response_data$response[missing_indices] <- NA
  response_data$optionOrder[missing_indices] <- NA
  
  # Return final data frame with columns in the expected order
  final_data <- response_data %>%
    dplyr::select(
      respondentID, rCode, qid, constructID, constructName, isStandard,
      response_value, response, optionOrder,
      qText, qShortText, qStem, qStemModifier, qFullText,
      qOrder, displayNumber, responseType, selectionType, 
      demographicQ, reverseCode, surveyTypeName, EndYear,
      surveyVersion, surveyWindow, surveyRequired, anonymous,
      surveyTypeID, surveyFullName, coreConcept, questionType,
      displayedOnReports, displayNumberSub, EmployeeClassification,
      ShortQuestion, surveyID, CDESchoolNumber, optionValence,
      list_list_label, qShortText_opt, posFlag,
      grade_level_bin, frl_bin, race_bin, iep_bin, gt_bin, ell_bin,
      GradePreK, GradeK2, Grade35, Grade68, Grade912
    )
  
  return(final_data)
}

# Generate sample data
sample_data <- generate_factor_structured_data()

# Save as FST file (compatible with your app)
write_fst(sample_data, "FSP_surveyData_sample.fst")

# Preview the data
cat("Sample data created with", length(unique(sample_data$respondentID)), "respondents and", 
    length(unique(sample_data$rCode)), "questions\n")

# Show a summary of the construct distribution
constructs_summary <- sample_data %>%
  dplyr::select(constructName, isStandard) %>%
  distinct() %>%
  arrange(isStandard, constructName)

print(constructs_summary)

# Show demographic data summary
demographic_summary <- sample_data %>%
  dplyr::select(respondentID, grade_level_bin, frl_bin, race_bin, iep_bin, gt_bin, ell_bin) %>%
  distinct() %>%
  summarise(
    total_respondents = n(),
    lower_school_pct = mean(grade_level_bin == "0", na.rm = TRUE) * 100,
    upper_school_pct = mean(grade_level_bin == "1", na.rm = TRUE) * 100,
    frl_pct = mean(frl_bin == "1", na.rm = TRUE) * 100,
    non_white_pct = mean(race_bin == "0", na.rm = TRUE) * 100,
    iep_pct = mean(iep_bin == "1", na.rm = TRUE) * 100,
    gt_pct = mean(gt_bin == "1", na.rm = TRUE) * 100,
    ell_pct = mean(ell_bin == "1", na.rm = TRUE) * 100
  )

print(demographic_summary)

# Now let's verify the factor structure by running a simple correlation analysis
# This will show us if the items within each construct correlate well
factor_verification <- function() {
  cat("Analyzing item correlations to verify factor structure...\n")
  
  # Calculate average correlation within each construct
  construct_ids <- unique(sample_data$constructID)
  
  for (c_id in construct_ids) {
    # Get all items for this construct
    construct_items <- sample_data %>%
      filter(constructID == c_id) %>%
      dplyr::select(respondentID, rCode, response_value)
    
    # Get unique rCodes
    rcodes <- unique(construct_items$rCode)
    
    # Reshape to wide format for correlation calculation
    wide_data <- construct_items %>%
      pivot_wider(id_cols = respondentID, 
                  names_from = rCode, 
                  values_from = response_value) %>%
      dplyr::select(-respondentID)
    
    # Calculate correlation matrix
    cor_matrix <- cor(wide_data, use = "pairwise.complete.obs")
    
    # Calculate average correlation (excluding self-correlations)
    n_items <- ncol(wide_data)
    total_corr <- sum(cor_matrix) - n_items  # Subtract diagonal (self-correlations)
    avg_corr <- total_corr / (n_items^2 - n_items)
    
    # Get construct name
    construct_name <- sample_data %>%
      filter(constructID == c_id) %>%
      pull(constructName) %>%
      unique()
    
    cat(sprintf("Construct %d (%s): Average inter-item correlation = %.3f\n", 
                c_id, construct_name, avg_corr))
  }
  
  # Now calculate cross-construct correlations
  cat("\nAnalyzing across-construct correlations...\n")
  
  # First get average response by construct and respondent
  construct_scores <- sample_data %>%
    group_by(respondentID, constructID, constructName) %>%
    summarise(avg_score = mean(response_value, na.rm = TRUE), .groups = "drop")
  
  # Reshape to wide format
  construct_scores_wide <- construct_scores %>%
    pivot_wider(id_cols = respondentID,
                names_from = constructName, 
                values_from = avg_score)
  
  # Calculate correlation matrix
  construct_cor <- cor(construct_scores_wide[,-1], use = "pairwise.complete.obs")
  
  # Print cross-construct correlation matrix
  cat("Cross-construct correlation matrix:\n")
  print(round(construct_cor, 3))
}

# Run factor verification
factor_verification()

detach("package:MASS", unload = TRUE)

