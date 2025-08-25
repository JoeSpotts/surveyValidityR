library(fst)
library(data.table)
library(tidyverse)
library(lavaan)

#' Read and Assign Data Files Based on Prefix
#'
#' This function reads all files in a specified directory, extracting the prefix before an underscore in each filename, and assigns the contents to variables named after these prefixes in the global environment.
#' 
#' 
#' @param folder_path The path to the folder containing the data files.
#' @importFrom readr fread
#' @importFrom purrr map set_names
#' @importFrom stringr str_extract
#' @examples
#' read_and_assign_files("path/to/data")
#' @export
#' Read and Assign Data Files Based on Prefix
#'
#' This function reads all files in a specified directory, extracting the prefix before an underscore in each filename, and assigns the contents to variables named after these prefixes in the global environment. It handles both single and multiple files in the directory.
#' 
#' @param folder_path The path to the folder containing the data files.
#' @importFrom readr fread
#' @importFrom purrr map set_names
#' @importFrom stringr str_extract
#' @examples
#' read_and_assign_files("path/to/data")
#' @export
read_and_assign_files <- function(folder_path) {
  files <- list.files(folder_path, full.names = TRUE)
  file_names <- set_names(files, str_extract(basename(files), "^[^_]*"))
  
  if (length(files) == 1) {
    assign(names(file_names), fread(files), envir = .GlobalEnv)
  } else {
    map2(files, names(file_names), ~assign(.y, fread(.x), envir = .GlobalEnv))
  }
}


surveyData <- read.fst("data/FSP_surveyData2024-03-01.fst")
###Pull in data from the database.
# surveyData <- allResponses
####Create Construct Lookup Table####
qConstructLookup <- surveyData %>% 
  filter(EndYear == 2022,
         str_detect(constructName, "Standard")) %>% 
  distinct(constructName, constructID, qText, EndYear, qid, rCode) %>% 
  mutate(qid_O = paste0("C", constructID, "_Q", qid))


cToQ_lookup <- qConstructLookup %>% 
  select(qid_O, constructID, qid)



textJoiner <- qConstructLookup %>% 
  mutate(Item = row_number(),
         Standard = constructName) 

# write.csv(qConstructLookup, "constructLookupFSP.csv")

# apa_df <- read.csv("constructLookupFSP.csv")
# 
# knitr::kable(apa_df, format = "markdown")
#  print(knitr::kable(apa_df, format = "markdown"))
# devtools::install_github("crsh/papaja")
# library(papaja)
# apa_table <- apa_table(qConstructLookup, align = "lcccc", caption = "Questions by Standard and Year", note = "Note: 'X' indicates the presence of a question in the specified year.")
# 
# # To view the table in R Markdown, print using results = 'asis'
# print(apa_table, results = 'asis')


# library(lavaan)
# 
# library(tidyverse)
# library(data.table)
# library(lavaan)
# 
# # Step 1: Adjust data and keep constructID for model specification
# data_adjusted <- surveyData %>%
#   filter(str_detect(constructName, "Standard"), EndYear == 2022) %>% 
#   mutate(response_value = ifelse(is.na(reverseCode), optionOrder, max(optionOrder, na.rm = TRUE) + 1 - optionOrder),
#          qid = paste0("C", as.character(constructID), "_Q", as.character(qid))) %>%
#   select(respondentID, constructID, qid, response_value)


library(dplyr)
library(stringr)

#' Prepare Survey Data for Analysis
#'
#' This function prepares survey data for analysis by filtering based on construct name, 
#' selecting specific end years, adjusting response values, and optionally including 
#' demographic variables.
#'
#' @param surveyData A dataframe containing the survey data.
#' @param endYears A vector of integers representing the end years to filter the data on. 
#'        Use NULL to select all years.
#' @param includeDemos A boolean indicating whether to include demographic variables in the output.
#' @param constructNameFilter A string or regular expression to filter constructs by name.
#' @return A dataframe adjusted for analysis, with or without demographic variables, filtered 
#'         by end years and construct names.
#' @examples
#' prepared_data <- prepare_survey_data(surveyData, endYears = c(2022), includeDemos = TRUE,
#'                                      constructNameFilter = "Standard")
#' @export
prepare_survey_data <- function(surveyData, endYears = NULL, includeDemos = FALSE, constructNameFilter = "Standard") {
  # Filter by constructName and optionally by EndYear
  data_filtered <- surveyData %>%
    filter(str_detect(constructName, constructNameFilter),
           if (!is.null(endYears)) EndYear %in% endYears else TRUE)
  
  # Adjust response values
  data_adjusted <- data_filtered %>%
    mutate(response_value = ifelse(is.na(reverseCode), optionOrder, max(optionOrder, na.rm = TRUE) + 1 - optionOrder),
           constructID = paste0("C", as.character(constructID)),
           rCode = paste0(constructID, rCode),
           qid = paste0(constructID, "_Q", as.character(qid)))
  
  # Select columns based on includeDemos
  cols_to_select <- c("respondentID", "qid", "rCode", "response_value", "constructID")
  if (includeDemos) {
    demo_vars <- c("GradePreK", "GradeK2", "Grade35", "Grade68","Grade912", "race_bin", "iep_bin", "gt_bin", "ell_bin", "frl_bin")
    cols_to_select <- c(cols_to_select, demo_vars)
  }
  
  # Select the necessary columns
  data_selected <- select(data_adjusted, one_of(cols_to_select))
  
  return(data_selected)
}


data_adjusted <- prepare_survey_data(surveyData, endYears = 2022, includeDemos = T, constructNameFilter = "Standard") 


# write.csv(data_adjusted, "data_adjusted.csv")

data_adjusted <- read.csv("data_adjusted.csv")
# Generate model specifications using constructID
model_specs <- data_adjusted %>%
  select(constructID, rCode) %>%
  distinct() %>%
  group_by(constructID) %>%
  summarise(indicators = paste(rCode, collapse = " + ")) %>%
  ungroup() %>%
  mutate(construct_label = paste0("F", constructID),  # Creating a label for each factor
         model_line = paste(construct_label, "=~", indicators)) %>%
  pull(model_line)

# Combine all model specifications into a single model string
cfa_model <- paste(model_specs, collapse = "\n")

# Step 2: Drop constructID before pivoting
data_wide <- data_adjusted %>%
  select(respondentID, rCode, response_value) %>%
  pivot_wider(names_from = rCode, values_from = response_value) %>% 
  select(-respondentID)
# Install and load necessary packages
###MISSINGNESS Analysis
library(Amelia)
library(mice)
library(VIM)


# Basic summary of missing data
summaryStats <- summary(data_wide)
print(summaryStats)

# Count of missing values per column
missing <- as.data.frame(colSums(is.na(data_wide))) %>%
  rownames_to_column(var = "rCode") %>%
  rename(missing_count = `colSums(is.na(data_wide))`)

# Assuming textJoiner is a data frame with a column to join on
missing_joined <- missing %>%
  left_join(textJoiner, by = "rCode")

# View missingness table
print(missing_joined)
# # Step 8: Create a graph to display missingness by question
# missingness_chart <- missing_joined %>%
#   ggplot(aes(x = reorder(qText, -missing_count), y = missing_count, fill = Standard)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   labs(
#     title = "Missingness by Question",
#     x = "Question",
#     y = "Count of Missing Values"
#   ) +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 8, hjust = 1))
# 
# # View missingness chart
# print(missingness_chart)
# 
# # Pattern of missing data using md.pattern from mice
# md.pattern(data_wide)
# 
# # Aggregated visualization using aggr from VIM
# aggr_plot <- aggr(data_wide, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE, labels = names(data_wide), cex.axis = .7, gap = 3, ylab = c("Missing data", "Pattern"))
# 
# 
# summary(missing)
# 
# library(RcmdrMisc)
# 
# correlation <- rcorr.adjust(data_wide) # This function is build into R Commander.



# install.packages("psych", dependencies = TRUE)
###
# Set seed for reproducibility
set.seed(867)

# Calculate the number of rows in your dataset
total_rows <- nrow(data_wide)

# Define the split proportions
split_proportion <- 0.5  # 50% for each segment

# Create a vector of random indices
indices <- sample(1:total_rows, size = total_rows * split_proportion, replace = FALSE)

# Split the data into two segments
data_efa <- data_wide[indices, ]
data_cfa <- data_wide[-indices, ]

library(mice)
imputed_data <- mice(data_efa, m = 5, method = 'pmm', seed = 867)

# Extract one of the imputed datasets for EFA
complete_efa <- complete(imputed_data, 1)
library(psych)
KMO(complete_efa)

###Run Bartlett's test for sphericity
cortest.bartlett(complete_efa)


ev <- eigen(cor(complete_efa)) # get eigenvalues
ev$values

scree(complete_efa, pc=FALSE)  # Use pc=FALSE for factor analysis

print(scree)


fa.parallel(complete_efa, fa="fa")



Nfacs <- 1  # This is for four factors. You can change this as needed.

fit <- factanal(complete_efa, Nfacs, rotation="promax")


print(fit, digits=2, cutoff=0.3, sort=TRUE)

# Visualizing the factor loadings
library(GPArotation)

fit <- factanal(complete_efa, factors = Nfacs, rotation = "promax")
print(fit, digits = 2, cutoff = 0.3, sort = TRUE)

# Visualizing the factor loadings
library(factoextra)
library(FactoMineR)

fa_loadings <- as.data.frame(fit$loadings[, 1:Nfacs])

# Get the variable names
rownames(fa_loadings) <- colnames(complete_efa)


efa_fit <- PCA(complete_efa, graph = FALSE)
# Visualize the factor loadings (example plot, you can customize further)
fviz_eig(efa_fit)  # Eigenvalues
fviz_eig(efa_fit, addlabels = TRUE, ylim = c(0, 50))


loads <- fit$loadings

fa.diagram(loads, box = F)
fviz_pca_var(efa_fit, repel = TRUE, labelsize = 5, label = "var", title = "Factor Contributions")
fviz_pca_var(efa_fit, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, title = "Factor Loadings Plot")

fa.diagram(loads, main = "Factor Structure", size = 0.7, digits = 2)
##
###
#
#CFA LAND####
##
#
## If you want to run this before eliminating missing values use: 
library(mice)
imputed_data_cfa <- mice(data_cfa, m = 5, method = 'pmm', seed = 867)

# Extract one of the imputed datasets for EFA
complete_cfa <- complete(imputed_data_cfa, 1)

write.csv(complete_cfa, "complete_cfa.csv")
# Combine all model specifications into a single model string

complete_cfa <- read.csv("complete_cfa.csv")
cfa_model <- paste(model_specs, collapse = "\n")


#Step 3: create construct-item map for CFA
constructs_items <- surveyData %>%
  filter(EndYear == 2022) %>% 
  select(constructName, qid) %>%
  distinct() %>%
  group_by(constructName) %>%
  summarise(items = list(qid))


# Step 4: Run CFA
fit_cfa <- cfa(cfa_model, data = complete_cfa)
summary(fit_cfa, fit.measures = TRUE)





library(semPlot)

semPaths(fit_cfa, 
         what = "est", 
         layout = "circle", 
         edge.label.cex = 0.8,
         sizeMan = 4,
         sizeLat = 4,
         residuals = FALSE,
         color = list(lat = "lightblue", man = "lightgreen"),
         edge.color = "darkgrey",
         edge.curved = 0.15,
         shapeMan = "rectangle",
         node.width = 1.5,
         node.height = 1.2,
         node.label.cex = 0.8,
         curvePivot = TRUE,
         edge.label.position = 0.65,       # Split long labels into multiple lines to reduce overlap
         legend = FALSE
         
)
# Add a title to the plot
title("Six-Factor Structure Model", cex.main = 1.5, font.main = 2)

#Step 5: extract items that may warrant a second look

# Compute modification indices
mod_indices <- modificationIndices(fit_cfa)
# Filter high modification indices
high_mod_indices <- subset(mod_indices, mi > 10)
print(high_mod_indices)


# Examine standardized residuals
residuals <- residuals(fit_cfa, type = "standardized")
print(residuals)

library(semTools)
# Compute reliability for each factor
reliability_results <- reliability(fit_cfa)
print(reliability_results)

# Compute average variance extracted (AVE) for each factor
ave <- semTools::reliability(fit_cfa)
print(ave)


# Extract standardized loadings
standardized_loadings <- inspect(fit_cfa, "std")
print(standardized_loadings)


###RASCH#####

library(eRm)
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

#####CREATES A LIST OF CONSTRUCT Metrics####
pcm_models <- map(constructs, ~run_pcm_model(sample_n(complete_efa, 100), .x))
names(pcm_models) <- constructs


# Extract residuals
residuals <- residuals(pcm_models[1], type = "matrix")

# Perform PCA on residuals
pca_res <- prcomp(residuals)
summary(pca_res)

library(eRm)
library(dplyr)
library(purrr)


extract_pcm_info <- function(pcm_model) {
  # Extract item difficulty parameters (eta) and their standard errors
  item_difficulty <- summary(pcm_model)$itempar
  
  # Extract person parameters (thetas) if available
  if(!is.null(pcm_model$person)) {
    person_parameters <- summary(pcm_model)$person
  } else {
    person_parameters <- NULL
  }
  
  # Extract model fit information
  model_fit <- list(
    logLik = logLik(pcm_model),
    AIC = AIC(pcm_model),
    BIC = BIC(pcm_model)
  )
  
  # Organize into a list for this model
  model_info <- list(
    item_difficulty = item_difficulty,
    person_parameters = person_parameters,
    model_fit = model_fit
  )
  
  return(model_info)
}



pcm_info <- map(pcm_models, extract_pcm_info)

library(WrightMap)

explore_pcm_model <- function(pcm_model, construct, output_dir) {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Extract item parameters
  item_params <- coef(pcm_model, simplify = TRUE)
  
  # Extract person abilities (thetas) - Adjusted to access the correct structure
  person_params <- pcm_model$etapar
  
  # Check if there are named elements and convert to a vector if needed
  if(is.list(person_params)) {
    if(all(sapply(person_params, is.numeric))) {
      person_params <- unlist(person_params)
    }
  }
  
  # Save the Wright Map - Adjusting the function call to match WrightMap's expectations
  wright_map_file <- file.path(output_dir, paste0("wright_map_", construct, ".png"))
  png(wright_map_file)
  wrightMap(thetas = person_params, itemSide = item_params)
  dev.off()
  
  # Additional exploration can be added here (e.g., summary, ICC plots)
  model_info <- extract_pcm_info(pcm_model)
  save_pcm_info(model_info, "analysis/pcm_model_output")
}

# Example usage, replace "C21" and "analysis/" with your actual construct and output directory
# explore_pcm_model(pcm_model, "C21", "analysis/")

run_and_explore_pcm <- function(data, construct, output_dir) {
  pcm_model <- run_pcm_model(sample_n(data, 1000), construct)
  explore_pcm_model(pcm_model, construct, output_dir)
}
library(WrightMap)
run_and_explore_pcm(complete_efa, "C21", "analysis/")

run_and_explore_pcm(complete_efa, "C22", "analysis/")
run_and_explore_pcm(complete_efa, "C23", "analysis/")
run_and_explore_pcm(complete_efa, "C24", "analysis/")
run_and_explore_pcm(complete_efa, "C25", "analysis/")
run_and_explore_pcm(complete_efa, "C26", "analysis/")







####CFA FOR INVARIANCE#####
demoData <- prepare_survey_data(surveyData, endYears = 2022, includeDemos = T, constructNameFilter = "Standard") 


#####Create a lookuptable for questions and constructs####
cToQ_lookup <- demoData %>% distinct(constructID, qid) %>% 
  mutate(qid_O = paste())


#' Generate CFA Model Syntax from Lookup Table
#'
#' @param lookupTable A dataframe with construct IDs and associated question IDs.
#' @return A string containing the lavaan model syntax.
generate_model_syntax <- function(lookupTable) {
  # Group by constructID and concatenate item variables to form CFA model syntax for each construct
  model_list <- lookupTable %>%
    group_by(constructID) %>%
    summarise(items = paste(qid, collapse = " + ")) %>%
    ungroup() %>%
    mutate(model_syntax = paste(constructID, "=~", items)) %>%
    pull(model_syntax)
  
  # Collapse all construct models into one string separated by line breaks
  model_syntax <- paste(model_list, collapse = "\n")
  
  return(model_syntax)
}


modelSyntax <- generate_model_syntax(cToQ_lookup)


#' Test Whole Model Invariance Across a Demographic Variable
#'
#' @param data A dataframe with the survey data, including construct IDs, item IDs, and demographic variables.
#' @param modelSyntax The lavaan model syntax for the whole model CFA.
#' @param demoVar The name of the demographic variable to test for invariance.
#' @return A list containing fit indices for configural, metric, and scalar invariance models.
test_invariance_whole_model <- function(data, modelSyntax, demoVar) {

  # Remove rows where the demographic variable is NA
  data <- data %>% filter(!is.na(.[[demoVar]]))
  # Ensure the demographic variable is a factor
  data[[demoVar]] <- as.factor(data[[demoVar]])
  
  
  
  # Configural model - baseline model without constraints
  fit_configural <- cfa(modelSyntax, data = data, group = demoVar)
  
  # Metric invariance - factor loadings constrained to be equal
  fit_metric <- cfa(update(modelSyntax, group.equal = "loadings"), data = data, group = demoVar)
  
  # Scalar invariance - item intercepts constrained to be equal
  fit_scalar <- cfa(update(modelSyntax, group.equal = c("loadings", "intercepts")), data = data, group = demoVar)
  
  # Collect and return fit indices for each model
  results <- list(
    configural = fitMeasures(fit_configural),
    metric = fitMeasures(fit_metric),
    scalar = fitMeasures(fit_scalar)
  )
  
  return(results)
}

invariance_results <- test_invariance_whole_model(demoData, modelSyntax, "frl_bin")



###BUILDING DEMOGRAPHIC TABLE#####


demoTable <- surveyData %>% 
  select(respondentID, EndYear, GradePreK:frl_bin) %>% 
  distinct() 

demoTable <- surveyData %>%
  select(respondentID, EndYear, GradePreK:frl_bin) %>%
  distinct() %>%
  # Reshape data to long format for easier aggregation
  pivot_longer(cols = -c(respondentID, EndYear), names_to = "demoCol", values_to = "demo")


demoSummary <- demoTable %>%
  group_by(demoCol, demo, EndYear) %>%
  mutate(demoCount = n()) %>%
  group_by(demoCol, EndYear) %>%
  mutate(Total = n()) %>%
  mutate(Percent = (demoCount/Total)) %>% 
  filter(!is.na(demo)) %>% 
  select(-respondentID) %>% 
  distinct() %>% 
  arrange(EndYear,  demo)

write.csv(demoSummary, "demoSummary.csv")



apa_df <- read.csv("constructLookupFSP.csv")

knitr::kable(apa_df, format = "markdown")
knitr::kable(apa_df, format = "markdown")
print(knitr::kable(apa_df, format = "markdown"))

library(papaja)
apa_table <- apa_table(qConstructLookup, align = "lcccc", caption = "Questions by Standard and Year", note = "Note: 'X' indicates the presence of a question in the specified year.")

# To view the table in R Markdown, print using results = 'asis'
print(apa_table, results = 'html')


