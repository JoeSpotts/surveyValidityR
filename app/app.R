library(shiny)
library(fst)
library(tidyverse)

# Source your support functions and modules
source("modules/dataPrep.R")          # Contains dataPrepUI, dataPrepServer
source("prepare_survey_data.R")       # Contains prepare_survey_data function
source("modules/descriptives.R")      # Contains descriptivesUI, descriptivesServer
source("modules/factorAnalysis.R")    # Contains reliabilityFactorUI, reliabilityFactorServer
source("modules/cfaAnalysis.R")       # Contains cfaAnalysisUI, cfaAnalysisServer
source("modules/raschAnalysis.R")     # Contains raschAnalysisUI, raschAnalysisServer
source("modules/cfaInvariance.R")     # Contains cfaInvarianceUI, cfaInvarianceServer

# Create a function to set up demographic metadata
create_demo_metadata <- function() {
  # Initialize the dataframe with column info for demographics
  demo_metadata <- data.frame(
    column = c("grade_level_bin", "frl_bin", "race_bin", "iep_bin", "gt_bin", "ell_bin"),
    display_name = c("Grade Level", "Free/Reduced Lunch Status", "Race/Ethnicity", 
                     "IEP Status", "Gifted/Talented", "English Language Learner"),
    type = rep("binary", 6),
    na_label = c("Grade Not Specified", "FRL Status Not Specified", "Race Not Specified",
                 "IEP Status Not Specified", "GT Status Not Specified", "ELL Status Not Specified"),
    stringsAsFactors = FALSE
  )
  
  # Create value labels for binary variables
  grade_labels <- c("Upper School", "Lower School")
  names(grade_labels) <- c("1", "0")
  
  frl_labels <- c("FRL", "Not FRL")
  names(frl_labels) <- c("1", "0")
  
  race_labels <- c("Of Color or Hispanic", "White")
  names(race_labels) <- c("0", "1")
  
  iep_labels <- c("IEP", "No IEP")
  names(iep_labels) <- c("1", "0")
  
  gt_labels <- c("GT", "Not GT")
  names(gt_labels) <- c("1", "0")
  
  ell_labels <- c("ELL", "Not ELL")
  names(ell_labels) <- c("1", "0")
  
  # Create a list of value labels
  value_labels <- list(
    list(grade_labels),
    list(frl_labels),
    list(race_labels),
    list(iep_labels),
    list(gt_labels),
    list(ell_labels)
  )
  
  # Add the value_labels to the demo_metadata
  demo_metadata$value_labels <- value_labels
  
  return(demo_metadata)
}

# Global data load - use sample data
tryCatch({
  surveyData <- fst::read.fst("FSP_surveyData_sample.fst")
  
  # Debugging - print column names to verify data structure
  cat("Loaded sample data with columns:", paste(names(surveyData), collapse=", "), "\n")
  
  # Verify essential columns exist
  required_cols <- c("respondentID", "constructID", "constructName", "rCode", "response_value")
  missing_cols <- setdiff(required_cols, names(surveyData))
  if(length(missing_cols) > 0) {
    warning("Survey data is missing these required columns: ", 
            paste(missing_cols, collapse=", "))
  }
  
}, error = function(e) {
  # Fallback to a minimal dataset if the sample file doesn't exist
  warning("Could not load sample data: ", e$message, 
          "\nCreating minimal test dataset instead.")
  
  # Create a minimal test dataset
  surveyData <- data.frame(
    respondentID = rep(1:10, each=5),
    rCode = rep(paste0("Q", 1:5), times=10),
    response_value = sample(1:4, 50, replace=TRUE),
    constructID = 1,
    constructName = "Test Construct",
    EndYear = 2022,
    optionOrder = sample(1:4, 50, replace=TRUE),
    reverseCode = NA
  )
})

# Create demographic metadata
demo_metadata <- create_demo_metadata()

# Define UI
ui <- fluidPage(
  titlePanel("Survey Data Analysis Application"),
  tabsetPanel(
    tabPanel("Data Preparation",
             dataPrepUI("myDataPrepModule"),
             br(),
             verbatimTextOutput("previewData")
    ),
    tabPanel("Descriptives Analysis",
             descriptivesUI("myDescriptivesModule")
    ),
    tabPanel("Reliability & Factor Analysis",
             reliabilityFactorUI("relFactMod")
    ),
    tabPanel("Confirmatory Factor Analysis",
             cfaAnalysisUI("cfaMod")
    ),
    tabPanel("Rasch Analysis",
             raschAnalysisUI("raschMod")
    ),
    tabPanel("Measurement Invariance",
             cfaInvarianceUI("invMod")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Store CFA models for sharing with invariance module
  cfa_models <- reactiveVal(NULL)
  
  # Data preparation module - using directly without callModule
  preparedData <- dataPrepServer("myDataPrepModule")
  
  # Set up debugging observer for the prepared data
  observe({
    req(preparedData())
    cat("Data after preparation - Rows:", nrow(preparedData()), 
        "Cols:", ncol(preparedData()), "\n")
    cat("Columns:", paste(names(preparedData()), collapse=", "), "\n")
  })
  
  output$previewData <- renderPrint({
    req(preparedData())
    head(preparedData())
  })
  
  # Descriptives module
  descriptivesServer("myDescriptivesModule", adjustedData = preparedData)
  
  # Reliability & Factor Analysis module
  reliabilityFactorServer("relFactMod", adjustedData = preparedData)
  
  # Store CFA models for sharing with invariance module
  cfa_models <- reactiveVal(NULL)
  
  # CFA module with callback to store models
  cfaResults <- cfaAnalysisServer("cfaMod", adjustedData = preparedData)
  
  # Observer to capture CFA models for use in invariance testing
  observe({
    # Debug output
    res <- cfaResults()
    cat("CFA results received, type:", class(res)[1], "\n")
    
    # Only store valid model objects
    if(!is.null(res) && !is.character(res) && any(class(res) %in% c("lavaan", "lavaan.mi"))) {
      # Store the models
      cfa_models(res)
    } else {
      cat("Invalid CFA result type, not storing:", typeof(res), "\n")
    }
  })
  
  # Rasch Analysis module
  raschAnalysisServer("raschMod", adjustedData = preparedData)
  
  # CFA Invariance module
  cfaInvarianceServer("invMod", 
                      adjustedData = preparedData, 
                      cfaModelData = cfa_models)
}

# Run the app
shinyApp(ui, server)