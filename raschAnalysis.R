# modules/raschAnalysis.R

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)  # For factor analysis
library(eRm)    # For Rasch analysis
library(lattice)  # For ICC plot
library(WrightMap) # For Wright Map
library(DT)
library(formattable)
library(htmltools)
library(shinycssloaders)

# ---------------------- UI Function ------------------------------- #
# ---------------------- UI Function ------------------------------- #
raschAnalysisUI <- function(id, label = "Rasch Analysis") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             wellPanel(
               h3("Rasch Measurement Analysis"),
               p("Rasch analysis provides a framework for evaluating how well items measure a latent trait, 
                 placing both items and persons on the same interval scale. It offers insights into item difficulty, 
                 person ability, and the functioning of the measurement instrument."),
               
               # Tabbed interface for model setup
               tabsetPanel(
                 id = ns("model_setup_tabs"),
                 
                 # Tab 1: Model Type & Item Selection
                 tabPanel("Model Configuration",
                          div(class = "row",
                              div(class = "col-md-4",
                                  h4("Measurement Model"),
                                  radioButtons(ns("model_type"), "Select Model Type:",
                                               choices = c("Based on Survey Structure (By Construct)" = "by_construct",
                                                           "Unidimensional (All Items)" = "unidimensional",
                                                           "Custom Item Selection" = "custom"),
                                               selected = "by_construct"),
                                  conditionalPanel(
                                    condition = "input.model_type == 'by_construct'",
                                    ns = ns,
                                    selectInput(ns("selected_construct"), 
                                                "Select Construct to Analyze:",
                                                choices = NULL)
                                  ),
                                  conditionalPanel(
                                    condition = "input.model_type == 'custom'",
                                    ns = ns,
                                    p("Define your custom model by selecting which items to include:"),
                                    uiOutput(ns("itemSelectionUI"))
                                  ),
                                  radioButtons(ns("item_format"), "Item Format:",
                                               choices = c("Auto-detect" = "auto",
                                                           "Dichotomous (Binary)" = "dichotomous",
                                                           "Polytomous (Rating Scale)" = "polytomous"),
                                               selected = "auto")
                              ),
                              div(class = "col-md-8",
                                  h4("Dimensionality Testing"),
                                  p("Before running a Rasch model, it's important to check whether your data meets the assumption of unidimensionality."),
                                  actionButton(ns("run_dimensionality"), "Run Dimensionality Test", 
                                               class = "btn-info"),
                                  br(), br(),
                                  conditionalPanel(
                                    condition = "input.run_dimensionality > 0",
                                    ns = ns,
                                    uiOutput(ns("dimensionalityResults"))
                                  )
                              )
                          )
                 ),
                 
                 # Tab 2: Advanced Options
                 tabPanel("Advanced Options",
                          # First row of advanced options
                          div(class = "row",
                              div(class = "col-md-4",
                                  selectInput(ns("rm_method"), "Rasch Model Estimation Method:",
                                              choices = c("Conditional Maximum Likelihood (CML)" = "CML",
                                                          "Joint Maximum Likelihood (JML)" = "JML",
                                                          "Marginal Maximum Likelihood (MML)" = "MML"),
                                              selected = "CML")
                              ),
                              div(class = "col-md-4",
                                  numericInput(ns("icc_items"), "Number of ICC Plots to Show per Page:",
                                               value = 6, min = 1, max = 20, step = 1),
                                  checkboxInput(ns("show_confidence"), "Show Confidence Intervals in ICC Plots", TRUE)
                              ),
                              div(class = "col-md-4",
                                  selectInput(ns("fit_criteria"), "Fit Criteria Standard:",
                                              choices = c("Conservative (0.8-1.2)" = "conservative",
                                                          "Standard (0.7-1.3)" = "standard",
                                                          "Survey/Rating Scale (0.6-1.4)" = "survey",
                                                          "Exploratory/Lenient (0.5-1.7)" = "lenient",
                                                          "Custom" = "custom"),
                                              selected = "survey"),
                                  conditionalPanel(
                                    condition = "input.fit_criteria == 'custom'",
                                    ns = ns,
                                    numericInput(ns("misfit_cutoff"), "Custom Misfit Cutoff Value:",
                                                 value = 1.7, min = 1.0, max = 2.0, step = 0.1)
                                  ),
                                  p("For engagement surveys, the Survey/Rating Scale or Exploratory criteria are recommended due to the multidimensional nature of the construct.")
                              )
                          ),
                          # Add heading and spacing for the data handling section
                          div(class = "row",
                              div(class = "col-md-12",
                                  h4("Data Processing Options"),
                                  hr()
                              )
                          ),
                          # Second row of advanced options - data handling
                          div(class = "row",
                              div(class = "col-md-4",
                                  radioButtons(ns("missing_handling"), "Missing Data Handling:",
                                               choices = c("Remove incomplete cases" = "remove",
                                                           "Impute missing values" = "impute"),
                                               selected = "remove")
                              ),
                              div(class = "col-md-4",
                                  checkboxInput(ns("use_sample"), "Use random sample for faster processing", FALSE),
                                  conditionalPanel(
                                    condition = "input.use_sample == true",
                                    ns = ns,
                                    sliderInput(ns("sample_size"), "Sample Size (% of data):",
                                                min = 10, max = 100, value = 50, step = 5)
                                  )
                              ),
                              div(class = "col-md-4",
                                  numericInput(ns("impute_iterations"), "Imputation Iterations:",
                                               value = 5, min = 1, max = 20, step = 1),
                                  helpText("Only used if 'Impute missing values' is selected")
                              )
                          )
                 )
               ),
               
               div(class = "row", style = "margin-top: 15px;",
                   div(class = "col-md-12", 
                       div(style = "text-align: center;",
                           actionButton(ns("run_rasch"), "Run Rasch Analysis", 
                                        class = "btn-lg btn-primary"))
                   )
               ),
               
               div(class = "row", style = "margin-top: 10px;",
                   div(class = "col-md-12",
                       uiOutput(ns("raschStatus"))
                   )
               )
             )
      )
    ),
    
    conditionalPanel(
      condition = "input.run_rasch > 0",
      ns = ns,
      
      fluidRow(
        column(12,
               tabsetPanel(
                 id = ns("rasch_tabs"),
                 
                 # Tab 1: Model Summary
                 tabPanel("Model Summary",
                          wellPanel(
                            h3("Rasch Model Results"),
                            # Add data processing information section
                            div(class = "row",
                                div(class = "col-md-12",
                                    h4("Data Processing Information"),
                                    div(class = "panel panel-default",
                                        div(class = "panel-body",
                                            div(class = "row",
                                                div(class = "col-md-6",
                                                    tags$ul(
                                                      tags$li(strong("Original Dataset: "), textOutput(ns("original_rows"), inline = TRUE), " respondents"),
                                                      tags$li(strong("Final Dataset: "), textOutput(ns("final_rows"), inline = TRUE), " respondents"),
                                                      tags$li(strong("Complete Responses: "), textOutput(ns("complete_pct"), inline = TRUE), "% of original data")
                                                    )
                                                ),
                                                div(class = "col-md-6",
                                                    tags$ul(
                                                      tags$li(strong("Missing Data: "), textOutput(ns("missing_pct"), inline = TRUE), "% of cells"),
                                                      tags$li(strong("Missing Data Handling: "), textOutput(ns("missing_handling_text"), inline = TRUE)),
                                                      tags$li(strong("Random Sample: "), textOutput(ns("sample_info"), inline = TRUE))
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ),
                            # Original model summary content
                            div(class = "row",
                                div(class = "col-md-12", 
                                    uiOutput(ns("modelSummaryUI")))
                            ),
                            tags$hr(),
                            div(class = "row",
                                div(class = "col-md-12", 
                                    h4("Model Fit Statistics"),
                                    uiOutput(ns("modelFitUI")))
                            )
                          )
                 ),
                 
                 # Tab 2: Item Analysis
                 tabPanel("Item Analysis",
                          wellPanel(
                            h3("Item Parameters and Fit Statistics"),
                            div(class = "row",
                                div(class = "col-md-7",
                                    h4("Interpreting Item Parameters"),
                                    p("In Rasch modeling, item parameters represent the difficulty of each item:"),
                                    tags$ul(
                                      tags$li(strong("Higher values (more positive):"), " Indicate more difficult items that are harder to endorse or answer correctly."),
                                      tags$li(strong("Lower values (more negative):"), " Indicate easier items that are more likely to be endorsed or answered correctly.")
                                    ),
                                    h4("Interpreting Fit Statistics"),
                                    p("Fit statistics help identify items that may not function as expected within the Rasch model:"),
                                    tags$ul(
                                      tags$li(strong("Infit Mean-Square (MSQ):"), " Inlier-sensitive fit statistic, more sensitive to unexpected responses close to item difficulty"),
                                      tags$li(strong("Outfit Mean-Square (MSQ):"), " Outlier-sensitive fit statistic, more affected by unexpected responses far from item difficulty"),
                                      tags$li(strong("Expected range:"), " Typically 0.5 to 1.5; values outside this range suggest potential misfit"),
                                      tags$li(strong("Values > 1.5:"), " Item displays more randomness than expected (underfit)"),
                                      tags$li(strong("Values < 0.5:"), " Item displays less randomness than expected (overfit)")
                                    )
                                ),
                                div(class = "col-md-5",
                                    div(class = "panel panel-default",
                                        div(class = "panel-heading", h4("Scholarly References")),
                                        div(class = "panel-body",
                                            tags$ul(
                                              tags$li(strong("Bond & Fox (2015):"), " Suggest MSQ values between 0.6 and 1.4 as acceptable for rating scales."),
                                              tags$li(strong("Linacre (2002):"), " Recommends MSQ values between 0.5 and 1.5 as productive for measurement."),
                                              tags$li(strong("Wright & Linacre (1994):"), " Provide ranges of acceptable fit values for different test situations.")
                                            ),
                                            tags$hr(),
                                            strong("Full Citations:"),
                                            tags$ul(
                                              tags$li("Bond, T. G., & Fox, C. M. (2015). Applying the Rasch model: Fundamental measurement in the human sciences (3rd ed.). Routledge."),
                                              tags$li("Linacre, J. M. (2002). What do infit and outfit, mean-square and standardized mean? Rasch Measurement Transactions, 16(2), 878."),
                                              tags$li("Wright, B. D., & Linacre, J. M. (1994). Reasonable mean-square fit values. Rasch Measurement Transactions, 8(3), 370.")
                                            )
                                        )
                                    )
                                )
                            ),
                            div(class = "col-md-12",
                                div(class = "alert alert-info",
                                    h4("Fit Statistics for Engagement Surveys"),
                                    p("For multi-dimensional constructs like engagement or family-school partnerships:"),
                                    tags$ul(
                                      tags$li(strong("More lenient fit criteria (0.5-1.7)"), " are often appropriate when:"),
                                      tags$li(style = "margin-left: 20px;", "Multiple related constructs are being measured"),
                                      tags$li(style = "margin-left: 20px;", "Items have theoretical importance despite statistical misfit"),
                                      tags$li(style = "margin-left: 20px;", "The instrument is in developmental stages"),
                                      tags$li(strong("Reference:"), " Fisher, W.P. (2007). Rating scale instrument quality criteria. Rasch Measurement Transactions, 21(1), 1095.")
                                    ),
                                    p("For engagement surveys specifically, minor deviations from fit criteria should be interpreted in the context of your theoretical framework. Items with high misfit may indicate aspects of engagement that are qualitatively different or influenced by local factors.")
                                )
                            ),
                            div(class = "row", style = "margin-top: 20px;",
                                div(class = "col-md-12", 
                                    uiOutput(ns("itemStatsUI")))
                            )
                          )
                 ),
                 
                 # Tab 3: Item Characteristic Curves (ICCs)
                 # Tab 3: Item Characteristic Curves (ICCs)
                 tabPanel("Item Characteristic Curves",
                          wellPanel(
                            h3("Item Characteristic Curves (ICCs)"),
                            p("These curves show the probability of endorsing an item (or getting it correct) 
      as a function of the latent trait being measured."),
                            div(class = "row",
                                div(class = "col-md-12", 
                                    div(style = "text-align: center;",
                                        withSpinner(plotOutput(ns("iccPlots"), height = "600px"))
                                    )
                                )
                            )
                          )
                 ),
                 
                 # Tab 4: Wright Map
                 tabPanel("Wright Map",
                          wellPanel(
                            h3("Wright Map (Person-Item Map)"),
                            p("The Wright Map displays both person abilities and item difficulties on the same scale, 
                              allowing for direct comparison."),
                            div(class = "row",
                                div(class = "col-md-8",
                                    div(class = "panel panel-default",
                                        div(class = "panel-heading", h4("Interpreting the Wright Map")),
                                        div(class = "panel-body",
                                            tags$ul(
                                              tags$li(strong("Left side:"), " Distribution of person abilities - higher values indicate more able respondents"),
                                              tags$li(strong("Right side:"), " Distribution of item difficulties - higher values indicate more difficult items"),
                                              tags$li(strong("Alignment:"), " Ideally, items should be distributed across the same range as persons"),
                                              tags$li(strong("Gaps:"), " Areas with few items may indicate ranges where the instrument lacks precision"),
                                              tags$li(strong("Targeting:"), " The mean of item difficulties should be close to the mean of person abilities for optimal measurement")
                                            )
                                        )
                                    )
                                ),
                                div(class = "col-md-4",
                                    div(class = "alert alert-info",
                                        h4("Measurement Implications"),
                                        tags$ul(
                                          tags$li(strong("Items above most persons:"), " The test may be too difficult overall"),
                                          tags$li(strong("Items below most persons:"), " The test may be too easy overall"),
                                          tags$li(strong("Missing items in ability ranges:"), " Consider adding items of appropriate difficulty"),
                                          tags$li(strong("Redundant items:"), " Multiple items at the same difficulty level may be unnecessary")
                                        )
                                    )
                                )
                            ),
                            div(class = "row",
                                div(class = "col-md-12", 
                                    div(style = "text-align: center;",
                                        withSpinner(plotOutput(ns("wrightMap"), height = "600px"))))
                            )
                          )
                 ),
                 
                 # Tab 5: Technical Details
                 tabPanel("Technical Details",
                          wellPanel(
                            h3("Detailed Model Information"),
                            verbatimTextOutput(ns("technicalDetails"))
                          )
                 )
               )
        )
      )
    )
  )
}
    

# ---------------------- Server Function --------------------------- #
raschAnalysisServer <- function(id, adjustedData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize rasch_vals as reactiveValues (not reactiveVal)
    rasch_vals <- reactiveValues(
      detected_format = NULL,
      dimensionality_results = NULL
    )
    
    # # Debug - check what data is coming in
    # observe({
    #   req(adjustedData())
    #   cat("Rasch module received data at", Sys.time(), "\n")
    #   cat("Data has", nrow(adjustedData()), "rows and", ncol(adjustedData()), "columns\n")
    #   cat("Column names:", paste(names(adjustedData()), collapse=", "), "\n")
    # })
    # 
   
    # Get available constructs for selection
    
    availableConstructs <- reactive({
      req(adjustedData())
      processed_data <- adjustedData()
      
      # Check if the required columns exist
      if(all(c('constructName', 'constructID') %in% names(processed_data))) {
        # Get unique construct names
        constructs <- processed_data %>%
          dplyr::select(constructName, constructID) %>%
          distinct() %>%
          arrange(constructName)
        
        return(constructs)
      } else {
        # Create a default data frame if columns don't exist
        warning("Required columns 'constructName' or 'constructID' not found in data")
        return(data.frame(
          constructID = 1,
          constructName = 'All Items',
          stringsAsFactors = FALSE
        ))
      }
    })
    
    # Update the construct dropdown when data is available
    observe({
      constructs <- availableConstructs()
      if(!is.null(constructs) && nrow(constructs) > 0) {
        updateSelectInput(session, "selected_construct",
                          choices = setNames(constructs$constructID, constructs$constructName),
                          selected = constructs$constructID[1])
      }
    })
    
    # Get all available items for custom selection
    output$itemSelectionUI <- renderUI({
      req(adjustedData())
      processed_data <- adjustedData()
      
      # Get all items
      items <- processed_data %>%
        dplyr::select(rCode, qText, constructName) %>%
        distinct() %>%
        mutate(item_label = paste0(rCode, " - ", qText, " (", constructName, ")")) %>%
        arrange(constructName, rCode)
      
      selectizeInput(
        ns("custom_items"),
        "Select Items to Include:",
        choices = setNames(items$rCode, items$item_label),
        selected = items$rCode[1:min(10, nrow(items))],
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })
    
    # Prepare data for Rasch analysis
    prepareRaschData <- reactive({
      req(adjustedData())
      processed_data <- adjustedData()
      
      # Get the data based on selection
      if(input$model_type == "by_construct") {
        # Filter by selected construct
        filtered_data <- processed_data %>%
          filter(constructID == input$selected_construct)
        
        model_name <- paste("Construct:", 
                            filtered_data %>% 
                              pull(constructName) %>% 
                              unique())
        
      } else if(input$model_type == "unidimensional") {
        # Use all items
        filtered_data <- processed_data
        model_name <- "All Items (Unidimensional)"
        
      } else if(input$model_type == "custom") {
        # Use custom selected items
        filtered_data <- processed_data %>%
          filter(rCode %in% input$custom_items)
        
        model_name <- "Custom Item Selection"
      }
      
      # Use a random sample if requested
      if(input$use_sample) {
        sample_percentage <- input$sample_size / 100
        respondent_ids <- unique(filtered_data$respondentID)
        n_respondents <- length(respondent_ids)
        sampled_ids <- sample(respondent_ids, 
                              size = floor(n_respondents * sample_percentage), 
                              replace = FALSE)
        
        filtered_data <- filtered_data %>%
          filter(respondentID %in% sampled_ids)
        
        model_name <- paste0(model_name, " (", input$sample_size, "% sample)")
      }
      
      # Create wide format data
      wide_data <- filtered_data %>%
        dplyr::select(respondentID, rCode, response_value) %>%
        pivot_wider(names_from = rCode, values_from = response_value)
      
      # Calculate the amount of missing data
      total_cells <- nrow(wide_data) * (ncol(wide_data) - 1)  # Exclude respondentID
      missing_cells <- sum(is.na(wide_data[, -1]))
      missing_percentage <- round((missing_cells / total_cells) * 100, 1)
      
      # Calculate the number of complete rows
      complete_rows <- sum(complete.cases(wide_data[, -1]))
      complete_percentage <- round((complete_rows / nrow(wide_data)) * 100, 1)
      
      # Handle missing values
      if(input$missing_handling == "remove") {
        # Remove rows with any missing values
        model_data <- na.omit(wide_data) %>%
          dplyr::select(-respondentID)
        
      } else if(input$missing_handling == "impute") {
        # Impute missing values using MICE
        withProgress(message = 'Imputing missing values...', value = 0, {
          # Extract the data matrix without respondentID
          data_to_impute <- wide_data %>% dplyr::select(-respondentID)
          
          # Check if there are any missing values
          if(any(is.na(data_to_impute))) {
            # Impute missing values
            set.seed(123)  # For reproducibility
            imputed_data <- try(
              mice::mice(data_to_impute, 
                         m = 1, 
                         maxit = input$impute_iterations, 
                         method = 'pmm', 
                         printFlag = FALSE)
            )
            
            if(inherits(imputed_data, "try-error")) {
              # If imputation fails, fall back to complete cases
              model_data <- na.omit(data_to_impute)
              warning("Imputation failed. Using complete cases instead.")
            } else {
              # Extract the imputed dataset
              model_data <- mice::complete(imputed_data, 1)
            }
          } else {
            # If no missing values, use the data as is
            model_data <- data_to_impute
          }
        })
      }
      
      # Create a mapping of item codes to question text
      item_mapping <- filtered_data %>%
        dplyr::select(rCode, qText, constructName, constructID) %>%
        distinct()
      
      # Auto-detect item format if needed
      if(input$item_format == "auto") {
        # Check if data is dichotomous (0/1) or polytomous
        max_value <- max(filtered_data$response_value, na.rm = TRUE)
        format_type <- ifelse(max_value <= 1, "dichotomous", "polytomous")
        rasch_vals[["detected_format"]] <- format_type
      } else {
        rasch_vals[["detected_format"]] <- format_type <- input$item_format
      }
      
      # Make sure we have data after processing
      if(ncol(model_data) == 0) {
        stop("No data left after processing. Check your filtering criteria.")
      }
      
      if(nrow(model_data) < 5) {
        stop("Too few observations left after processing. Need at least 5 respondents.")
      }
      
      return(list(
        data = model_data,
        model_name = model_name,
        item_mapping = item_mapping,
        format_type = rasch_vals[["detected_format"]],
        missing_percentage = missing_percentage,
        complete_percentage = complete_percentage,
        original_rows = nrow(wide_data),
        final_rows = nrow(model_data),
        use_sample = input$use_sample,
        sample_percentage = if(input$use_sample) input$sample_size else 100
      ))
    })
    
    # Run dimensionality analysis
    observeEvent(input$run_dimensionality, {
      req(prepareRaschData())
      
      rasch_data <- prepareRaschData()
      data_matrix <- as.matrix(rasch_data$data)
      
      # Using tryCatch to handle potential errors
      tryCatch({
        withProgress(message = 'Running Dimensionality Analysis...', value = 0, {
          
          # Step 1: Parallel Analysis
          incProgress(0.3, detail = "Performing Parallel Analysis")
          parallel_analysis <- fa.parallel(data_matrix, fm = "minres", fa = "fa", plot = FALSE)
          
          # Step 2: Single-factor analysis to check loadings
          incProgress(0.3, detail = "Checking Factor Loadings")
          factor_analysis <- fa(data_matrix, nfactors = 1, rotate = "none", fm = "minres")
          
          # Extract loadings from factor analysis
          factor_loadings <- data.frame(
            Item = rownames(factor_analysis$loadings),
            Loading = factor_analysis$loadings[,1]
          )
          
          # Determine if data appears unidimensional
          suggested_factors <- parallel_analysis$nfact
          avg_loading <- mean(abs(factor_loadings$Loading))
          low_loading_items <- factor_loadings %>%
            filter(abs(Loading) < 0.3) %>%
            nrow()
          
          # Calculate additional metrics
          variance_explained <- parallel_analysis$fa.values[1] / sum(parallel_analysis$fa.values)
          eigenvalue_ratio <- parallel_analysis$fa.values[1] / max(parallel_analysis$fa.values[2], 0.001)
          
          explanation <- if(suggested_factors == 1 && avg_loading > 0.4) {
            list(
              conclusion = "Unidimensional",
              color = "success",
              text = "Data appears to be sufficiently unidimensional for Rasch analysis. The parallel analysis suggests a single factor, and most items load adequately on this factor."
            )
          } else if((suggested_factors <= 3 && variance_explained > 0.45 && eigenvalue_ratio > 3) || 
                    (suggested_factors <= 2 && avg_loading > 0.5)) {
            list(
              conclusion = "Functionally Unidimensional",
              color = "info",
              text = paste0("While parallel analysis suggests ", suggested_factors, " factors, the data appears functionally unidimensional. The first factor explains ", 
                            round(variance_explained * 100), "% of variance and is ", round(eigenvalue_ratio, 1), 
                            " times stronger than the second factor. Rasch analysis is likely appropriate.")
            )
          } else if(suggested_factors <= 3 && avg_loading > 0.3) {
            list(
              conclusion = "Potentially Unidimensional",
              color = "warning",
              text = paste0("Data shows some multidimensionality (", suggested_factors, " factors suggested), but may still be appropriate for Rasch analysis. Consider checking if low-loading items form a coherent subscale.")
            )
          } else {
            list(
              conclusion = "Multidimensional",
              color = "danger",
              text = paste0("Data appears to be multidimensional (", suggested_factors, " factors suggested). Rasch analysis assumes unidimensionality; consider splitting items into separate analyses based on constructs.")
            )
          }
          
          # Prepare plot data
          incProgress(0.4, detail = "Preparing Visualization")
          
          # Store results for rendering
          rasch_vals[["dimensionality_results"]] <- list(
            parallel_analysis = parallel_analysis,
            factor_analysis = factor_analysis,
            factor_loadings = factor_loadings,
            explanation = explanation,
            eigenvalues_actual = parallel_analysis$fa.values,
            eigenvalues_simulated = parallel_analysis$fa.sim
          )
        })
      }, error = function(e) {
        rasch_vals[["dimensionality_results"]] <- list(
          error = TRUE,
          error_message = paste("Error in dimensionality analysis:", e$message)
        )
      })
    })
    
    # Render dimensionality results
    output$dimensionalityResults <- renderUI({
      req(rasch_vals[["dimensionality_results"]])
      results <- rasch_vals[["dimensionality_results"]]
      
      if(isTRUE(results$error)) {
        return(div(class = "alert alert-danger",
                   h4("Error in Dimensionality Analysis"),
                   p(results$error_message)))
      }
      
      expl <- results$explanation
      
      tagList(
        div(class = paste0("alert alert-", expl$color),
            h4("Dimensionality Analysis Results: ", strong(expl$conclusion)),
            p(expl$text)
        ),
        div(class = "row",
            div(class = "col-md-6",
                h4("Parallel Analysis Scree Plot"),
                renderPlot({
                  # Create custom scree plot using ggplot2
                  df <- data.frame(
                    Factor = 1:length(results$eigenvalues_actual),
                    Actual = results$eigenvalues_actual,
                    Simulated = results$eigenvalues_simulated
                  ) %>%
                    pivot_longer(cols = c("Actual", "Simulated"), 
                                 names_to = "Type", 
                                 values_to = "Eigenvalue")
                  
                  ggplot(df, aes(x = Factor, y = Eigenvalue, color = Type, linetype = Type)) +
                    geom_line(size = 1) +
                    geom_point(size = 3) +
                    scale_color_manual(values = c("Actual" = "blue", "Simulated" = "red")) +
                    scale_linetype_manual(values = c("Actual" = "solid", "Simulated" = "dashed")) +
                    labs(title = "Parallel Analysis Scree Plot",
                         subtitle = paste("Suggested factors:", results$parallel_analysis$nfact),
                         x = "Factor Number",
                         y = "Eigenvalue") +
                    theme_minimal() +
                    theme(legend.position = "bottom",
                          plot.title = element_text(face = "bold"),
                          axis.title = element_text(face = "bold"))
                }, height = 300)
            ),
            div(class = "col-md-6",
                h4("Factor Loadings (Single Factor)"),
                renderPlot({
                  # Plot factor loadings
                  loadings_df <- results$factor_loadings %>%
                    mutate(Item = factor(Item, levels = Item[order(abs(Loading), decreasing = TRUE)])) %>%
                    mutate(Color = ifelse(abs(Loading) < 0.3, "Low", 
                                          ifelse(abs(Loading) < 0.5, "Medium", "High")))
                  
                  ggplot(loadings_df, aes(x = Item, y = Loading, fill = Color)) +
                    geom_col() +
                    scale_fill_manual(values = c("Low" = "#FF9999", "Medium" = "#FFCC99", "High" = "#99CC99")) +
                    labs(title = "Factor Loadings (Single Factor)",
                         subtitle = paste("Average absolute loading:", round(mean(abs(loadings_df$Loading)), 2)),
                         x = "Item",
                         y = "Loading") +
                    theme_minimal() +
                    theme(legend.position = "bottom",
                          plot.title = element_text(face = "bold"),
                          axis.title = element_text(face = "bold"),
                          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
                }, height = 300)
            )
        ),
        div(class = "row",
            div(class = "col-md-12",
                h4("Recommendation"),
                if(expl$conclusion == "Unidimensional") {
                  div(class = "alert alert-success",
                      p("The data appears unidimensional, which is optimal for Rasch analysis. You can proceed with confidence."))
                } else if(expl$conclusion == "Functionally Unidimensional") {
                  div(class = "alert alert-info",
                      p("The data appears functionally unidimensional despite some minor secondary dimensions. Your dataset shows:"),
                      tags$ul(
                        tags$li(strong("Strong primary dimension"), " that explains a substantial portion of the variance"),
                        tags$li(strong("High average factor loading"), " indicating good item cohesion"),
                        tags$li(strong("Eigenvalue ratio > 3"), " showing the first factor is much stronger than secondary factors")
                      ),
                      p("You can proceed with Rasch analysis with reasonable confidence. Consider reviewing or removing any items with particularly low loadings on the primary factor.")
                  )
                } else if(expl$conclusion == "Potentially Unidimensional") {
                  div(class = "alert alert-warning",
                      p("The data shows some multidimensionality but may still be appropriate for Rasch analysis. Consider:"),
                      tags$ul(
                        tags$li("Proceeding with the 'By Construct' model type to analyze each construct separately"),
                        tags$li("Checking if items with low loadings (< 0.3) form a coherent subscale"),
                        tags$li("Reviewing item content to ensure conceptual unity")
                      ))
                } else {
                  div(class = "alert alert-danger",
                      p("The data appears multidimensional, which violates a key assumption of Rasch analysis. Consider:"),
                      tags$ul(
                        tags$li(strong("Recommended:"), " Use the 'By Construct' model type to analyze each construct separately"),
                        tags$li("Use the 'Custom Item Selection' to group items that seem to measure the same construct"),
                        tags$li("Review your theoretical framework and survey structure")
                      ))
                }
            )
        
        )
      )
    })
    
    # Run Rasch Analysis
    raschResults <- eventReactive(input$run_rasch, {
      req(prepareRaschData())
      rasch_data <- prepareRaschData()
      data_matrix <- as.matrix(rasch_data$data)
      
      # Using tryCatch to handle potential errors
      tryCatch({
        withProgress(message = 'Running Rasch Analysis...', value = 0, {
          
          incProgress(0.1, detail = "Preparing data")
          
          # Step 1: Choose appropriate Rasch model based on data format
          incProgress(0.3, detail = "Fitting Rasch model")
          
          if(rasch_data$format_type == "dichotomous") {
            # For dichotomous data, use the Rasch model
            rasch_model <- eRm::RM(data_matrix)
          } else {
            # For polytomous data, use the Partial Credit Model
            rasch_model <- eRm::PCM(data_matrix)
          }
          
          # Step 2: Calculate person parameters
          incProgress(0.2, detail = "Calculating person parameters")
          person_params <- eRm::person.parameter(rasch_model)
          
          # Step 3: Item fit statistics
          incProgress(0.2, detail = "Computing item fit statistics")
          item_fit <- eRm::itemfit(person_params)
          
          # Step 4: Person fit statistics
          incProgress(0.1, detail = "Computing person fit statistics")
          person_fit <- eRm::personfit(person_params)
          
          # Step 5: Model fit and summary
          incProgress(0.1, detail = "Finalizing results")
          
          # Return all results including data processing info
          return(list(
            model = rasch_model,
            person_params = person_params,
            item_fit = item_fit,
            person_fit = person_fit,
            model_name = rasch_data$model_name,
            item_mapping = rasch_data$item_mapping,
            format_type = rasch_data$format_type,
            data = rasch_data$data,
            missing_percentage = rasch_data$missing_percentage,
            complete_percentage = rasch_data$complete_percentage,
            original_rows = rasch_data$original_rows,
            final_rows = rasch_data$final_rows,
            use_sample = rasch_data$use_sample,
            sample_percentage = rasch_data$sample_percentage,
            error = FALSE
          ))
        })
      }, error = function(e) {
        # Return error information
        return(list(
          error = TRUE,
          error_message = paste("Error in Rasch analysis:", e$message)
        ))
      })
    })
    
    # Display analysis status
    output$raschStatus <- renderUI({
      req(input$run_rasch > 0)
      
      results <- raschResults()
      
      if(isTRUE(results$error)) {
        div(class = "alert alert-danger",
            icon("triangle-exclamation"),
            "Error in Rasch analysis: ", results$error_message)
      } else {
        div(class = "alert alert-success",
            icon("check-circle"),
            "Rasch analysis completed successfully!")
      }
    })
    
    # Output for data processing information
    output$original_rows <- renderText({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      results$original_rows
    })
    
    output$final_rows <- renderText({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      results$final_rows
    })
    
    output$complete_pct <- renderText({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      results$complete_percentage
    })
    
    output$missing_pct <- renderText({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      results$missing_percentage
    })
    
    output$missing_handling_text <- renderText({
      req(raschResults(), !raschResults()$error)
      if(input$missing_handling == "remove") {
        "Removed incomplete cases"
      } else {
        "Imputed missing values"
      }
    })
    
    output$sample_info <- renderText({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      if(results$use_sample) {
        paste0(results$sample_percentage, "% of available data")
      } else {
        "100% (full dataset)"
      }
    })
    
    # Render model summary information
    output$modelSummaryUI <- renderUI({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      
      # Extract key information from model
      model_summary <- summary(results$model)
      
      # Create formatted output
      tagList(
        div(class = "row",
            div(class = "col-md-6",
                h4("Model Information"),
                tags$ul(
                  tags$li(strong("Model Type: "), ifelse(results$format_type == "dichotomous", 
                                                         "Dichotomous Rasch Model (RM)", 
                                                         "Polytomous Partial Credit Model (PCM)")),
                  tags$li(strong("Analysis Name: "), results$model_name),
                  tags$li(strong("Number of Items: "), length(results$model$X[1,])),
                  tags$li(strong("Number of Persons: "), length(results$model$X[,1])),
                  tags$li(strong("Estimation Method: "), input$rm_method)
                )
            ),
            div(class = "col-md-6",
                h4("Model Summary"),
                verbatimTextOutput(ns("briefModelSummary")),
                if(results$format_type == "auto") {
                  div(class = "alert alert-info",
                      p("Item format was auto-detected as ", strong(results$format_type), ".")
                  )
                } else {
                  NULL
                }
            )
        ),
        div(class = "row",
            div(class = "col-md-12",
                h4("Reliability Information"),
                div(class = "panel panel-default",
                    div(class = "panel-body",
                        renderText({
                          # Compute reliability estimates with robust error handling
                          tryCatch({
                            # Check if we have person parameters
                            if(is.null(results$person_params) || 
                               is.null(results$person_params$theta.table)) {
                              return("Could not compute reliability statistics. Person parameters are missing.")
                            }
                            
                            # Extract theta values and standard errors
                            theta_table <- results$person_params$theta.table
                            
                            # Check if we have the expected column structure
                            if(!"Person Parameter" %in% colnames(theta_table) || sum(is.na(theta_table[, "Person Parameter"])) == nrow(theta_table)) {
                              return("Could not compute reliability statistics. Person parameter values are missing or invalid.")
                            }
                            
                            # Get valid theta values
                            theta <- theta_table[, "Person Parameter"]
                            
                            # Get SE values from the list structure
                            if(!is.null(results$person_params$se.theta) && length(results$person_params$se.theta) > 0) {
                              # Extract SE from the first group (or only group)
                              se_theta <- results$person_params$se.theta[[1]]
                              
                              # Check if lengths match
                              if(length(theta) != length(se_theta)) {
                                # Try to match by names if available
                                if(!is.null(names(theta)) && !is.null(names(se_theta))) {
                                  # Create a matching vector
                                  matched_se <- rep(NA, length(theta))
                                  names(matched_se) <- names(theta)
                                  matched_indices <- match(names(se_theta), names(theta))
                                  matched_se[matched_indices] <- se_theta
                                  se_theta <- matched_se
                                } else {
                                  # If not matchable, just use a subset or extend
                                  if(length(theta) < length(se_theta)) {
                                    se_theta <- se_theta[1:length(theta)]
                                  } else {
                                    se_theta <- c(se_theta, rep(NA, length(theta) - length(se_theta)))
                                  }
                                }
                              }
                            } else {
                              return("Standard errors for person parameters are missing.")
                            }
                            
                            # Only use cases with valid theta and SE
                            valid_cases <- !is.na(theta) & !is.na(se_theta) & se_theta > 0
                            
                            if(sum(valid_cases) < 10) {
                              return("Not enough valid cases for reliability calculation. Only " 
                                     + sum(valid_cases) + " valid cases found.")
                            }
                            
                            # Calculate reliability
                            obs_var <- var(theta[valid_cases])
                            err_var <- mean(se_theta[valid_cases]^2)
                            true_var <- obs_var - err_var
                            
                            if(true_var <= 0 || obs_var <= 0) {
                              return("Could not compute reliability statistics. Estimated true variance is not positive.")
                            }
                            
                            PSI <- true_var / obs_var
                            separation <- sqrt(true_var / err_var)
                            
                            reliability_text <- paste0(
                              "Person Reliability (PSI): ", round(PSI, 3),
                              "\nPerson Separation: ", round(separation, 3),
                              "\n\nBased on ", sum(valid_cases), " valid cases out of ", length(theta), " total cases"
                            )
                            
                            # Also calculate Cronbach's alpha as a backup
                            if(!is.null(results$data) && ncol(results$data) > 0 && nrow(results$data) > 0) {
                              tryCatch({
                                alpha_result <- psych::alpha(results$data)
                                reliability_text <- paste0(
                                  reliability_text,
                                  "\n\nCronbach's alpha: ", round(alpha_result$total$std.alpha, 3),
                                  " (alternative reliability measure)"
                                )
                              }, error = function(e) {
                                # If alpha calculation fails, just return the PSI
                              })
                            }
                            
                            reliability_text
                            
                          }, error = function(e) {
                            # More descriptive error message with the exact error
                            paste0(
                              "Could not compute reliability statistics. This may happen with small sample sizes, extreme response patterns, or polytomous models with threshold issues.\n",
                              "Technical details: ", e$message
                            )
                          })
                        }),
                        p("The Person Separation Index (PSI) is analogous to Cronbach's alpha. Values > 0.8 indicate good reliability, 
                  values > 0.7 indicate acceptable reliability. Person Separation indicates how many statistically 
                  different strata of person abilities the test can distinguish (values > 2 are considered good).")
                    )
                )
            )
        
        )
      )
    })
    
    # Brief model summary text
    # Fixed brieftModelSummary function that correctly extracts model attributes
    output$briefModelSummary <- renderPrint({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      
      # Extract values more carefully
      model <- results$model
      
      # For PCM and RM models, the attributes are stored differently
      loglik <- if(!is.null(model$loglik)) model$loglik else NA
      iter <- if(!is.null(model$iter)) model$iter else NA
      deviance <- if(!is.null(attr(model, "deviance"))) attr(model, "deviance") else NA
      AIC <- if(!is.null(attr(model, "AIC"))) attr(model, "AIC") else NA
      
      # Print the information safely
      cat("Log-likelihood: ", ifelse(is.numeric(loglik), round(loglik, 2), "Not available"), "\n")
      cat("Number of iterations: ", ifelse(is.numeric(iter), iter, "Not available"), "\n")
      
      if(!is.na(deviance) && is.numeric(deviance)) {
        cat("Deviance: ", round(deviance, 2), "\n")
      }
      
      if(!is.na(AIC) && is.numeric(AIC)) {
        cat("AIC: ", round(AIC, 2), "\n")
      }
    })
    
    # Fixed modelFitUI function with proper extraction from eRm item fit output
    output$modelFitUI <- renderUI({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      
      # Check if item fit exists
      if(is.null(results$item_fit)) {
        return(div(class = "alert alert-warning",
                   "Item fit information is not available for this model."))
      }
      
      # Extract the item fit statistics directly using the structure we now know
      if(!is.null(results$item_fit$i.infitMSQ) && !is.null(results$item_fit$i.outfitMSQ)) {
        infit_msq <- results$item_fit$i.infitMSQ
        outfit_msq <- results$item_fit$i.outfitMSQ
        item_names <- names(results$item_fit$i.infitMSQ)
        
        # Get appropriate fit criteria based on selection
        misfit_ranges <- list(
          conservative = c(0.8, 1.2),
          standard = c(0.7, 1.3),
          survey = c(0.6, 1.4),
          lenient = c(0.5, 1.7),
          custom = c(1/input$misfit_cutoff, input$misfit_cutoff)
        )
        
        # Get selected criteria
        selected_range <- misfit_ranges[[input$fit_criteria]]
        lower_bound <- selected_range[1]
        upper_bound <- selected_range[2]
        
        # Count misfitting items
        outfit_misfit_count <- sum(outfit_msq < lower_bound | outfit_msq > upper_bound, na.rm = TRUE)
        infit_misfit_count <- sum(infit_msq < lower_bound | infit_msq > upper_bound, na.rm = TRUE)
        total_items <- length(item_names)
        
        # Create fit quality assessment
        fit_quality <- if(outfit_misfit_count / total_items < 0.1 && infit_misfit_count / total_items < 0.1) {
          list(text = "Good", class = "success")
        } else if(outfit_misfit_count / total_items < 0.2 && infit_misfit_count / total_items < 0.2) {
          list(text = "Acceptable", class = "info")
        } else if(outfit_misfit_count / total_items < 0.3 && infit_misfit_count / total_items < 0.3) {
          list(text = "Marginal", class = "warning")
        } else {
          list(text = "Poor", class = "danger")
        }
        
        # Create a data frame for plotting
        plot_data <- data.frame(
          Item = item_names,
          Infit = infit_msq,
          Outfit = outfit_msq,
          stringsAsFactors = FALSE
        )
        
        # Create formatted output
        tagList(
          div(class = "row",
              div(class = "col-md-4",
                  div(class = paste0("panel panel-", fit_quality$class),
                      div(class = "panel-heading", h4("Overall Fit Quality")),
                      div(class = "panel-body text-center",
                          h3(fit_quality$text),
                          p(paste0(outfit_misfit_count, " of ", total_items, " items (", 
                                   round(outfit_misfit_count / total_items * 100), "%) show outfit misfit")),
                          p(paste0(infit_misfit_count, " of ", total_items, " items (", 
                                   round(infit_misfit_count / total_items * 100), "%) show infit misfit"))
                      )
                  )
              ),
              div(class = "col-md-8",
                  div(class = "panel panel-default",
                      div(class = "panel-heading", h4("Fit Charts")),
                      div(class = "panel-body",
                          renderPlot({
                            # Create a data frame for ggplot
                            fit_data_long <- plot_data %>%
                              tidyr::pivot_longer(cols = c("Infit", "Outfit"), 
                                                  names_to = "Type", 
                                                  values_to = "MSQ")
                            
                            # Join with item mapping for better labels if available
                            if(!is.null(results$item_mapping) && nrow(results$item_mapping) > 0) {
                              # Try to join with item mapping
                              mapped_data <- tryCatch({
                                fit_data_long %>%
                                  left_join(results$item_mapping %>% 
                                              dplyr::select(rCode, qText) %>% 
                                              distinct(), 
                                            by = c("Item" = "rCode"))
                              }, error = function(e) {
                                # If join fails, return original data frame
                                fit_data_long
                              })
                              
                              # If join succeeded and added qText column
                              if("qText" %in% colnames(mapped_data) && !all(is.na(mapped_data$qText))) {
                                fit_data_long <- mapped_data
                                # Create display label with both code and truncated text
                                fit_data_long$DisplayItem <- ifelse(
                                  !is.na(fit_data_long$qText),
                                  paste0(fit_data_long$Item, "\n(", substr(fit_data_long$qText, 1, 25), "...)"),
                                  fit_data_long$Item
                                )
                              } else {
                                # If join failed or no qText, use item code as display
                                fit_data_long$DisplayItem <- fit_data_long$Item
                              }
                            } else {
                              # If no item mapping available, use item code as display
                              fit_data_long$DisplayItem <- fit_data_long$Item
                            }
                            
                            # Plot the fit statistics
                            ggplot(fit_data_long, aes(x = reorder(DisplayItem, MSQ), y = MSQ, fill = Type)) +
                              geom_col(position = "dodge") +
                              geom_hline(yintercept = upper_bound, linetype = "dashed", color = "red") +
                              geom_hline(yintercept = lower_bound, linetype = "dashed", color = "red") +
                              geom_hline(yintercept = 1, linetype = "solid", color = "darkgreen") +
                              scale_fill_manual(values = c("Infit" = "#66CCCC", "Outfit" = "#FFCC66")) +
                              coord_flip() +
                              labs(title = "Item Fit Statistics",
                                   subtitle = paste0("Acceptable range: ", round(lower_bound, 2), " to ", round(upper_bound, 2)),
                                   x = "Item",
                                   y = "Mean Square (MSQ)") +
                              theme_minimal() +
                              theme(legend.position = "bottom",
                                    plot.title = element_text(face = "bold"),
                                    axis.title = element_text(face = "bold"))
                          }, height = 400)
                      )
                  )
              )
          )
        )
      } else {
        # Return a message if we can't find the expected fit statistics
        div(class = "alert alert-warning",
            h4("Item Fit Statistics Not Available"),
            p("The expected item fit statistics (infit/outfit MSQ) could not be found in the model results."),
            p("This may happen with certain model types or when fit statistics cannot be calculated.")
        )
      }
    })
    
    # Add a debug output to help troubleshoot
    output$fit_debug_info <- renderPrint({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      
      cat("Model class:", class(results$model), "\n")
      cat("Item fit class:", class(results$item_fit), "\n")
      
      if(!is.null(results$item_fit)) {
        cat("Item fit components:\n")
        print(names(results$item_fit))
        
        if("itemfit" %in% names(results$item_fit)) {
          cat("\nItemfit structure:\n")
          print(str(results$item_fit$itemfit))
          cat("\nItemfit column names:\n")
          print(colnames(results$item_fit$itemfit))
        } else if("i.fit" %in% names(results$item_fit)) {
          cat("\ni.fit structure:\n")
          print(str(results$item_fit$i.fit))
        }
      }
    })
    
    # Fixed Item statistics UI function
    output$itemStatsUI <- renderUI({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      
      # Check if required components exist
      if(is.null(results$model) || is.null(results$item_fit)) {
        return(div(class = "alert alert-warning",
                   "Model or item fit information is not available."))
      }
      
      # Extract item parameters and fit statistics
      tryCatch({
        # Get item parameters from model
        item_params <- NULL
        if(!is.null(results$model$betapar)) {
          # Extract base parameters - need to handle PCM having multiple categories
          # For PCM, we'll take the first parameter for each item
          item_names <- colnames(results$model$X)
          
          # Initialize vectors for item stats
          difficulty <- numeric(length(item_names))
          se <- numeric(length(item_names))
          names(difficulty) <- item_names
          names(se) <- item_names
          
          # Find the main parameter for each item
          for(i in seq_along(item_names)) {
            item <- item_names[i]
            # Look for parameters for this item
            param_indices <- grep(paste0("^beta ", item, "\\.c1$"), names(results$model$betapar))
            if(length(param_indices) > 0) {
              # Use c1 parameter as the main difficulty
              difficulty[i] <- results$model$betapar[param_indices[1]]
              se[i] <- results$model$se.beta[param_indices[1]]
            } else {
              # Try without the .c1 suffix
              param_indices <- grep(paste0("^beta ", item), names(results$model$betapar))
              if(length(param_indices) > 0) {
                # Use the first parameter found
                difficulty[i] <- results$model$betapar[param_indices[1]]
                se[i] <- results$model$se.beta[param_indices[1]]
              } else {
                # No parameter found
                difficulty[i] <- NA
                se[i] <- NA
              }
            }
          }
        } else {
          # Fallback if betapar not available
          item_names <- colnames(results$model$X)
          difficulty <- rep(NA, length(item_names))
          se <- rep(NA, length(item_names))
          names(difficulty) <- item_names
          names(se) <- item_names
        }
        
        # Get fit statistics from item_fit
        infit_msq <- results$item_fit$i.infitMSQ
        outfit_msq <- results$item_fit$i.outfitMSQ
        infit_z <- results$item_fit$i.infitZ
        outfit_z <- results$item_fit$i.outfitZ
        discrimination <- results$item_fit$i.disc
        
        # Create a data frame with all item statistics
        item_stats <- data.frame(
          Item = item_names,
          Difficulty = difficulty,
          SE = se,
          Infit_MSQ = infit_msq,
          Infit_Z = infit_z,
          Outfit_MSQ = outfit_msq,
          Outfit_Z = outfit_z,
          Discrimination = discrimination,
          stringsAsFactors = FALSE
        )
        
        # Join with item mapping for better display if available
        if(!is.null(results$item_mapping) && nrow(results$item_mapping) > 0) {
          # Try join with tryCatch to handle potential errors
          item_stats <- tryCatch({
            item_stats %>%
              left_join(results$item_mapping %>% 
                          dplyr::select(rCode, qText, constructName) %>% 
                          distinct(), 
                        by = c("Item" = "rCode"))
          }, error = function(e) {
            # If join fails, return original data frame
            item_stats
          })
        }
        
        # Get appropriate fit criteria based on selection
        misfit_ranges <- list(
          conservative = c(0.8, 1.2),
          standard = c(0.7, 1.3),
          survey = c(0.6, 1.4),
          lenient = c(0.5, 1.7),
          custom = c(1/input$misfit_cutoff, input$misfit_cutoff)
        )
        
        # Get selected criteria
        selected_range <- misfit_ranges[[input$fit_criteria]]
        lower_bound <- selected_range[1]
        upper_bound <- selected_range[2]
        
        # Render the table with conditional formatting
        DT::renderDataTable({
          DT::datatable(
            item_stats,
            options = list(
              pageLength = 25,
              scrollX = TRUE,
              dom = 'Bfrtip'
            ),
            rownames = FALSE,
            caption = "Item Parameters and Fit Statistics"
          ) %>%
            formatRound(columns = c("Difficulty", "SE", "Infit_MSQ", "Infit_Z", "Outfit_MSQ", "Outfit_Z", "Discrimination"), digits = 2) %>%
            formatStyle(
              "Infit_MSQ",
              backgroundColor = styleInterval(
                c(lower_bound, 1, upper_bound),
                c("#FFCCCC", "#CCFFCC", "#FFFFCC", "#FFCCCC")
              ),
              fontWeight = styleInterval(
                c(lower_bound, upper_bound),
                c("bold", "normal", "bold")
              )
            ) %>%
            formatStyle(
              "Outfit_MSQ",
              backgroundColor = styleInterval(
                c(lower_bound, 1, upper_bound),
                c("#FFCCCC", "#CCFFCC", "#FFFFCC", "#FFCCCC")
              ),
              fontWeight = styleInterval(
                c(lower_bound, upper_bound),
                c("bold", "normal", "bold")
              )
            ) %>%
            formatStyle(
              "Discrimination",
              backgroundColor = styleInterval(
                c(0.3, 0.7),
                c("#FFCCCC", "#FFFFCC", "#CCFFCC")
              )
            )
        })
        
      }, error = function(e) {
        # If anything fails, return an error message
        div(class = "alert alert-danger",
            h4("Error Generating Item Analysis Table"),
            p("There was an error generating the item analysis table:"),
            p(strong(e$message)),
            p("This might be due to the structure of the model output or inconsistencies in the data."),
            # Add a button to show technical details
            actionButton(ns("show_item_debug"), "Show Technical Details")
        )
      })
    })
    
    # Add observer for debug information button
    observeEvent(input$show_item_debug, {
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      
      showModal(modalDialog(
        title = "Item Analysis Debug Information",
        
        # Display model structure
        h4("Model Structure"),
        verbatimTextOutput(ns("item_debug_model")),
        
        # Display item fit structure
        h4("Item Fit Structure"),
        verbatimTextOutput(ns("item_debug_fit")),
        
        size = "l",
        easyClose = TRUE
      ))
    })
    
    # Debug outputs
    output$item_debug_model <- renderPrint({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      
      cat("Model class:", class(results$model), "\n")
      cat("Item names:", colnames(results$model$X), "\n\n")
      
      cat("Beta parameters (first 10):\n")
      if(!is.null(results$model$betapar)) {
        print(head(results$model$betapar, 10))
      } else {
        cat("No beta parameters available\n")
      }
    })
    
    output$item_debug_fit <- renderPrint({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      
      cat("Item fit class:", class(results$item_fit), "\n\n")
      
      cat("Item fit components:\n")
      print(names(results$item_fit))
      
      cat("\nInfit MSQ (first 10):\n")
      if(!is.null(results$item_fit$i.infitMSQ)) {
        print(head(results$item_fit$i.infitMSQ, 10))
      } else {
        cat("No infit MSQ available\n")
      }
    })
    
    # ICC plot handling
    # ICC plots
    output$iccPlots <- renderPlot({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      
      # Get the model
      model <- results$model
      
      # Get all item names
      item_names <- colnames(model$X)
      
      # Set up a grid layout based on number of items
      n_items <- length(item_names)
      n_cols <- min(3, n_items)  # Maximum 3 columns
      n_rows <- ceiling(n_items / n_cols)
      
      # Set up the plotting grid
      par(mfrow = c(n_rows, n_cols))
      
      # For each item, create a separate plot
      for(item in item_names) {
        tryCatch({
          # Plot just this one item
          plotICC(model, item.subset = item, ask = FALSE, mplot = FALSE)
        }, error = function(e) {
          # If there's an error for this item, show a placeholder
          plot(1, 1, type = "n", 
               xlim = c(-4, 4), ylim = c(0, 1),
               xlab = "Ability", ylab = "Probability",
               main = paste("Item:", item))
          text(0, 0.5, paste("Error:", e$message), cex = 0.8)
        })
      }
    })
    
    
    # Wright Map
    output$wrightMap <- renderPlot({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      
      tryCatch({
        # Correctly extract person ability parameters based on your model structure
        if(!is.null(results$person_params$thetapar) && 
           length(results$person_params$thetapar) > 0 && 
           !is.null(results$person_params$thetapar[[1]])) {
          # Extract thetas from the first group (NAgroup1)
          thetas <- results$person_params$thetapar[[1]]
        } else if(!is.null(results$person_params$theta.table) && 
                  "Person Parameter" %in% colnames(results$person_params$theta.table)) {
          # Alternative: extract from theta.table if available
          thetas <- results$person_params$theta.table[, "Person Parameter"]
        } else {
          # Fallback in case neither approach works
          plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "Cannot generate Wright Map: Person parameters not found", cex = 1.5)
          return()
        }
        
        # Debug info
        cat("Number of thetas:", length(thetas), "\n")
        cat("First few theta values:", head(thetas), "\n")
        
        # Get the item difficulties - filter to only keep main parameters
        all_betas <- results$model$betapar
        
        # For PCM models, need to handle multiple parameters per item
        if(length(grep("\\.c", names(all_betas))) > 0) {
          # This is a PCM model with category parameters
          # For visualization, let's use only one parameter per item (e.g., c1 parameters)
          # Get unique item names without the category suffix
          item_names <- unique(gsub("\\.c[0-9]+$", "", gsub("^beta ", "", names(all_betas))))
          
          # Initialize a vector for filtered difficulties
          item_diff <- numeric(length(item_names))
          names(item_diff) <- item_names
          
          # For each item, find an appropriate difficulty parameter
          for(i in seq_along(item_names)) {
            # Try to find c1 parameter first
            c1_idx <- grep(paste0("^beta ", item_names[i], "\\.c1$"), names(all_betas))
            if(length(c1_idx) > 0) {
              item_diff[i] <- all_betas[c1_idx[1]]
            } else {
              # Try to find any parameter for this item
              any_idx <- grep(paste0("^beta ", item_names[i]), names(all_betas))
              if(length(any_idx) > 0) {
                item_diff[i] <- all_betas[any_idx[1]]
              } else {
                item_diff[i] <- NA
              }
            }
          }
        } else {
          # For RM models, just use the betapar directly
          item_diff <- all_betas
        }
        
        # Debug info
        cat("Number of items:", length(item_diff), "\n")
        cat("First few item difficulties:", head(item_diff), "\n")
        
        # Create a basic Wright Map manually
        # Set up the plot area
        plot(NA, NA, xlim = c(0, 1), ylim = c(-4, 4),
             xlab = "", ylab = "Logit (Ability/Difficulty)",
             main = "Wright Map (Person-Item Map)",
             xaxt = "n")  # No x-axis ticks
        
        # Add a midline
        abline(h = 0, lty = 2, col = "gray")
        
        # Left side: Person distribution
        if(length(thetas) > 1) {
          # For this large dataset, use density estimation
          # Remove any NA or infinite values
          thetas <- thetas[is.finite(thetas)]
          
          if(length(thetas) > 0) {
            # Create a density plot of person abilities
            dens <- try(density(thetas, n = 512, from = -4, to = 4, na.rm = TRUE), silent = TRUE)
            
            if(!inherits(dens, "try-error") && !is.null(dens$x) && !is.null(dens$y) && 
               length(dens$x) == length(dens$y) && length(dens$x) > 0) {
              # Scale the density to fit on the left half of the plot
              max_dens <- max(dens$y)
              scaled_dens <- dens$y / max_dens * 0.45
              
              # Plot the density as a filled polygon - making sure vectors have same length
              x_vals <- c(0.5 - scaled_dens, rep(0.5, length(dens$y)))
              y_vals <- c(dens$x, rev(dens$x))
              
              # Double check that lengths match
              if(length(x_vals) == length(y_vals)) {
                polygon(x_vals, y_vals, col = "lightblue", border = "blue")
              } else {
                # If vectors don't match, use a simpler approach with histogram
                h <- hist(thetas, breaks = seq(-4, 4, by = 0.5), plot = FALSE)
                
                # Calculate the center of each bin
                mids <- h$mids
                # Normalize counts
                counts <- h$counts / max(h$counts) * 0.45
                
                # Plot bars
                for(i in 1:length(mids)) {
                  if(counts[i] > 0) {
                    rect(0.5 - counts[i], mids[i] - 0.25, 0.5, mids[i] + 0.25, 
                         col = "lightblue", border = "blue")
                  }
                }
              }
            } else {
              # If density calculation fails, use a simpler histogram approach
              h <- hist(thetas, breaks = seq(-4, 4, by = 0.5), plot = FALSE)
              
              # Calculate the center of each bin
              mids <- h$mids
              # Normalize counts
              counts <- h$counts / max(h$counts) * 0.45
              
              # Plot bars
              for(i in 1:length(mids)) {
                if(counts[i] > 0) {
                  rect(0.5 - counts[i], mids[i] - 0.25, 0.5, mids[i] + 0.25, 
                       col = "lightblue", border = "blue")
                }
              }
            }
            
            # Add mean and SD indicators
            mean_theta <- mean(thetas, na.rm = TRUE)
            sd_theta <- sd(thetas, na.rm = TRUE)
            
            segments(0.25, mean_theta, 0.5, mean_theta, col = "red", lwd = 2)
            text(0.2, mean_theta, "Mean", col = "red", pos = 2)
            
            segments(0.25, mean_theta + sd_theta, 0.5, mean_theta + sd_theta, col = "red", lty = 2)
            segments(0.25, mean_theta - sd_theta, 0.5, mean_theta - sd_theta, col = "red", lty = 2)
          }
          
          # Add some person count info
          text(0.25, -3.5, paste(length(thetas), "persons"), cex = 0.8)
        } else {
          text(0.25, 0, "No valid person parameters", cex = 1)
        }
        
        # Right side: Item difficulties
        if(length(item_diff) > 0) {
          # Remove any NA values
          item_diff <- item_diff[!is.na(item_diff)]
          
          # Plot item positions
          points(rep(0.75, length(item_diff)), item_diff, pch = 16, col = "darkgreen")
          
          # Add item labels
          for(i in 1:length(item_diff)) {
            # Shortened labels for readability
            item_name <- names(item_diff)[i]
            if(nchar(item_name) > 15) {
              item_label <- paste0(substr(item_name, 1, 12), "...")
            } else {
              item_label <- item_name
            }
            
            text(0.8, item_diff[i], item_label, pos = 4, cex = 0.7)
          }
          
          # Add mean item difficulty
          mean_diff <- mean(item_diff)
          segments(0.5, mean_diff, 0.75, mean_diff, col = "darkgreen", lwd = 2)
          text(0.45, mean_diff, "Mean", col = "darkgreen", pos = 2)
          
          # Add item count info
          text(0.75, -3.5, paste(length(item_diff), "items"), cex = 0.8)
        } else {
          text(0.75, 0, "No valid item parameters", cex = 1)
        }
        
        # Add dividing line
        abline(v = 0.5, lty = 3)
        
        # Add labels
        text(0.25, 3.5, "PERSONS", font = 2)
        text(0.75, 3.5, "ITEMS", font = 2)
        
      }, error = function(e) {
        # If anything fails, show a simple error message
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error generating Wright Map:", e$message), cex = 1.2)
        text(1, 0.8, "The structure of the model may not be compatible with this visualization", cex = 1)
        
        # Print the error to the console for debugging
        cat("Wright Map error:", e$message, "\n")
        print(traceback())
      })
    })
    
    # Technical details
    output$technicalDetails <- renderPrint({
      req(raschResults(), !raschResults()$error)
      results <- raschResults()
      
      # Print full model summary
      cat("FULL MODEL SUMMARY\n\n")
      print(summary(results$model))
      
      cat("\n\nITEM PARAMETERS\n\n")
      print(results$model$betapar)
      
      cat("\n\nITEM FIT STATISTICS\n\n")
      print(results$item_fit)
      
      cat("\n\nPERSON PARAMETERS\n\n")
      print(head(results$person_params$theta.table, 10))
      cat("... (showing first 10 persons only)\n")
      
      cat("\n\nPERSON FIT STATISTICS\n\n")
      print(head(results$person_fit$p.outfitMSQ, 10))
      cat("... (showing first 10 persons only)\n")
    })
  })
}