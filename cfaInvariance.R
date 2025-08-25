# modules/cfaInvariance.R

library(shiny)
library(dplyr)
library(tidyr)
library(lavaan)
library(semPlot)
library(DT)
library(htmltools)
library(shinycssloaders)
library(formattable)
library(kableExtra)
library(ggplot2)

# ---------------------- UI Function ------------------------------- #
cfaInvarianceUI <- function(id, label = "CFA Measurement Invariance") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             wellPanel(
               h3("Measurement Invariance Analysis"),
               p("This module evaluates whether the factor structure is equivalent across different demographic groups. 
                 Measurement invariance is a prerequisite for making valid group comparisons."),
               
               # Model setup panel
               tabsetPanel(
                 id = ns("model_setup_tabs"),
                 
                 # Tab 1: Model & Group Selection
                 tabPanel("Model & Group Selection",
                          div(class = "row",
                              div(class = "col-md-6",
                                  # CFA Model Selection
                                  h4("Step 1: Select CFA Model"),
                                  radioButtons(ns("model_source"), "Model Source:",
                                               choices = c("Use existing CFA model" = "existing",
                                                           "Create new model" = "new"),
                                               selected = "new"),
                                  conditionalPanel(
                                    condition = "input.model_source == 'existing'",
                                    ns = ns,
                                    selectInput(ns("existing_model"), "Select Model:",
                                                choices = NULL)  # Will be updated dynamically
                                  ),
                                  conditionalPanel(
                                    condition = "input.model_source == 'new'",
                                    ns = ns,
                                    uiOutput(ns("constructSelectionUI"))
                                  )
                              ),
                              div(class = "col-md-6",
                                  # Group Selection
                                  h4("Step 2: Select Demographic Group"),
                                  uiOutput(ns("demoSelectionUI")),
                                  uiOutput(ns("groupSelectionUI")),
                                  hr(),
                                  h4("Data Information"),
                                  verbatimTextOutput(ns("dataInfo"))
                              )
                          )
                 ),
                 
                 # Tab 2: Invariance Options
                 tabPanel("Invariance Options",
                          div(class = "row",
                              div(class = "col-md-6",
                                  h4("Invariance Testing Approach"),
                                  radioButtons(ns("invariance_approach"), "Testing Approach:",
                                               choices = c("Sequential (Recommended)" = "sequential",
                                                           "Selective Focus" = "selective"),
                                               selected = "sequential"),
                                  conditionalPanel(
                                    condition = "input.invariance_approach == 'selective'",
                                    ns = ns,
                                    checkboxGroupInput(ns("sel_invariance_levels"), "Invariance Levels to Test:",
                                                       choices = c("Configural invariance" = "configural",
                                                                   "Metric invariance (equal loadings)" = "metric",
                                                                   "Scalar invariance (equal intercepts)" = "scalar",
                                                                   "Strict invariance (equal residuals)" = "strict",
                                                                   "Latent means" = "means"),
                                                       selected = c("configural", "metric", "scalar"))
                                  ),
                                  checkboxInput(ns("use_scaled"), "Use scaled chi-square difference tests (for non-normal data)", TRUE),
                                  selectInput(ns("estimator"), "Estimation Method:",
                                              choices = c("Maximum Likelihood" = "ML",
                                                          "Robust Maximum Likelihood" = "MLR",
                                                          "Diagonally Weighted Least Squares" = "DWLS"),
                                              selected = "MLR")
                              ),
                              div(class = "col-md-6",
                                  h4("Advanced Options"),
                                  numericInput(ns("cutoff_cfi"), "CFI Change Cutoff:", 
                                               value = 0.01, min = 0.001, max = 0.05, step = 0.001),
                                  numericInput(ns("cutoff_rmsea"), "RMSEA Change Cutoff:", 
                                               value = 0.015, min = 0.001, max = 0.05, step = 0.001),
                                  checkboxInput(ns("std_loadings"), "Show standardized loadings in results", TRUE),
                                  radioButtons(ns("missing_handling"), "Missing Data Handling:",
                                               choices = c("Full Information Maximum Likelihood (FIML)" = "fiml",
                                                           "Listwise deletion" = "listwise"),
                                               selected = "fiml"),
                                  div(class = "alert alert-info",
                                      h4("Fit Indices Interpretation"),
                                      tags$ul(
                                        tags$li(strong("CFI Δ ≤ 0.01:"), " Support for invariance"),
                                        tags$li(strong("RMSEA Δ ≤ 0.015:"), " Support for invariance"),
                                        tags$li(strong("Chi-square difference test:"), " p > 0.05 supports invariance")
                                      ),
                                      p("Reference: Chen (2007). Sensitivity of goodness of fit indexes to lack of measurement invariance.")
                                  )
                              )
                          )
                 )
               ),
               
               div(class = "row", style = "margin-top: 15px;",
                   div(class = "col-md-12", 
                       div(style = "text-align: center;",
                           actionButton(ns("run_invariance"), "Run Invariance Analysis", 
                                        class = "btn-lg btn-primary"))
                   )
               ),
               
               div(class = "row", style = "margin-top: 10px;",
                   div(class = "col-md-12",
                       uiOutput(ns("analysisStatus"))
                   )
               )
             )
      )
    ),
    
    conditionalPanel(
      condition = "input.run_invariance > 0",
      ns = ns,
      
      fluidRow(
        column(12,
               tabsetPanel(
                 id = ns("invariance_tabs"),
                 
                 # Tab 1: Results Overview
                 tabPanel("Results Overview",
                          wellPanel(
                            h3("Measurement Invariance Results"),
                            div(class = "row",
                                div(class = "col-md-12", 
                                    uiOutput(ns("invarianceOverviewUI")))
                            )
                          )
                 ),
                 
                 # Tab 2: Model Comparisons
                 tabPanel("Model Comparisons",
                          wellPanel(
                            h3("Model Fit Comparisons"),
                            div(class = "row",
                                div(class = "col-md-12", 
                                    withSpinner(tableOutput(ns("modelComparisonTable"))))
                            ),
                            div(class = "row",
                                div(class = "col-md-12",
                                    uiOutput(ns("comparisonInterpretation")))
                            )
                          )
                 ),
                 
                 # Tab 3: Group Parameter Comparison
                 tabPanel("Group Parameters",
                          wellPanel(
                            h3("Group Differences in Parameters"),
                            p("This tab shows key parameter estimates for each group, allowing comparison of loadings, intercepts, and other parameters."),
                            selectInput(ns("parameter_type"), "Parameter Type:",
                                        choices = c("Factor Loadings" = "loadings",
                                                    "Intercepts" = "intercepts",
                                                    "Residual Variances" = "residuals",
                                                    "Factor Variances" = "factor_var",
                                                    "Factor Covariances" = "factor_cov",
                                                    "Latent Means" = "means"),
                                        selected = "loadings"),
                            
                            # Parameter comparison table
                            withSpinner(DT::dataTableOutput(ns("parameterTable"))),
                            
                            # Visual parameter comparison
                            h4("Visual Parameter Comparison"),
                            withSpinner(plotOutput(ns("parameterPlot"), height = "500px")),
                            div(class = "alert alert-info",
                                "This plot shows parameter estimates with 95% confidence intervals for each group. 
                                Non-overlapping confidence intervals suggest potential measurement non-invariance.")
                          )
                 ),
                 
                 # Tab 4: Modification Indices 
                 tabPanel("Modification Indices",
                          wellPanel(
                            h3("Modification Indices"),
                            p("This tab shows modification indices that can help identify sources of misfit in your invariance models."),
                            selectInput(ns("mi_model"), "Select Model:",
                                        choices = NULL, # Will be populated when results are available
                                        selected = NULL),
                            numericInput(ns("mi_threshold"), "Show indices above:", 
                                         value = 3.84, min = 0, max = 50),
                            withSpinner(DT::dataTableOutput(ns("modIndicesTable"))),
                            div(class = "alert alert-warning",
                                strong("Note: "), "Modification indices suggest potential model improvements, but changes should be theoretically justified.
                                MI > 3.84 suggests a significant improvement if the parameter were freely estimated.")
                          )
                 ),
                 
                 # Tab 5: Technical Output
                 tabPanel("Technical Output",
                          wellPanel(
                            h3("Technical Details"),
                            selectInput(ns("tech_model"), "Select Model:",
                                        choices = NULL, # Will be populated when results are available
                                        selected = NULL),
                            verbatimTextOutput(ns("technicalOutput"))
                          )
                 )
               )
        )
      )
    )
  )
}

# ---------------------- Server Function --------------------------- #
cfaInvarianceServer <- function(id, adjustedData, cfaModelData = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store invariance results
    invarianceResults <- reactiveVal(NULL)
    
    # Get available constructs for selection
    # Find the availableConstructs reactive function in cfaInvariance.R and replace with:
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
    
    # Render construct selection UI
    output$constructSelectionUI <- renderUI({
      req(availableConstructs())
      constructs <- availableConstructs()
      
      selectizeInput(
        ns("selected_constructs"),
        "Select Constructs to Include:",
        choices = setNames(constructs$constructID, constructs$constructName),
        selected = constructs$constructID[1],
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })
    
    # Get demographic variables from the data
    demographicVars <- reactive({
      req(adjustedData())
      processed_data <- adjustedData()
      
      # Check for demo metadata attribute
      demo_metadata <- attr(processed_data, "demo_metadata")
      
      if (!is.null(demo_metadata)) {
        # Use the configured demographics with their display names
        setNames(demo_metadata$column, demo_metadata$display_name)
      } else {
        # Fallback to detecting columns ending with _bin
        demo_cols <- grep("_bin$", names(processed_data), value = TRUE)
        setNames(demo_cols, demo_cols)
      }
    })
    
    # Render demographic selection UI
    output$demoSelectionUI <- renderUI({
      demo_vars <- demographicVars()
      if (length(demo_vars) > 0) {
        selectInput(
          ns("selected_demographic"),
          "Select Demographic Variable:",
          choices = demo_vars,
          selected = names(demo_vars)[1]
        )
      } else {
        div(class = "alert alert-warning",
            "No demographic variables detected. Please add demographics in the Data Preparation step.")
      }
    })
    
    # Get unique values for the selected demographic
    demographicGroups <- reactive({
      req(adjustedData(), input$selected_demographic)
      processed_data <- adjustedData()
      
      # Get unique values
      groups <- processed_data %>%
        dplyr::select(!!sym(input$selected_demographic)) %>%
        distinct() %>%
        pull()
      
      # Remove NA values and convert to character
      groups <- groups[!is.na(groups)]
      groups <- as.character(groups)
      
      groups
    })
    
    # Render group selection UI
    output$groupSelectionUI <- renderUI({
      req(demographicGroups())
      groups <- demographicGroups()
      
      if (length(groups) < 2) {
        div(class = "alert alert-warning",
            "This demographic variable doesn't have enough groups (minimum 2 required).")
      } else {
        tagList(
          selectInput(
            ns("reference_group"),
            "Reference Group:",
            choices = groups,
            selected = groups[1]
          ),
          checkboxGroupInput(
            ns("comparison_groups"),
            "Comparison Groups:",
            choices = groups[groups != input$reference_group],
            selected = groups[groups != input$reference_group][1]
          )
        )
      }
    })
    
    # Show data info
    output$dataInfo <- renderPrint({
      req(adjustedData(), input$selected_demographic, 
          input$reference_group, input$comparison_groups)
      
      processed_data <- adjustedData()
      demographic_var <- input$selected_demographic
      
      # Get counts by group
      group_counts <- processed_data %>%
        group_by(!!sym(demographic_var)) %>%
        summarise(respondent_count = n_distinct(respondentID)) %>%
        filter(!is.na(!!sym(demographic_var)))
      
      # Print info about the selected groups
      cat("Data Information:\n\n")
      
      # Reference group
      ref_count <- group_counts %>%
        filter(!!sym(demographic_var) == input$reference_group) %>%
        pull(respondent_count)
      cat("Reference Group:", input$reference_group, "\n")
      cat("  Count:", ref_count, "respondents\n\n")
      
      # Comparison groups
      cat("Comparison Groups:\n")
      for (group in input$comparison_groups) {
        group_count <- group_counts %>%
          filter(!!sym(demographic_var) == group) %>%
          pull(respondent_count)
        cat("  -", group, ":", group_count, "respondents\n")
      }
    })
    
    # Prepare data for Invariance Analysis
    prepareInvarianceData <- reactive({
      req(adjustedData(), input$selected_demographic, 
          input$reference_group, input$comparison_groups,
          input$model_source)
      
      processed_data <- adjustedData()
      
      # Get the model specification based on the selection
      if (input$model_source == "new") {
        req(input$selected_constructs)
        
        # Filter for selected constructs
        selected_data <- processed_data %>%
          filter(constructID %in% input$selected_constructs)
        
        # Generate model specification based on constructs
        model_spec <- selected_data %>%
          dplyr::select(constructID, rCode) %>%
          distinct() %>%
          group_by(constructID) %>%
          summarise(indicators = paste(rCode, collapse = " + ")) %>%
          ungroup() %>%
          mutate(construct_label = paste0("F", sub("^C", "", constructID)),
                 model_line = paste(construct_label, "=~", indicators)) %>%
          pull(model_line) %>%
          paste(collapse = "\n")
      } else {
        # TODO: Implement loading from CFA module
        req(input$existing_model)
        model_spec <- "F1 =~ Q1 + Q2 + Q3" # Placeholder
      }
      
      # Create the grouping variable
      all_groups <- c(input$reference_group, input$comparison_groups)
      
      # Filter data to include only selected groups
      inv_data <- processed_data %>%
        filter(!!sym(input$selected_demographic) %in% all_groups)
      
      # Convert to wide format
      wide_data <- inv_data %>%
        dplyr::select(respondentID, rCode, response_value, !!sym(input$selected_demographic)) %>%
        pivot_wider(names_from = rCode, values_from = response_value) %>%
        # Create a group label with reference group as 1 and comparison as 2,3,etc.
        mutate(
          group = factor(
            !!sym(input$selected_demographic),
            levels = all_groups,
            labels = all_groups
          )
        ) %>%
        dplyr::select(-respondentID, -!!sym(input$selected_demographic))
      
      # Handle missing values based on setting
      if (input$missing_handling == "listwise") {
        wide_data <- na.omit(wide_data)
      } # FIML will be handled by lavaan automatically
      
      # Get mapping for interpretation
      item_mapping <- processed_data %>%
        dplyr::select(rCode, qText, constructName, constructID) %>%
        distinct()
      
      # Return all data needed for analysis
      list(
        data = wide_data,
        model_spec = model_spec,
        item_mapping = item_mapping,
        group_var = "group",
        group_levels = all_groups
      )
    })
    
    # Run invariance analysis when the button is clicked
    observeEvent(input$run_invariance, {
      req(prepareInvarianceData())
      
      # Get the prepared data
      inv_data <- prepareInvarianceData()
      
      withProgress(message = 'Running invariance analysis...', value = 0, {
        # Store results in a list
        all_results <- list()
        
        # Set up the model fitting approach
        estimator <- input$estimator
        missing <- ifelse(input$missing_handling == "fiml", "fiml", "listwise")
        scaled <- input$use_scaled
        std_loadings <- input$std_loadings
        
        # Get the sequence of models to test based on the approach
        if (input$invariance_approach == "sequential") {
          # Sequential testing - all levels in order
          invariance_levels <- c("configural", "metric", "scalar", "strict", "means")
        } else {
          # Selective testing - only selected levels
          invariance_levels <- input$sel_invariance_levels
        }
        
        # Track current progress
        total_steps <- length(invariance_levels)
        current_step <- 0
        
        # Run each invariance level
        for (level in invariance_levels) {
          current_step <- current_step + 1
          incProgress(amount = 1/total_steps, 
                      detail = paste("Testing", level, "invariance"))
          
          # Set up model constraints based on level
          group.equal <- NULL
          group.partial <- NULL
          group.mean <- NULL
          
          if (level == "configural") {
            # No constraints for configural model
          } else if (level == "metric") {
            group.equal <- c("loadings")
          } else if (level == "scalar") {
            group.equal <- c("loadings", "intercepts")
          } else if (level == "strict") {
            group.equal <- c("loadings", "intercepts", "residuals")
          } else if (level == "means") {
            group.equal <- c("loadings", "intercepts", "residuals")
            group.mean <- c()  # Estimate latent means for all groups except reference
          }
          
          # Fit the model
          tryCatch({
            fit <- cfa(
              model = inv_data$model_spec,
              data = inv_data$data,
              group = inv_data$group_var,
              estimator = estimator,
              missing = missing,
              group.equal = group.equal,
              group.partial = group.partial,
              group.mean = group.mean,
              std.lv = TRUE
            )
            
            # Store the result
            all_results[[level]] <- list(
              fit = fit,
              level = level,
              fit_measures = fitMeasures(fit, c("chisq", "df", "pvalue", 
                                                "cfi", "tli", "rmsea", "srmr", 
                                                "aic", "bic"))
            )
            
            # Get modification indices if requested
            if (level %in% c("metric", "scalar", "strict")) {
              all_results[[level]]$mi <- modificationIndices(fit)
            }
            
            # Get parameter estimates
            if (std_loadings) {
              all_results[[level]]$parameters <- standardizedSolution(fit)
            } else {
              all_results[[level]]$parameters <- parameterEstimates(fit)
            }
            
          }, error = function(e) {
            all_results[[level]] <- list(
              error = TRUE,
              error_message = paste("Error in", level, "model:", e$message)
            )
          })
        }
        
        # Calculate model comparisons if we have multiple successful models
        if (length(all_results) > 1) {
          # Set up comparisons list
          comparisons <- list()
          
          # Define the sequence of comparisons based on available models
          available_levels <- names(all_results)
          comparison_pairs <- list()
          
          if (all(c("configural", "metric") %in% available_levels)) {
            comparison_pairs <- c(comparison_pairs, list(c("configural", "metric")))
          }
          if (all(c("metric", "scalar") %in% available_levels)) {
            comparison_pairs <- c(comparison_pairs, list(c("metric", "scalar")))
          }
          if (all(c("scalar", "strict") %in% available_levels)) {
            comparison_pairs <- c(comparison_pairs, list(c("scalar", "strict")))
          }
          if (all(c("strict", "means") %in% available_levels)) {
            comparison_pairs <- c(comparison_pairs, list(c("strict", "means")))
          }
          
          # Perform each comparison
          for (pair in comparison_pairs) {
            baseline <- pair[1]
            comparison <- pair[2]
            
            # Check if both models exist and don't have errors
            if (!is.null(all_results[[baseline]]) && !is.null(all_results[[comparison]]) &&
                !isTRUE(all_results[[baseline]]$error) && !isTRUE(all_results[[comparison]]$error)) {
              
              # Run chi-square difference test
              if (scaled) {
                diff_test <- lavTestLRT(all_results[[baseline]]$fit, 
                                        all_results[[comparison]]$fit, 
                                        method = "satorra.bentler.2001")
              } else {
                diff_test <- anova(all_results[[baseline]]$fit, all_results[[comparison]]$fit)
              }
              
              # Calculate fit index differences
              fm_baseline <- all_results[[baseline]]$fit_measures
              fm_comparison <- all_results[[comparison]]$fit_measures
              
              cfi_diff <- abs(fm_baseline["cfi"] - fm_comparison["cfi"])
              rmsea_diff <- abs(fm_baseline["rmsea"] - fm_comparison["rmsea"])
              
              # Determine if invariance holds based on fit index changes
              cfi_invariance <- cfi_diff <= input$cutoff_cfi
              rmsea_invariance <- rmsea_diff <= input$cutoff_rmsea
              chisq_invariance <- ifelse(is.null(diff_test$`Pr(>Chisq)`), NA, 
                                         diff_test$`Pr(>Chisq)`[2] > 0.05)
              
              # Overall decision based on majority of criteria
              support_count <- sum(c(cfi_invariance, rmsea_invariance, chisq_invariance), na.rm = TRUE)
              total_criteria <- sum(!is.na(c(cfi_invariance, rmsea_invariance, chisq_invariance)))
              
              overall_invariance <- if (support_count >= ceiling(total_criteria/2)) {
                TRUE
              } else {
                FALSE
              }
              
              # Store comparison results
              comparisons[[paste(baseline, comparison, sep = "_vs_")]] <- list(
                baseline = baseline,
                comparison = comparison,
                diff_test = diff_test,
                cfi_diff = cfi_diff,
                rmsea_diff = rmsea_diff,
                cfi_invariance = cfi_invariance,
                rmsea_invariance = rmsea_invariance,
                chisq_invariance = chisq_invariance,
                overall_invariance = overall_invariance
              )
            }
          }
          
          # Add comparisons to results
          all_results$comparisons <- comparisons
        }
        
        # Calculate highest level of invariance achieved
        invariance_achieved <- "none"
        invariance_order <- c("none", "configural", "metric", "scalar", "strict", "means")
        
        for (level in rev(invariance_order)) {
          if (level == "none") {
            # Default if nothing else is achieved
            invariance_achieved <- level
            break
          }
          
          if (level == "configural") {
            # Configural is achieved if the model converged without errors
            if (!is.null(all_results[["configural"]]) && !isTRUE(all_results[["configural"]]$error)) {
              invariance_achieved <- level
              break
            }
          } else {
            # For other levels, check if comparison with previous level supports invariance
            prev_level <- invariance_order[which(invariance_order == level) - 1]
            comparison_key <- paste(prev_level, level, sep = "_vs_")
            
            if (!is.null(all_results$comparisons[[comparison_key]]) && 
                all_results$comparisons[[comparison_key]]$overall_invariance) {
              invariance_achieved <- level
              break
            }
          }
        }
        
        # Add metadata for interpretation
        all_results$metadata <- list(
          invariance_achieved = invariance_achieved,
          group_levels = inv_data$group_levels,
          item_mapping = inv_data$item_mapping,
          model_spec = inv_data$model_spec,
          demographic_var = input$selected_demographic,
          reference_group = input$reference_group,
          comparison_groups = input$comparison_groups
        )
        
        # Store the results
        invarianceResults(all_results)
      })
    })
    
    # Display analysis status
    output$analysisStatus <- renderUI({
      req(input$run_invariance > 0)
      
      results <- invarianceResults()
      
      if (is.null(results)) {
        div(class = "alert alert-info",
            icon("spinner", class = "fa-spin"),
            "Running measurement invariance analysis...")
      } else if (length(results) == 0) {
        div(class = "alert alert-danger",
            icon("triangle-exclamation"),
            "No valid results were generated. Please check your model specification and data.")
      } else {
        # Check for errors in results
        any_errors <- FALSE
        error_messages <- character(0)
        
        for (level in c("configural", "metric", "scalar", "strict", "means")) {
          if (!is.null(results[[level]]) && isTRUE(results[[level]]$error)) {
            any_errors <- TRUE
            error_messages <- c(error_messages, results[[level]]$error_message)
          }
        }
        
        if (any_errors) {
          div(class = "alert alert-warning",
              icon("triangle-exclamation"),
              "Analysis completed with some errors:",
              tags$ul(
                lapply(unique(error_messages), function(msg) tags$li(msg))
              ),
              "Results for successful models are still available in the tabs below.")
        } else {
          div(class = "alert alert-success",
              icon("check-circle"),
              "Measurement invariance analysis completed successfully!",
              "Review the results in the tabs below.")
        }
      }
    })
    
    # Render overview of results
    output$invarianceOverviewUI <- renderUI({
      req(invarianceResults())
      results <- invarianceResults()
      
      # Get the highest level of invariance achieved
      invariance_achieved <- results$metadata$invariance_achieved
      
      # Determine overall result message and style
      if (invariance_achieved == "none") {
        summary_class <- "alert-danger"
        summary_heading <- "No Measurement Invariance Established"
        summary_text <- "The analysis could not establish configural invariance, which is the minimum requirement for group comparisons."
      } else if (invariance_achieved == "configural") {
        summary_class <- "alert-warning"
        summary_heading <- "Configural Invariance Established"
        summary_text <- "The analysis established that the factor structure is similar across groups, but the strength of relationships between items and factors differs."
      } else if (invariance_achieved == "metric") {
        summary_class <- "alert-info"
        summary_heading <- "Metric Invariance Established"
        summary_text <- "The analysis established that items relate to factors with similar strength across groups. This supports comparing factor variances and covariances across groups."
      } else if (invariance_achieved == "scalar") {
        summary_class <- "alert-success"
        summary_heading <- "Scalar Invariance Established"
        summary_text <- "The analysis established that both factor loadings and intercepts are similar across groups. This supports comparing latent means across groups."
      } else if (invariance_achieved == "strict") {
        summary_class <- "alert-success"
        summary_heading <- "Strict Invariance Established"
        summary_text <- "The analysis established that factor loadings, intercepts, and residual variances are similar across groups. This is a strong form of invariance."
      } else if (invariance_achieved == "means") {
        summary_class <- "alert-success"
        summary_heading <- "Complete Invariance with Equal Latent Means"
        summary_text <- "The analysis established the strongest form of invariance, including equal latent means across groups."
      }
      
      # Create the overview UI
      tagList(
        div(class = paste("alert", summary_class),
            h4(summary_heading),
            p(summary_text),
            p("Groups compared: ", 
              strong(results$metadata$reference_group), " (reference) vs. ",
              paste(results$metadata$comparison_groups, collapse = ", ")),
            p("Demographic variable: ", em(results$metadata$demographic_var))
        ),
        
        h4("Invariance Testing Steps"),
        p("Measurement invariance is tested in a sequence of increasingly constrained models:"),
        
        div(class = "row",
            div(class = "col-md-12",
                tags$ul(class = "list-group",
                        tags$li(class = "list-group-item", 
                                strong("1. Configural Invariance: "), 
                                "Same factor structure across groups",
                                if (!is.null(results[["configural"]]) && !isTRUE(results[["configural"]]$error)) {
                                  span(class = "badge bg-success", "Supported")
                                } else {
                                  span(class = "badge bg-danger", "Not Supported")
                                }),
                        tags$li(class = "list-group-item", 
                                strong("2. Metric Invariance: "), 
                                "Equal factor loadings across groups",
                                if (invariance_achieved %in% c("metric", "scalar", "strict", "means")) {
                                  span(class = "badge bg-success", "Supported")
                                } else {
                                  span(class = "badge bg-danger", "Not Supported")
                                }),
                        tags$li(class = "list-group-item", 
                                strong("3. Scalar Invariance: "), 
                                "Equal intercepts across groups",
                                if (invariance_achieved %in% c("scalar", "strict", "means")) {
                                  span(class = "badge bg-success", "Supported")
                                } else {
                                  span(class = "badge bg-danger", "Not Supported")
                                }),
                        tags$li(class = "list-group-item", 
                                strong("4. Strict Invariance: "), 
                                "Equal residual variances across groups",
                                if (invariance_achieved %in% c("strict", "means")) {
                                  span(class = "badge bg-success", "Supported")
                                } else {
                                  span(class = "badge bg-danger", "Not Supported")
                                }),
                        tags$li(class = "list-group-item", 
                                strong("5. Equal Latent Means: "), 
                                "Equal latent factor means across groups",
                                if (invariance_achieved %in% c("means")) {
                                  span(class = "badge bg-success", "Supported")
                                } else {
                                  span(class = "badge bg-danger", "Not Supported")
                                })
                )
            )
        ),
        
        h4("Implications for Your Research"),
        div(class = "panel panel-default",
            div(class = "panel-body",
                if (invariance_achieved %in% c("none", "configural")) {
                  div(
                    p(strong("Limited Comparability: "), "Your survey items function differently across groups, making direct comparisons problematic."),
                    tags$ul(
                      tags$li("The meaning or interpretation of constructs may differ between groups."),
                      tags$li("Consider reviewing the questionnaire for items that may be interpreted differently by different groups."),
                      tags$li("Exploratory analyses may be done separately for each group, but direct comparisons are not supported.")
                    )
                  )
                } else if (invariance_achieved == "metric") {
                  div(
                    p(strong("Comparison of Relationships: "), "You can compare the relationships between constructs across groups."),
                    tags$ul(
                      tags$li("The strength of relationships (correlations, regression coefficients) between constructs can be meaningfully compared."),
                      tags$li("You cannot directly compare mean scores because intercepts differ across groups."),
                      tags$li("Examine factor covariances to understand how constructs relate within each group.")
                    )
                  )
                } else if (invariance_achieved %in% c("scalar", "strict", "means")) {
                  div(
                    p(strong("Full Comparability: "), "You can compare both relationships and mean levels across groups."),
                    tags$ul(
                      tags$li("Mean comparisons between groups are supported and can be interpreted meaningfully."),
                      tags$li("Differences in observed means reflect true differences in the underlying constructs."),
                      tags$li("Consider using multi-group structural equation modeling to test theoretical models across groups.")
                    )
                  )
                },
                div(class = "alert alert-info",
                    p(strong("Technical Note: "), "Researchers sometimes accept partial invariance when full invariance is not achieved. This involves freeing specific parameters that show group differences while constraining the rest. The 'Modification Indices' tab can help identify parameters that might be freed to improve model fit.")
                )
            )
        )
      )
    })
    
    # Render model comparison table
    output$modelComparisonTable <- renderTable({
      req(invarianceResults())
      results <- invarianceResults()
      
      # Create a table with all model fit indices
      model_fits <- data.frame(
        Model = character(),
        ChiSq = numeric(),
        df = numeric(),
        p = numeric(),
        CFI = numeric(),
        TLI = numeric(),
        RMSEA = numeric(),
        SRMR = numeric(),
        AIC = numeric(),
        BIC = numeric(),
        stringsAsFactors = FALSE
      )
      
      # Add each model's fit indices
      for (level in c("configural", "metric", "scalar", "strict", "means")) {
        if (!is.null(results[[level]]) && !isTRUE(results[[level]]$error)) {
          fit <- results[[level]]$fit_measures
          
          model_fits <- rbind(model_fits, data.frame(
            Model = level,
            ChiSq = fit["chisq"],
            df = fit["df"],
            p = fit["pvalue"],
            CFI = fit["cfi"],
            TLI = fit["tli"],
            RMSEA = fit["rmsea"],
            SRMR = fit["srmr"],
            AIC = fit["aic"],
            BIC = fit["bic"],
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # If we have comparisons, add them to the table
      if (!is.null(results$comparisons) && length(results$comparisons) > 0) {
        comparison_fits <- data.frame(
          Models = character(),
          ChiSq_diff = numeric(),
          df_diff = numeric(),
          p_diff = numeric(),
          CFI_diff = numeric(),
          RMSEA_diff = numeric(),
          Invariance = character(),
          stringsAsFactors = FALSE
        )
        
        for (comp_name in names(results$comparisons)) {
          comp <- results$comparisons[[comp_name]]
          
          # Extract chi-square difference test results
          if (input$use_scaled) {
            # For scaled chi-square
            chisq_diff <- comp$diff_test$`Chisq diff`[2]
            df_diff <- comp$diff_test$`Df diff`[2]
            p_diff <- comp$diff_test$`Pr(>Chisq)`[2]
          } else {
            # For regular chi-square
            chisq_diff <- comp$diff_test$`Chisq diff`[2]
            df_diff <- comp$diff_test$`Df diff`[2]
            p_diff <- comp$diff_test$`Pr(>Chisq)`[2]
          }
          
          # Determine invariance support text
          if (comp$overall_invariance) {
            inv_text <- "Supported"
          } else {
            inv_text <- "Not supported"
          }
          
          comparison_fits <- rbind(comparison_fits, data.frame(
            Models = paste(comp$baseline, "vs", comp$comparison),
            ChiSq_diff = chisq_diff,
            df_diff = df_diff,
            p_diff = p_diff,
            CFI_diff = comp$cfi_diff,
            RMSEA_diff = comp$rmsea_diff,
            Invariance = inv_text,
            stringsAsFactors = FALSE
          ))
        }
        
        # Return both tables
        knitr::kable(list(
          "Model Fit Indices" = model_fits,
          "Model Comparisons" = comparison_fits
        ), format = "html", digits = 3, row.names = FALSE) %>%
          kable_styling(bootstrap_options = c("striped", "hover"))
      } else {
        # Return just the model fits table
        knitr::kable(model_fits, format = "html", digits = 3, row.names = FALSE) %>%
          kable_styling(bootstrap_options = c("striped", "hover"))
      }
    }, sanitize.text.function = function(x) x)
    
    # Render comparison interpretation
    output$comparisonInterpretation <- renderUI({
      req(invarianceResults())
      results <- invarianceResults()
      
      # If we have comparisons, interpret them
      if (!is.null(results$comparisons) && length(results$comparisons) > 0) {
        interpretation_elements <- list()
        
        for (comp_name in names(results$comparisons)) {
          comp <- results$comparisons[[comp_name]]
          
          # Create alert class based on invariance support
          alert_class <- if (comp$overall_invariance) "alert-success" else "alert-warning"
          
          # Create interpretation text
          interp_text <- if (comp$overall_invariance) {
            paste("The data supports", comp$comparison, "invariance.")
          } else {
            paste("The data does not support", comp$comparison, "invariance.")
          }
          
          # Evidence summary
          evidence <- list()
          
          if (!is.na(comp$chisq_invariance)) {
            if (comp$chisq_invariance) {
              evidence <- c(evidence, tags$li("Chi-square difference test is non-significant (p > 0.05), supporting invariance."))
            } else {
              evidence <- c(evidence, tags$li("Chi-square difference test is significant (p < 0.05), suggesting non-invariance."))
            }
          }
          
          if (!is.na(comp$cfi_invariance)) {
            if (comp$cfi_invariance) {
              evidence <- c(evidence, tags$li(paste0("CFI change (", round(comp$cfi_diff, 4), 
                                                     ") is below the threshold of ", input$cutoff_cfi, 
                                                     ", supporting invariance.")))
            } else {
              evidence <- c(evidence, tags$li(paste0("CFI change (", round(comp$cfi_diff, 4), 
                                                     ") exceeds the threshold of ", input$cutoff_cfi, 
                                                     ", suggesting non-invariance.")))
            }
          }
          
          if (!is.na(comp$rmsea_invariance)) {
            if (comp$rmsea_invariance) {
              evidence <- c(evidence, tags$li(paste0("RMSEA change (", round(comp$rmsea_diff, 4), 
                                                     ") is below the threshold of ", input$cutoff_rmsea, 
                                                     ", supporting invariance.")))
            } else {
              evidence <- c(evidence, tags$li(paste0("RMSEA change (", round(comp$rmsea_diff, 4), 
                                                     ") exceeds the threshold of ", input$cutoff_rmsea, 
                                                     ", suggesting non-invariance.")))
            }
          }
          
          # Create the interpretation element
          interp_element <- div(class = paste("alert", alert_class),
                                h4(paste("Comparing", comp$baseline, "vs", comp$comparison)),
                                p(interp_text),
                                p("Evidence:"),
                                tags$ul(evidence),
                                if (!comp$overall_invariance) {
                                  p("Check the 'Modification Indices' tab to identify specific items or parameters causing non-invariance.")
                                } else {
                                  NULL
                                }
          )
          
          interpretation_elements <- c(interpretation_elements, list(interp_element))
        }
        
        # Add general recommendations
        interpretation_elements <- c(interpretation_elements, list(
          div(class = "alert alert-info",
              h4("General Interpretation Guidelines"),
              p("Measurement invariance testing evaluates whether survey items function similarly across different groups."),
              tags$ul(
                tags$li("Configural invariance: Same factor structure across groups"),
                tags$li("Metric invariance: Equal factor loadings across groups"),
                tags$li("Scalar invariance: Equal intercepts across groups"),
                tags$li("Strict invariance: Equal residual variances across groups")
              ),
              p("Scalar invariance is typically considered the minimum requirement for making meaningful mean comparisons across groups.")
          )
        ))
        
        # Return all interpretation elements
        do.call(tagList, interpretation_elements)
      } else {
        # If no comparisons, show a message
        div(class = "alert alert-warning",
            "No model comparisons are available. This might be because only one model was successfully fitted or all models resulted in errors.")
      }
    })
    
    # Update technical model selection choices
    observe({
      req(invarianceResults())
      results <- invarianceResults()
      
      # Get available models
      available_models <- character(0)
      
      for (level in c("configural", "metric", "scalar", "strict", "means")) {
        if (!is.null(results[[level]]) && !isTRUE(results[[level]]$error)) {
          available_models <- c(available_models, level)
        }
      }
      
      # Update selection choices
      updateSelectInput(session, "tech_model", 
                        choices = available_models,
                        selected = if(length(available_models) > 0) available_models[1] else NULL)
      
      updateSelectInput(session, "mi_model", 
                        choices = available_models,
                        selected = if(length(available_models) > 0) available_models[1] else NULL)
    })
    
    # Technical output - model summary
    output$technicalOutput <- renderPrint({
      req(invarianceResults(), input$tech_model)
      results <- invarianceResults()
      
      # Check if the selected model exists and has no errors
      if (!is.null(results[[input$tech_model]]) && !isTRUE(results[[input$tech_model]]$error)) {
        # Print full model summary
        cat("MODEL SUMMARY FOR:", toupper(input$tech_model), "INVARIANCE\n\n")
        summary(results[[input$tech_model]]$fit, fit.measures = TRUE, standardized = TRUE)
      } else {
        cat("Model summary not available for", input$tech_model, "invariance.")
      }
    })
    
    # Modification indices table
    output$modIndicesTable <- DT::renderDataTable({
      req(invarianceResults(), input$mi_model)
      results <- invarianceResults()
      
      # Check if the selected model exists and has modification indices
      if (!is.null(results[[input$mi_model]]) && 
          !isTRUE(results[[input$mi_model]]$error) && 
          !is.null(results[[input$mi_model]]$mi)) {
        
        # Get modification indices
        mi_data <- results[[input$mi_model]]$mi
        
        # Filter based on threshold
        mi_data <- mi_data[mi_data$mi >= input$mi_threshold, ]
        
        # Sort by mi value
        mi_data <- mi_data[order(-mi_data$mi), ]
        
        # Format the table
        DT::datatable(
          mi_data,
          options = list(
            pageLength = 25,
            scrollX = TRUE
          ),
          caption = paste("Modification Indices for", input$mi_model, "invariance model"),
          rownames = FALSE
        ) %>%
          formatRound(columns = c("mi", "epc"), digits = 3)
      } else {
        # Return empty table with message
        DT::datatable(
          data.frame(Message = "No modification indices available for this model."),
          options = list(dom = 't'),
          rownames = FALSE
        )
      }
    })
    
    # Parameter table
    output$parameterTable <- DT::renderDataTable({
      req(invarianceResults(), input$parameter_type)
      results <- invarianceResults()
      
      # Find the most constrained model that has parameters
      model_order <- c("means", "strict", "scalar", "metric", "configural")
      param_model <- NULL
      
      for (level in model_order) {
        if (!is.null(results[[level]]) && 
            !isTRUE(results[[level]]$error) && 
            !is.null(results[[level]]$parameters)) {
          param_model <- level
          break
        }
      }
      
      if (is.null(param_model)) {
        # Return empty table with message
        return(DT::datatable(
          data.frame(Message = "No parameter estimates available."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Get parameters
      params <- results[[param_model]]$parameters
      
      # Filter by parameter type
      if (input$parameter_type == "loadings") {
        param_data <- params[params$op == "=~", ]
      } else if (input$parameter_type == "intercepts") {
        param_data <- params[params$op == "~1" & params$lhs %in% unique(params$rhs[params$op == "=~"]), ]
      } else if (input$parameter_type == "residuals") {
        param_data <- params[params$op == "~~" & params$lhs == params$rhs & 
                               params$lhs %in% unique(params$rhs[params$op == "=~"]), ]
      } else if (input$parameter_type == "factor_var") {
        param_data <- params[params$op == "~~" & params$lhs == params$rhs & 
                               !(params$lhs %in% unique(params$rhs[params$op == "=~"])), ]
      } else if (input$parameter_type == "factor_cov") {
        param_data <- params[params$op == "~~" & params$lhs != params$rhs & 
                               !(params$lhs %in% unique(params$rhs[params$op == "=~"])), ]
      } else if (input$parameter_type == "means") {
        param_data <- params[params$op == "~1" & 
                               !(params$lhs %in% unique(params$rhs[params$op == "=~"])), ]
      }
      
      # Extract item mapping
      item_mapping <- results$metadata$item_mapping
      
      # Join with item mapping to get question text
      if (!is.null(item_mapping) && nrow(item_mapping) > 0) {
        if (input$parameter_type == "loadings") {
          # For loadings, join by the right-hand side
          param_data <- merge(param_data, item_mapping, 
                              by.x = "rhs", by.y = "rCode", 
                              all.x = TRUE, sort = FALSE)
        } else if (input$parameter_type == "intercepts" || input$parameter_type == "residuals") {
          # For intercepts, join by the left-hand side
          param_data <- merge(param_data, item_mapping, 
                              by.x = "lhs", by.y = "rCode", 
                              all.x = TRUE, sort = FALSE)
        }
      }
      
      # Format for display
      DT::datatable(
        param_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE
        ),
        caption = paste("Parameter Estimates for", input$parameter_type, "from", param_model, "model"),
        rownames = FALSE
      ) %>%
        formatRound(columns = c("est", "se", "z", "pvalue", "std.all", "std.lv", "std.nox"), digits = 3)
    })
    
    # Parameter comparison plot
    output$parameterPlot <- renderPlot({
      req(invarianceResults(), input$parameter_type)
      results <- invarianceResults()
      
      # Find a model that has parameters and is appropriate for group comparison
      param_model <- NULL
      
      if (input$parameter_type == "loadings") {
        # For loadings, use configural or metric
        for (level in c("configural", "metric")) {
          if (!is.null(results[[level]]) && 
              !isTRUE(results[[level]]$error) && 
              !is.null(results[[level]]$parameters)) {
            param_model <- level
            break
          }
        }
      } else if (input$parameter_type == "intercepts") {
        # For intercepts, use configural or scalar
        for (level in c("configural", "scalar")) {
          if (!is.null(results[[level]]) && 
              !isTRUE(results[[level]]$error) && 
              !is.null(results[[level]]$parameters)) {
            param_model <- level
            break
          }
        }
      } else {
        # For other parameters, use any model
        for (level in c("configural", "metric", "scalar", "strict", "means")) {
          if (!is.null(results[[level]]) && 
              !isTRUE(results[[level]]$error) && 
              !is.null(results[[level]]$parameters)) {
            param_model <- level
            break
          }
        }
      }
      
      if (is.null(param_model)) {
        # Return empty plot with message
        plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", 
             xlim = c(0, 1), ylim = c(0, 1))
        text(0.5, 0.5, "No parameter estimates available for plotting", cex = 1.5)
        return()
      }
      
      # Get parameters
      params <- results[[param_model]]$parameters
      
      # Get group levels
      group_levels <- results$metadata$group_levels
      
      # Filter by parameter type
      if (input$parameter_type == "loadings") {
        param_data <- params[params$op == "=~", ]
      } else if (input$parameter_type == "intercepts") {
        param_data <- params[params$op == "~1" & params$lhs %in% unique(params$rhs[params$op == "=~"]), ]
      } else if (input$parameter_type == "residuals") {
        param_data <- params[params$op == "~~" & params$lhs == params$rhs & 
                               params$lhs %in% unique(params$rhs[params$op == "=~"]), ]
      } else if (input$parameter_type == "factor_var") {
        param_data <- params[params$op == "~~" & params$lhs == params$rhs & 
                               !(params$lhs %in% unique(params$rhs[params$op == "=~"])), ]
      } else if (input$parameter_type == "factor_cov") {
        param_data <- params[params$op == "~~" & params$lhs != params$rhs & 
                               !(params$lhs %in% unique(params$rhs[params$op == "=~"])), ]
      } else if (input$parameter_type == "means") {
        param_data <- params[params$op == "~1" & 
                               !(params$lhs %in% unique(params$rhs[params$op == "=~"])), ]
      }
      
      # If we have group column, process for plotting
      if ("group" %in% names(param_data)) {
        # For plotting, need to create a data frame with: parameter, group, estimate, ci.lower, ci.upper
        plot_data <- param_data %>%
          mutate(
            parameter = case_when(
              input$parameter_type == "loadings" ~ paste(lhs, "=~", rhs),
              input$parameter_type == "intercepts" ~ paste(lhs, "~1"),
              input$parameter_type == "residuals" ~ paste(lhs, "~~", rhs),
              input$parameter_type == "factor_var" ~ paste(lhs, "~~", rhs),
              input$parameter_type == "factor_cov" ~ paste(lhs, "~~", rhs),
              input$parameter_type == "means" ~ paste(lhs, "~1"),
              TRUE ~ paste(lhs, op, rhs)
            ),
            ci.lower = est - 1.96 * se,
            ci.upper = est + 1.96 * se
          ) %>%
          dplyr::select(parameter, group, est, ci.lower, ci.upper)
        
        # Convert group to factor with levels matching the original groups
        plot_data$group <- factor(plot_data$group, levels = as.character(1:length(group_levels)))
        
        # Create a more readable parameter label
        item_mapping <- results$metadata$item_mapping
        if (!is.null(item_mapping) && nrow(item_mapping) > 0) {
          # Function to create readable labels
          create_label <- function(param) {
            parts <- strsplit(param, " ")[[1]]
            
            if (length(parts) >= 3) {
              lhs <- parts[1]
              op <- parts[2]
              rhs <- parts[3]
              
              # For loadings, use the item text if available
              if (op == "=~" && rhs %in% item_mapping$rCode) {
                item_info <- item_mapping[item_mapping$rCode == rhs, ]
                if (nrow(item_info) > 0 && !is.na(item_info$qText[1])) {
                  # Truncate question text if too long
                  q_text <- item_info$qText[1]
                  if (nchar(q_text) > 30) {
                    q_text <- paste0(substr(q_text, 1, 27), "...")
                  }
                  return(paste0(lhs, " → ", q_text))
                }
              }
            }
            
            # Default: return the original parameter
            return(param)
          }
          
          # Apply the labeling function
          plot_data$parameter_label <- sapply(plot_data$parameter, create_label)
        } else {
          plot_data$parameter_label <- plot_data$parameter
        }
        
        # Convert parameter to factor ordered by the mean estimate
        param_order <- plot_data %>%
          group_by(parameter) %>%
          summarize(mean_est = mean(est, na.rm = TRUE)) %>%
          arrange(mean_est) %>%
          pull(parameter)
        
        plot_data$parameter <- factor(plot_data$parameter, levels = param_order)
        plot_data$parameter_label <- factor(plot_data$parameter_label, 
                                            levels = unique(plot_data$parameter_label[match(param_order, plot_data$parameter)]))
        
        # Create the plot
        ggplot(plot_data, aes(x = est, y = parameter_label, color = group)) +
          geom_point(position = position_dodge(width = 0.5), size = 3) +
          geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), 
                         position = position_dodge(width = 0.5), 
                         height = 0.2, alpha = 0.7) +
          scale_color_brewer(palette = "Set1", 
                             name = "Group", 
                             labels = group_levels) +
          labs(title = paste("Group Comparison of", tools::toTitleCase(input$parameter_type)),
               subtitle = paste("From", tools::toTitleCase(param_model), "Model with 95% Confidence Intervals"),
               x = "Estimate",
               y = NULL) +
          theme_minimal() +
          theme(axis.text.y = element_text(hjust = 0),
                legend.position = "bottom",
                panel.grid.minor = element_blank())
      } else {
        # If no group column, show message
        plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", 
             xlim = c(0, 1), ylim = c(0, 1))
        text(0.5, 0.5, "Group comparison not available for this parameter type in the selected model", cex = 1.2)
      }
    })
  })
}