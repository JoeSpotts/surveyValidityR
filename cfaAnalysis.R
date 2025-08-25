# modules/cfaAnalysis.R

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

# ---------------------- UI Function ------------------------------- #
# ---------------------- UI Function ------------------------------- #
cfaAnalysisUI <- function(id, label = "Confirmatory Factor Analysis") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             wellPanel(
               h3("Confirmatory Factor Analysis (CFA)"),
               p("CFA tests whether the data fits a hypothesized measurement model. Unlike exploratory factor analysis, 
                 CFA starts with a predefined factor structure based on theory or previous research."),
               
               # Tabbed interface for model setup
               tabsetPanel(
                 id = ns("model_setup_tabs"),
                 
                 # Tab 1: Model Type Selection
                 tabPanel("Model Type",
                          div(class = "row",
                              div(class = "col-md-4",
                                  radioButtons(ns("model_type"), "Select Model Type:",
                                               choices = c("Default Factor Model (based on ConstructID)" = "default_factor",
                                                           "Single-Factor Model" = "single_factor",
                                                           "Custom Factor Model" = "custom",
                                                           "Compare Models" = "compare"),
                                               selected = "default_factor")
                              ),
                              div(class = "col-md-8",
                                  conditionalPanel(
                                    condition = "input.model_type == 'custom'",
                                    ns = ns,
                                    p("Define your custom factor model by selecting which constructs to include:"),
                                    uiOutput(ns("constructSelectionUI"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.model_type == 'compare'",
                                    ns = ns,
                                    p("Select which models to compare:"),
                                    checkboxInput(ns("compare_six"), "Include Six-Factor Model", TRUE),
                                    checkboxInput(ns("compare_single"), "Include Single-Factor Model", TRUE),
                                    checkboxInput(ns("compare_custom"), "Include Custom Factor Model", FALSE),
                                    conditionalPanel(
                                      condition = "input.compare_custom == true",
                                      ns = ns,
                                      uiOutput(ns("compareConstructSelectionUI"))
                                    )
                                  )
                              )
                          )
                 ),
                 
                 # Tab 2: Advanced Options
                 tabPanel("Advanced Options",
                          div(class = "row",
                              div(class = "col-md-4",
                                  selectInput(ns("estimator"), "Estimation Method:",
                                              choices = c("Maximum Likelihood" = "ML",
                                                          "Robust Maximum Likelihood" = "MLR",
                                                          "Diagonally Weighted Least Squares" = "DWLS"),
                                              selected = "ML")
                              ),
                              div(class = "col-md-4",
                                  numericInput(ns("std_cutoff"), "Standardized Loading Cutoff for Highlighting:",
                                               value = 0.4, min = 0.1, max = 0.9, step = 0.05)
                              ),
                              div(class = "col-md-4",
                                  checkboxInput(ns("show_residuals"), "Show Residuals in Path Diagram", FALSE),
                                  checkboxInput(ns("show_covs"), "Show Covariances Between Factors", TRUE)
                              )
                          )
                 )
               ),
               
               div(class = "row", style = "margin-top: 15px;",
                   div(class = "col-md-12", 
                       div(style = "text-align: center;",
                           actionButton(ns("run_cfa"), "Run CFA Analysis", 
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
      condition = "input.run_cfa > 0",
      ns = ns,
      
      fluidRow(
        column(12,
               tabsetPanel(
                 id = ns("cfa_tabs"),
                 tabPanel("Model Fit",
                          wellPanel(
                            h3("Model Fit Indices"),
                            div(class = "row",
                                div(class = "col-md-12", uiOutput(ns("fitIndicesUI")))
                            ),
                            div(class = "row",
                                div(class = "col-md-12", 
                                    tags$h4("Interpretation of Fit Indices"),
                                    htmlOutput(ns("fitIndicesInterpretation")))
                            )
                          )
                 ),
                 tabPanel("Loading Analysis",
                          wellPanel(
                            h3("Standardized Factor Loadings"),
                            p("This table shows how strongly each item (survey question) relates to its respective factor. 
                              Standardized factor loadings can be interpreted as the correlation between the observed variable and the latent factor."),
                            
                            div(class = "row",
                                div(class = "col-md-7",
                                    h4("Interpreting Factor Loadings"),
                                    p("Factor loadings range from -1 to 1, with the following general interpretation guidelines:"),
                                    tags$ul(
                                      tags$li(strong("Excellent (≥ 0.70): "), "Very strong relationship between item and factor. These items are central to the construct definition."),
                                      tags$li(strong("Good (0.50 - 0.69): "), "Strong relationship. These items clearly represent the construct."),
                                      tags$li(strong("Acceptable (0.40 - 0.49): "), "Moderate relationship. These items adequately represent aspects of the construct."),
                                      tags$li(strong("Poor (< 0.40): "), "Weak relationship. These items may not be good indicators of the construct."),
                                      tags$li(strong("Cross-loading: "), "Item loads more strongly on a factor other than its intended construct.")
                                    ),
                                    
                                    h4("Loading Status Classification"),
                                    p("The 'Loading Status' column classifies each item based on its loading magnitude and whether it loads on its theoretically expected factor:"),
                                    tags$ul(
                                      tags$li(strong("Excellent: "), "Item loads on its expected factor with a standardized loading ≥ 0.70"),
                                      tags$li(strong("Good: "), "Item loads on its expected factor with a standardized loading between 0.50 and 0.69"),
                                      tags$li(strong("Acceptable: "), "Item loads on its expected factor with a standardized loading between 0.40 and 0.49"),
                                      tags$li(strong("Cross-loading: "), "Item loads more strongly on a factor other than its expected factor with a loading ≥ 0.40"),
                                      tags$li(strong("Poor loading: "), "Item's highest loading is < 0.40 or does not load appropriately on any factor")
                                    )
                                ),
                                
                                div(class = "col-md-5",
                                    div(class = "panel panel-default",
                                        div(class = "panel-heading", h4("Scholarly References")),
                                        div(class = "panel-body",
                                            tags$ul(
                                              tags$li(strong("Hair et al. (2010): "), "Suggest loadings > 0.50 as practically significant, with loadings ≥ 0.70 being ideal for well-defined constructs."),
                                              tags$li(strong("Tabachnick & Fidell (2013): "), "Recommend 0.32 as minimum loading, 0.45 as fair, 0.55 as good, 0.63 as very good, and 0.71 as excellent."),
                                              tags$li(strong("Comrey & Lee (1992): "), "Classified loadings as: 0.71 (excellent), 0.63 (very good), 0.55 (good), 0.45 (fair), and 0.32 (poor)."),
                                              tags$li(strong("Kline (2016): "), "Recommends standardized loadings of 0.70 or higher for clear factor definition.")
                                            ),
                                            tags$hr(),
                                            strong("Full Citations:"),
                                            tags$ul(
                                              tags$li("Hair, J. F., Black, W. C., Babin, B. J., & Anderson, R. E. (2010). Multivariate data analysis (7th ed.). Pearson."),
                                              tags$li("Tabachnick, B. G., & Fidell, L. S. (2013). Using multivariate statistics (6th ed.). Pearson."),
                                              tags$li("Comrey, A. L., & Lee, H. B. (1992). A first course in factor analysis (2nd ed.). Lawrence Erlbaum Associates."),
                                              tags$li("Kline, R. B. (2016). Principles and practice of structural equation modeling (4th ed.). Guilford Press.")
                                            )
                                        )
                                    ),
                                    div(class = "alert alert-info",
                                        h4("How to Use This Information"),
                                        p("Items with 'Excellent' or 'Good' status strongly define their respective constructs and can be confidently retained."),
                                        p("'Acceptable' items contribute to the construct but might benefit from refinement in future survey iterations."),
                                        p("'Cross-loading' items may indicate conceptual overlap between constructs, suggesting a need to re-examine the theoretical model."),
                                        p("'Poor loading' items should be considered for removal or substantial revision, as they may not effectively measure their intended constructs.")
                                    )
                                )
                            ),
                            
                            div(class = "row", style = "margin-top: 20px;",
                                div(class = "col-md-12", uiOutput(ns("factorLoadingsUI")))
                            )
                          )
                 ),
                 tabPanel("Path Diagram",
                          wellPanel(
                            h3("Visual Representation of Factor Structure"),
                            p("This diagram displays the relationships between latent factors (circles) and observed variables (rectangles)."),
                            div(class = "row",
                                div(class = "col-md-12", 
                                    div(style = "text-align: center;",
                                        withSpinner(plotOutput(ns("pathDiagram"), height = "600px")))
                                )
                            ),
                            div(class = "row",
                                div(class = "col-md-12",
                                    tags$br(),
                                    wellPanel(
                                      h4("Understanding Path Diagrams"),
                                      tags$ul(
                                        tags$li(strong("Circles:"), "Represent latent factors (unobserved constructs)"),
                                        tags$li(strong("Rectangles:"), "Represent manifest variables (observed survey items)"),
                                        tags$li(strong("Single-headed arrows:"), "Factor loadings (from latent to manifest)"),
                                        tags$li(strong("Double-headed arrows:"), "Covariances between factors")
                                      ),
                                      p("The numbers on the arrows represent standardized coefficients. Higher values indicate stronger relationships.")
                                    )
                                )
                            )
                          )
                 ),
                 tabPanel("Model Comparison",
                          conditionalPanel(
                            condition = "input.model_type == 'compare'",
                            ns = ns,
                            wellPanel(
                              h3("Comparison of Models"),
                              p("This comparison helps determine which model structure best fits the data."),
                              div(class = "row",
                                  div(class = "col-md-12", 
                                      div(style = "overflow-x: auto;",
                                          tableOutput(ns("modelComparisonTable")))
                                  )
                              ),
                              div(class = "row",
                                  div(class = "col-md-12",
                                      tags$br(),
                                      uiOutput(ns("modelComparisonInterpretation"))
                                  )
                              )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.model_type != 'compare'",
                            ns = ns,
                            wellPanel(
                              h3("Model Comparison"),
                              p("To compare different model structures, please select 'Compare Models' from the options and run the analysis again.")
                            )
                          )
                 ),
                 tabPanel("Technical Details",
                          wellPanel(
                            h3("Technical Model Details"),
                            h4("Model Specification"),
                            verbatimTextOutput(ns("modelSpecification")),
                            tags$br(),
                            h4("Summary Statistics"),
                            verbatimTextOutput(ns("modelSummary"))
                          )
                 )
               )
        )
      )
    )
  )
}

# ---------------------- Server Function --------------------------- #
cfaAnalysisServer <- function(id, adjustedData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get available constructs for selection
    # Find the availableConstructs reactive function in cfaAnalysis.R and replace with:
    # Find the availableConstructs reactive function in cfaAnalysis.R and replace with:
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
    
    # Render construct selection UI for custom model
    output$constructSelectionUI <- renderUI({
      req(availableConstructs())
      constructs <- availableConstructs()
      
      div(
        selectizeInput(
          ns("selected_constructs"),
          "Select Constructs to Include:",
          choices = setNames(constructs$constructID, constructs$constructName),
          selected = constructs$constructID,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        ),
        checkboxInput(
          ns("show_item_selection"),
          "Customize individual items for each construct",
          FALSE
        ),
        conditionalPanel(
          condition = "input.show_item_selection == true",
          ns = ns,
          uiOutput(ns("itemSelectionUI"))
        )
      )
    })
    
    # Render construct selection UI for comparison
    output$compareConstructSelectionUI <- renderUI({
      req(availableConstructs())
      constructs <- availableConstructs()
      
      selectizeInput(
        ns("compare_selected_constructs"),
        "Select Constructs for Custom Model:",
        choices = setNames(constructs$constructID, constructs$constructName),
        selected = constructs$constructID,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })
    
    # Render item selection UI based on selected constructs
    output$itemSelectionUI <- renderUI({
      req(adjustedData(), input$selected_constructs)
      processed_data <- adjustedData()
      
      # Filter items for selected constructs
      items_by_construct <- processed_data %>%
        filter(constructID %in% input$selected_constructs) %>%
        dplyr::select(constructID, constructName, rCode, qText) %>%
        distinct() %>%
        arrange(constructName, qText)
      
      # Group items by construct
      constructs_list <- split(items_by_construct, items_by_construct$constructName)
      
      # Create a selection UI for each construct
      construct_uis <- lapply(names(constructs_list), function(construct_name) {
        construct_items <- constructs_list[[construct_name]]
        
        div(
          h4(construct_name),
          selectizeInput(
            ns(paste0("items_", gsub("[^A-Za-z0-9]", "", construct_name))),
            "Select Items:",
            choices = setNames(construct_items$rCode, construct_items$qText),
            selected = construct_items$rCode,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          )
        )
      })
      
      # Combine all UIs
      do.call(tagList, construct_uis)
    })
    
    # Get selected items for all constructs
    selectedItems <- reactive({
      req(adjustedData(), input$selected_constructs)
      processed_data <- adjustedData()
      
      if (!input$show_item_selection) {
        # If not customizing items, include all items for selected constructs
        items <- processed_data %>%
          filter(constructID %in% input$selected_constructs) %>%
          dplyr::select(constructID, constructName, rCode, qText) %>%
          distinct()
        
        return(items)
      } else {
        # Get all constructs and their items
        all_items <- processed_data %>%
          filter(constructID %in% input$selected_constructs) %>%
          dplyr::select(constructID, constructName, rCode, qText) %>%
          distinct()
        
        # For each construct, get the selected items
        constructs_list <- split(all_items, all_items$constructName)
        
        selected_items <- lapply(names(constructs_list), function(construct_name) {
          input_id <- paste0("items_", gsub("[^A-Za-z0-9]", "", construct_name))
          
          # Check if this input exists
          if (is.null(input[[input_id]])) {
            return(NULL)
          }
          
          # Filter items for this construct to include only selected ones
          construct_items <- constructs_list[[construct_name]]
          selected <- construct_items %>%
            filter(rCode %in% input[[input_id]])
          
          return(selected)
        })
        
        # Combine results and remove NULLs
        selected_items <- do.call(rbind, selected_items[!sapply(selected_items, is.null)])
        
        return(selected_items)
      }
    })
    
    # Creating a reactive to hold processed data for CFA
    cfaData <- eventReactive(input$run_cfa, {
      req(adjustedData())
      
      withProgress(message = "Preparing data for CFA...", value = 0, {
        incProgress(0.2, detail = "Selecting relevant columns")
        
        # Get the processed data
        processed_data <- adjustedData()
        
        # Create wide format data for CFA
        data_wide <- processed_data %>%
          dplyr::select(respondentID, rCode, response_value) %>%
          pivot_wider(names_from = rCode, values_from = response_value)
        
        incProgress(0.2, detail = "Handling missing values")
        
        # Handle missing values
        data_cfa <- na.omit(data_wide) %>%
          dplyr::select(-respondentID)
        
        incProgress(0.2, detail = "Creating model specifications")
        
        # Generate model specifications for default factor model
        model_specs_six <- processed_data %>%
          dplyr::select(constructID, rCode) %>%
          distinct() %>%
          group_by(constructID) %>%
          summarise(indicators = paste(rCode, collapse = " + ")) %>%
          ungroup() %>%
          mutate(construct_label = paste0("F", sub("^C", "", constructID)),  # Create factor labels
                 model_line = paste(construct_label, "=~", indicators)) %>%
          pull(model_line)
        
        # Create six-factor model string
        cfa_model_default_factor <- paste(model_specs_six, collapse = "\n")
        
        # Create single-factor model string
        model_specs_single <- processed_data %>%
          dplyr::select(rCode) %>%
          distinct() %>%
          summarise(indicators = paste(rCode, collapse = " + ")) %>%
          mutate(construct_label = "F1",  # Single factor
                 model_line = paste(construct_label, "=~", indicators)) %>%
          pull(model_line)
        
        # Create custom model string if applicable
        cfa_model_custom <- NULL
        if (input$model_type == "custom" || (input$model_type == "compare" && input$compare_custom)) {
          if (input$model_type == "custom") {
            items <- selectedItems()
          } else {
            # For comparison mode, use simplified selection
            items <- processed_data %>%
              filter(constructID %in% input$compare_selected_constructs) %>%
              dplyr::select(constructID, constructName, rCode, qText) %>%
              distinct()
          }
          
          if (nrow(items) > 0) {
            model_specs_custom <- items %>%
              group_by(constructID) %>%
              summarise(indicators = paste(rCode, collapse = " + ")) %>%
              ungroup() %>%
              mutate(construct_label = paste0("F", sub("^C", "", constructID)),
                     model_line = paste(construct_label, "=~", indicators)) %>%
              pull(model_line)
            
            cfa_model_custom <- paste(model_specs_custom, collapse = "\n")
          }
        }
        
        incProgress(0.2, detail = "Creating mapping for interpretation")
        
        # Create variable mapping for interpretation
        var_mapping <- processed_data %>%
          dplyr::select(rCode, qText, constructName, constructID) %>%
          distinct()
        
        incProgress(0.2, detail = "Done")
        
        # Return all the necessary components
        list(
          data_cfa = data_cfa,
          cfa_model_default_factor = cfa_model_default_factor,
          model_specs_single = model_specs_single,
          cfa_model_custom = cfa_model_custom,
          var_mapping = var_mapping
        )
      })
    })
    
    # Running the CFA models based on user selection
    cfaResults <- reactive({
      req(cfaData())
      
      # Get the model type selection
      model_type <- input$model_type
      estimator <- input$estimator
      
      withProgress(message = "Running CFA analysis...", value = 0, {
        # Initialize results
        fit_cfa_six <- NULL
        fit_cfa_single <- NULL
        fit_cfa_custom <- NULL
        
        # Run the selected model(s)
        if (model_type == "default_factor" || 
            (model_type == "compare" && input$compare_six)) {
          incProgress(0.3, detail = "Fitting six-factor model")
          fit_cfa_six <- try(
            lavaan::cfa(cfaData()$cfa_model_default_factor, 
                        data = cfaData()$data_cfa,
                        estimator = estimator),
            silent = TRUE
          )
        }
        
        if (model_type == "single_factor" || 
            (model_type == "compare" && input$compare_single)) {
          incProgress(0.3, detail = "Fitting single-factor model")
          fit_cfa_single <- try(
            lavaan::cfa(cfaData()$model_specs_single, 
                        data = cfaData()$data_cfa,
                        estimator = estimator),
            silent = TRUE
          )
        }
        
        if ((model_type == "custom" || 
             (model_type == "compare" && input$compare_custom)) && 
            !is.null(cfaData()$cfa_model_custom)) {
          incProgress(0.3, detail = "Fitting custom model")
          fit_cfa_custom <- try(
            lavaan::cfa(cfaData()$cfa_model_custom, 
                        data = cfaData()$data_cfa,
                        estimator = estimator),
            silent = TRUE
          )
        }
        
        incProgress(0.4, detail = "Finalizing results")
        
        # Return the results
        list(
          fit_cfa_six = fit_cfa_six,
          fit_cfa_single = fit_cfa_single,
          fit_cfa_custom = fit_cfa_custom
        )
      })
    })
    
    # Display analysis status
    output$analysisStatus <- renderUI({
      if (input$run_cfa == 0) return(NULL)
      
      results <- cfaResults()
      model_type <- input$model_type
      
      # Check for errors in each model
      errors <- c()
      
      if ((model_type == "default_factor" || 
           (model_type == "compare" && input$compare_six)) && 
          inherits(results$fit_cfa_six, "try-error")) {
        errors <- c(errors, paste("Six-factor model:", gsub("^Error.*?: ", "", results$fit_cfa_six[1])))
      }
      
      if ((model_type == "single_factor" || 
           (model_type == "compare" && input$compare_single)) && 
          inherits(results$fit_cfa_single, "try-error")) {
        errors <- c(errors, paste("Single-factor model:", gsub("^Error.*?: ", "", results$fit_cfa_single[1])))
      }
      
      if ((model_type == "custom" || 
           (model_type == "compare" && input$compare_custom)) && 
          !is.null(results$fit_cfa_custom) &&
          inherits(results$fit_cfa_custom, "try-error")) {
        errors <- c(errors, paste("Custom model:", gsub("^Error.*?: ", "", results$fit_cfa_custom[1])))
      }
      
      # Handle comparison case with no models selected
      if (model_type == "compare" && 
          !input$compare_six && !input$compare_single && 
          (!input$compare_custom || is.null(cfaData()$cfa_model_custom))) {
        errors <- c(errors, "No models selected for comparison. Please select at least one model.")
      }
      
      # Display errors if any
      if (length(errors) > 0) {
        div(class = "alert alert-danger",
            icon("triangle-exclamation"),
            "Errors occurred during analysis:", 
            tags$ul(
              lapply(errors, function(error) tags$li(error))
            )
        )
      } else {
        div(class = "alert alert-success",
            icon("check-circle"),
            "Analysis completed successfully!")
      }
    })
    
    # Helper function to classify fit indices with color coding
    classifyFitIndex <- function(index_name, index_value) {
      if (index_name == "cfi" || index_name == "tli") {
        if (index_value >= 0.95) {
          list(class = "text-success", text = "Excellent", value = round(index_value, 3))
        } else if (index_value >= 0.90) {
          list(class = "text-primary", text = "Good", value = round(index_value, 3))
        } else if (index_value >= 0.80) {
          list(class = "text-warning", text = "Marginal", value = round(index_value, 3))
        } else {
          list(class = "text-danger", text = "Poor", value = round(index_value, 3))
        }
      } else if (index_name == "rmsea") {
        if (index_value <= 0.05) {
          list(class = "text-success", text = "Excellent", value = round(index_value, 3))
        } else if (index_value <= 0.08) {
          list(class = "text-primary", text = "Good", value = round(index_value, 3))
        } else if (index_value <= 0.10) {
          list(class = "text-warning", text = "Marginal", value = round(index_value, 3))
        } else {
          list(class = "text-danger", text = "Poor", value = round(index_value, 3))
        }
      } else if (index_name == "srmr") {
        if (index_value <= 0.05) {
          list(class = "text-success", text = "Excellent", value = round(index_value, 3))
        } else if (index_value <= 0.08) {
          list(class = "text-primary", text = "Good", value = round(index_value, 3))
        } else if (index_value <= 0.10) {
          list(class = "text-warning", text = "Marginal", value = round(index_value, 3))
        } else {
          list(class = "text-danger", text = "Poor", value = round(index_value, 3))
        }
      } else if (index_name == "pvalue") {
        if (index_value > 0.05) {
          list(class = "text-success", text = "Good (non-significant)", value = round(index_value, 3))
        } else {
          # For large samples, a significant chi-square is common
          list(class = "text-warning", text = "Significant", value = round(index_value, 3))
        }
      } else {
        list(class = "", text = "", value = round(index_value, 3))
      }
    }
    
    # Helper function to create fit index cards
    createFitIndexCards <- function(fit_measures, model_name) {
      # Chi-square and p-value
      chi_square_card <- div(class = "col-md-3",
                             div(class = "panel panel-default",
                                 div(class = "panel-heading", h4("Chi-Square Test")),
                                 div(class = "panel-body text-center",
                                     p(strong("χ² = ", fit_measures["chisq"] |> round(2))),
                                     p("df = ", fit_measures["df"] |> round(0)),
                                     p(class = classifyFitIndex("pvalue", fit_measures["pvalue"])$class,
                                       "p = ", classifyFitIndex("pvalue", fit_measures["pvalue"])$value)
                                 )
                             )
      )
      
      # CFI
      cfi_class <- classifyFitIndex("cfi", fit_measures["cfi"])
      cfi_card <- div(class = "col-md-3",
                      div(class = "panel panel-default",
                          div(class = "panel-heading", h4("Comparative Fit Index (CFI)")),
                          div(class = "panel-body text-center",
                              h2(class = cfi_class$class, cfi_class$value),
                              p(class = cfi_class$class, cfi_class$text)
                          )
                      )
      )
      
      # RMSEA
      rmsea_class <- classifyFitIndex("rmsea", fit_measures["rmsea"])
      rmsea_card <- div(class = "col-md-3",
                        div(class = "panel panel-default",
                            div(class = "panel-heading", h4("RMSEA")),
                            div(class = "panel-body text-center",
                                h2(class = rmsea_class$class, rmsea_class$value),
                                p(class = rmsea_class$class, rmsea_class$text)
                            )
                        )
      )
      
      # SRMR
      srmr_class <- classifyFitIndex("srmr", fit_measures["srmr"])
      srmr_card <- div(class = "col-md-3",
                       div(class = "panel panel-default",
                           div(class = "panel-heading", h4("SRMR")),
                           div(class = "panel-body text-center",
                               h2(class = srmr_class$class, srmr_class$value),
                               p(class = srmr_class$class, srmr_class$text)
                           )
                       )
      )
      
      # Return all cards in a row
      div(class = "row", chi_square_card, cfi_card, rmsea_card, srmr_card)
    }
    
    # Render fit indices UI
    output$fitIndicesUI <- renderUI({
      req(cfaResults(), input$run_cfa > 0)
      results <- cfaResults()
      model_type <- input$model_type
      
      # Create fit indices cards based on the model type
      if (model_type == "default_factor") {
        if (inherits(results$fit_cfa_six, "try-error")) {
          return(div(class = "alert alert-danger", "Error in model fitting"))
        }
        
        fit_measures <- lavaan::fitMeasures(results$fit_cfa_six, 
                                            c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
        model_name <- "Six-Factor Model"
        
        createFitIndexCards(fit_measures, model_name)
        
      } else if (model_type == "single_factor") {
        if (inherits(results$fit_cfa_single, "try-error")) {
          return(div(class = "alert alert-danger", "Error in model fitting"))
        }
        
        fit_measures <- lavaan::fitMeasures(results$fit_cfa_single, 
                                            c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
        model_name <- "Single-Factor Model"
        
        createFitIndexCards(fit_measures, model_name)
        
      } else if (model_type == "custom") {
        if (inherits(results$fit_cfa_custom, "try-error")) {
          return(div(class = "alert alert-danger", "Error in model fitting"))
        }
        
        fit_measures <- lavaan::fitMeasures(results$fit_cfa_custom, 
                                            c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
        model_name <- "Custom Factor Model"
        
        createFitIndexCards(fit_measures, model_name)
        
      } else if (model_type == "compare") {
        # For comparison, show all selected models
        output_elements <- list()
        
        if (input$compare_six && !inherits(results$fit_cfa_six, "try-error")) {
          fit_measures_six <- lavaan::fitMeasures(results$fit_cfa_six, 
                                                  c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
          
          output_elements <- c(output_elements, list(
            h4("Six-Factor Model"),
            createFitIndexCards(fit_measures_six, "Six-Factor Model"),
            tags$hr()
          ))
        }
        
        if (input$compare_single && !inherits(results$fit_cfa_single, "try-error")) {
          fit_measures_single <- lavaan::fitMeasures(results$fit_cfa_single, 
                                                     c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
          
          output_elements <- c(output_elements, list(
            h4("Single-Factor Model"),
            createFitIndexCards(fit_measures_single, "Single-Factor Model"),
            tags$hr()
          ))
        }
        
        if (input$compare_custom && 
            !is.null(results$fit_cfa_custom) && 
            !inherits(results$fit_cfa_custom, "try-error")) {
          fit_measures_custom <- lavaan::fitMeasures(results$fit_cfa_custom, 
                                                     c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
          
          output_elements <- c(output_elements, list(
            h4("Custom Factor Model"),
            createFitIndexCards(fit_measures_custom, "Custom Factor Model")
          ))
        }
        
        if (length(output_elements) == 0) {
          div(class = "alert alert-warning", 
              "No valid models to display. Please check your model selections and run the analysis again.")
        } else {
          # Remove the last <hr> if it exists
          if (length(output_elements) > 2 && identical(output_elements[[length(output_elements)]], tags$hr())) {
            output_elements <- output_elements[-length(output_elements)]
          }
          do.call(tagList, output_elements)
        }
      }
    })
    
    # Render fit indices interpretation
    output$fitIndicesInterpretation <- renderUI({
      req(input$run_cfa > 0)
      div(
        p("CFA produces several fit indices to evaluate how well the model fits the data:"),
        tags$ul(
          tags$li(
            strong("Chi-Square Test (χ²): "), 
            "Tests the hypothesis that the model fits the data. A non-significant p-value (> 0.05) suggests good fit. ",
            "However, with large samples, even well-fitting models often show significant chi-square values."
          ),
          tags$li(
            strong("Comparative Fit Index (CFI): "), 
            "Ranges from 0 to 1, with values closer to 1 indicating better fit. ",
            "CFI ≥ 0.95 is excellent, ≥ 0.90 is good, ≥ 0.80 is marginal, < 0.80 is poor."
          ),
          tags$li(
            strong("Root Mean Square Error of Approximation (RMSEA): "), 
            "Lower values indicate better fit. ",
            "RMSEA ≤ 0.05 is excellent, ≤ 0.08 is good, ≤ 0.10 is marginal, > 0.10 is poor."
          ),
          tags$li(
            strong("Standardized Root Mean Square Residual (SRMR): "), 
            "Lower values indicate better fit. ",
            "SRMR ≤ 0.05 is excellent, ≤ 0.08 is good, ≤ 0.10 is marginal, > 0.10 is poor."
          )
        ),
        div(class = "alert alert-info",
            p(strong("Note: "), "Model fit should be evaluated using multiple indices collectively, not just one. ",
              "Hu & Bentler (1999) recommend CFI ≥ 0.95 and SRMR ≤ 0.08 (or RMSEA ≤ 0.06) as indicators of good fit."),
            p(strong("Citation: "), "Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: ",
              "Conventional criteria versus new alternatives. Structural Equation Modeling, 6(1), 1-55.")
        )
      )
    })
    
    # Render factor loadings table
    # Render factor loadings table - FIXED VERSION
    output$factorLoadingsUI <- renderUI({
      req(cfaResults(), input$run_cfa > 0)
      results <- cfaResults()
      model_type <- input$model_type
      loading_cutoff <- input$std_cutoff
      
      if (model_type == "default_factor") {
        if (inherits(results$fit_cfa_six, "try-error")) {
          return(div(class = "alert alert-danger", "Error in six-factor model"))
        }
        
        model_to_use <- results$fit_cfa_six
        model_name <- "Six-Factor Model"
      } else if (model_type == "single_factor") {
        if (inherits(results$fit_cfa_single, "try-error")) {
          return(div(class = "alert alert-danger", "Error in single-factor model"))
        }
        
        model_to_use <- results$fit_cfa_single
        model_name <- "Single-Factor Model"
      } else if (model_type == "custom") {
        if (inherits(results$fit_cfa_custom, "try-error")) {
          return(div(class = "alert alert-danger", "Error in custom model"))
        }
        
        model_to_use <- results$fit_cfa_custom
        model_name <- "Custom Factor Model"
      } else if (model_type == "compare") {
        # For comparison, default to six-factor model if available
        if (input$compare_six && !inherits(results$fit_cfa_six, "try-error")) {
          model_to_use <- results$fit_cfa_six
          model_name <- "Six-Factor Model"
        } else if (input$compare_single && !inherits(results$fit_cfa_single, "try-error")) {
          model_to_use <- results$fit_cfa_single
          model_name <- "Single-Factor Model"
        } else if (input$compare_custom && !is.null(results$fit_cfa_custom) && 
                   !inherits(results$fit_cfa_custom, "try-error")) {
          model_to_use <- results$fit_cfa_custom
          model_name <- "Custom Factor Model"
        } else {
          return(div(class = "alert alert-danger", "No valid models available"))
        }
      }
      
      # Get standardized solution - fixed to match lavaan's output format
      std_solution <- lavaan::standardizedSolution(model_to_use)
      
      # Filter for factor loadings only (not variances or covariances)
      factor_loadings <- std_solution %>%
        filter(op == "=~") %>%
        dplyr::select(lhs, rhs, est.std, pvalue) %>%
        rename(
          Factor = lhs,
          Item = rhs,
          Loading = est.std,
          `P-value` = pvalue
        )
      
      # Join with variable mapping to get question text
      var_mapping <- cfaData()$var_mapping
      factor_loadings_with_text <- factor_loadings %>%
        left_join(var_mapping, by = c("Item" = "rCode")) %>%
        dplyr::select(Factor, Item, qText, constructName, constructID, Loading, `P-value`) %>%
        rename(`Question Text` = qText, 
               `Expected Construct` = constructName)
      
      # Add a column to indicate if item loaded on its expected factor
      factor_loadings_with_text <- factor_loadings_with_text %>%
        mutate(
          Expected_Factor = paste0("F", sub("^C", "", constructID)),
          `Loading Status` = case_when(
            Factor == Expected_Factor & Loading >= 0.7 ~ "Excellent",
            Factor == Expected_Factor & Loading >= 0.5 ~ "Good",
            Factor == Expected_Factor & Loading >= 0.4 ~ "Acceptable",
            Factor != Expected_Factor & Loading >= 0.4 ~ "Cross-loading",
            TRUE ~ "Poor loading"
          )
        )
      
      # Create DT table with formatting
      DT::renderDataTable({
        DT::datatable(
          factor_loadings_with_text,
          options = list(
            paging = TRUE,
            pageLength = 25,
            scrollX = TRUE,
            dom = 'Bfrtlip'
          ),
          caption = paste("Standardized Factor Loadings for", model_name),
          rownames = FALSE
        ) %>%
          formatRound(columns = c("Loading", "P-value"), digits = 3) %>%
          formatStyle(
            "Loading",
            background = styleInterval(
              c(loading_cutoff, 0.6, 0.7),
              c("#FFCCCC", "#FFFFCC", "#CCFFCC", "#99FF99")
            ),
            fontWeight = styleInterval(loading_cutoff, c("normal", "bold"))
          ) %>%
          formatStyle(
            "P-value",
            color = styleInterval(0.05, c("black", "#AAAAAA")),
            fontWeight = styleInterval(0.05, c("bold", "normal"))
          ) %>%
          formatStyle(
            "Loading Status",
            backgroundColor = styleEqual(
              c("Excellent", "Good", "Acceptable", "Cross-loading", "Poor loading"),
              c("#CCFFCC", "#DDFFDD", "#FFFFCC", "#FFE0B2", "#FFCCCC")
            ),
            fontWeight = styleEqual(
              c("Excellent", "Good"),
              c("bold", "bold")
            )
          )
      })
    })
    
    # Render path diagram
    output$pathDiagram <- renderPlot({
      req(cfaResults(), input$run_cfa > 0)
      results <- cfaResults()
      model_type <- input$model_type
      
      if (model_type == "default_factor") {
        if (inherits(results$fit_cfa_six, "try-error")) {
          return(NULL)
        }
        
        model_to_use <- results$fit_cfa_six
        model_name <- "Six-Factor Model"
      } else if (model_type == "single_factor") {
        if (inherits(results$fit_cfa_single, "try-error")) {
          return(NULL)
        }
        
        model_to_use <- results$fit_cfa_single
        model_name <- "Single-Factor Model"
      } else if (model_type == "custom") {
        if (inherits(results$fit_cfa_custom, "try-error")) {
          return(NULL)
        }
        
        model_to_use <- results$fit_cfa_custom
        model_name <- "Custom Factor Model"
      } else if (model_type == "compare") {
        # For comparison, choose which model to display
        # Default to six-factor if available
        if (input$compare_six && !inherits(results$fit_cfa_six, "try-error")) {
          model_to_use <- results$fit_cfa_six
          model_name <- "Six-Factor Model"
        } else if (input$compare_single && !inherits(results$fit_cfa_single, "try-error")) {
          model_to_use <- results$fit_cfa_single
          model_name <- "Single-Factor Model"
        } else if (input$compare_custom && !is.null(results$fit_cfa_custom) && 
                   !inherits(results$fit_cfa_custom, "try-error")) {
          model_to_use <- results$fit_cfa_custom
          model_name <- "Custom Factor Model"
        } else {
          return(NULL)
        }
      }
      
      # Create the diagram with improved formatting
      semPlot::semPaths(
        model_to_use,
        what = "std",  # Use standardized values
        layout = "circle",  # Circle layout to prevent overlap
        edge.label.cex = 0.5,  # Smaller edge labels
        sizeMan = 4,  # Size of manifest variables
        sizeLat = 6,  # Size of latent variables
        residuals = input$show_residuals,  # Show residuals based on user choice
        covs = input$show_covs, # Show covariances based on user choice
        color = list(lat = "lightblue", man = "lightgreen"),  # Color scheme
        edge.color = "darkgrey",
        edge.curved = 0.15,  # Slightly curved edges
        shapeMan = "rectangle",  # Rectangle for manifest variables
        node.width = 1.5,
        node.height = 1.2,
        node.label.cex = 0.6,  # Smaller node labels
        curvePivot = TRUE,
        edge.label.position = 0.65,
        legend = FALSE
      )
      
      # Add title
      title(paste(model_name, "Path Diagram"), cex.main = 1.5, font.main = 2)
    })
    
    # Model comparison table
    output$modelComparisonTable <- renderTable({
      req(cfaResults(), input$run_cfa > 0)
      results <- cfaResults()
      
      # Build the comparison table dynamically based on selected models
      fit_indices <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic")
      
      # Initialize table structure
      comparison_table <- data.frame(
        `Fit Index` = c("Chi-Square (χ²)", "Degrees of Freedom", "p-value", "CFI", "TLI", 
                        "RMSEA", "SRMR", "AIC", "BIC")
      )
      
      # Add data for each selected model
      models_to_compare <- list()
      model_names <- c()
      
      if (input$compare_six && !inherits(results$fit_cfa_six, "try-error")) {
        fit_measures_six <- lavaan::fitMeasures(results$fit_cfa_six, fit_indices)
        models_to_compare <- c(models_to_compare, list(fit_measures_six))
        model_names <- c(model_names, "Six-Factor Model")
      }
      
      if (input$compare_single && !inherits(results$fit_cfa_single, "try-error")) {
        fit_measures_single <- lavaan::fitMeasures(results$fit_cfa_single, fit_indices)
        models_to_compare <- c(models_to_compare, list(fit_measures_single))
        model_names <- c(model_names, "Single-Factor Model")
      }
      
      if (input$compare_custom && !is.null(results$fit_cfa_custom) && 
          !inherits(results$fit_cfa_custom, "try-error")) {
        fit_measures_custom <- lavaan::fitMeasures(results$fit_cfa_custom, fit_indices)
        models_to_compare <- c(models_to_compare, list(fit_measures_custom))
        model_names <- c(model_names, "Custom Factor Model")
      }
      
      # If we have less than 2 models, return a message
      if (length(models_to_compare) < 2) {
        return(data.frame(`Message` = "At least two valid models are needed for comparison."))
      }
      
      # Add model data to the comparison table
      for (i in 1:length(models_to_compare)) {
        model_data <- c(
          round(models_to_compare[[i]]["chisq"], 2),
          models_to_compare[[i]]["df"],
          round(models_to_compare[[i]]["pvalue"], 3),
          round(models_to_compare[[i]]["cfi"], 3),
          round(models_to_compare[[i]]["tli"], 3),
          round(models_to_compare[[i]]["rmsea"], 3),
          round(models_to_compare[[i]]["srmr"], 3),
          round(models_to_compare[[i]]["aic"], 1),
          round(models_to_compare[[i]]["bic"], 1)
        )
        
        comparison_table[[model_names[i]]] <- model_data
      }
      
      # Add model comparison statistics for each pair of models
      if (length(models_to_compare) >= 2) {
        # Add rows for model comparisons
        comparison_rows <- data.frame(`Fit Index` = character(0))
        
        for (i in 1:(length(models_to_compare)-1)) {
          for (j in (i+1):length(models_to_compare)) {
            # Calculate model comparison statistics
            chi_sq_diff <- abs(models_to_compare[[i]]["chisq"] - models_to_compare[[j]]["chisq"])
            df_diff <- abs(models_to_compare[[i]]["df"] - models_to_compare[[j]]["df"])
            p_value_diff <- 1 - pchisq(chi_sq_diff, df_diff)
            
            # Create row labels
            comparison_label <- paste(model_names[i], "vs", model_names[j])
            
            # Add rows for this comparison
            new_rows <- data.frame(
              `Fit Index` = c(
                paste("χ² diff:", comparison_label),
                paste("df diff:", comparison_label),
                paste("p-value diff:", comparison_label)
              )
            )
            
            # Add empty columns for all models
            for (name in model_names) {
              new_rows[[name]] <- ""
            }
            
            # Add the comparison values in the appropriate columns
            new_rows[[model_names[i]]] <- c(
              round(chi_sq_diff, 2),
              df_diff,
              round(p_value_diff, 5)
            )
            
            comparison_rows <- rbind(comparison_rows, new_rows)
          }
        }
        
        # Combine the tables
        comparison_table <- rbind(comparison_table, comparison_rows)
      }
      
      # Return the comparison table
      comparison_table
    }, rownames = FALSE, spacing = "m", width = "100%")
    
    # Model comparison interpretation
    output$modelComparisonInterpretation <- renderUI({
      req(cfaResults(), input$run_cfa > 0)
      results <- cfaResults()
      
      # Check which models we're comparing
      models_to_compare <- list()
      model_names <- c()
      
      if (input$compare_six && !inherits(results$fit_cfa_six, "try-error")) {
        fit_measures_six <- lavaan::fitMeasures(results$fit_cfa_six, 
                                                c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
        models_to_compare <- c(models_to_compare, list(fit_measures_six))
        model_names <- c(model_names, "Six-Factor Model")
      }
      
      if (input$compare_single && !inherits(results$fit_cfa_single, "try-error")) {
        fit_measures_single <- lavaan::fitMeasures(results$fit_cfa_single, 
                                                   c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
        models_to_compare <- c(models_to_compare, list(fit_measures_single))
        model_names <- c(model_names, "Single-Factor Model")
      }
      
      if (input$compare_custom && !is.null(results$fit_cfa_custom) && 
          !inherits(results$fit_cfa_custom, "try-error")) {
        fit_measures_custom <- lavaan::fitMeasures(results$fit_cfa_custom, 
                                                   c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
        models_to_compare <- c(models_to_compare, list(fit_measures_custom))
        model_names <- c(model_names, "Custom Factor Model")
      }
      
      # Make sure we have at least two models to compare
      if (length(models_to_compare) < 2) {
        return(div(class = "alert alert-warning",
                   "At least two valid models are needed for comparison."))
      }
      
      # Create a comparison matrix for all model pairs
      comparison_results <- data.frame(
        Model1 = character(), 
        Model2 = character(), 
        Better_CFI = character(),
        Better_RMSEA = character(),
        Better_AIC = character(),
        Better_BIC = character(),
        Chi_Sq_Diff = numeric(),
        DF_Diff = numeric(),
        P_Value_Diff = numeric(),
        Overall_Better = character()
      )
      
      for (i in 1:(length(models_to_compare)-1)) {
        for (j in (i+1):length(models_to_compare)) {
          # Compare models on each index
          better_cfi <- ifelse(models_to_compare[[i]]["cfi"] > models_to_compare[[j]]["cfi"], 
                               model_names[i], model_names[j])
          better_rmsea <- ifelse(models_to_compare[[i]]["rmsea"] < models_to_compare[[j]]["rmsea"], 
                                 model_names[i], model_names[j])
          better_aic <- ifelse(models_to_compare[[i]]["aic"] < models_to_compare[[j]]["aic"], 
                               model_names[i], model_names[j])
          better_bic <- ifelse(models_to_compare[[i]]["bic"] < models_to_compare[[j]]["bic"], 
                               model_names[i], model_names[j])
          
          # Calculate likelihood ratio test
          chi_sq_diff <- abs(models_to_compare[[i]]["chisq"] - models_to_compare[[j]]["chisq"])
          df_diff <- abs(models_to_compare[[i]]["df"] - models_to_compare[[j]]["df"])
          p_value_diff <- 1 - pchisq(chi_sq_diff, df_diff)
          
          # Determine overall better model
          model1_count <- sum(c(better_cfi, better_rmsea, better_aic, better_bic) == model_names[i])
          model2_count <- sum(c(better_cfi, better_rmsea, better_aic, better_bic) == model_names[j])
          
          if (model1_count >= 3) {
            overall_better <- model_names[i]
          } else if (model2_count >= 3) {
            overall_better <- model_names[j]
          } else {
            overall_better <- "Mixed results"
          }
          
          comparison_results <- rbind(comparison_results, data.frame(
            Model1 = model_names[i],
            Model2 = model_names[j],
            Better_CFI = better_cfi,
            Better_RMSEA = better_rmsea,
            Better_AIC = better_aic,
            Better_BIC = better_bic,
            Chi_Sq_Diff = chi_sq_diff,
            DF_Diff = df_diff,
            P_Value_Diff = p_value_diff,
            Overall_Better = overall_better
          ))
        }
      }
      
      # Create interpretation based on all comparisons
      output_elements <- list(
        h4("Model Comparison Interpretation"),
        p("The table above compares different model structures across multiple fit indices:")
      )
      
      # Add interpretation for each comparison
      for (i in 1:nrow(comparison_results)) {
        comparison <- comparison_results[i, ]
        
        # Determine alert class based on result clarity
        if (comparison$Overall_Better == "Mixed results") {
          alert_class <- "alert-info"
          better_text <- "Mixed results - consider both models based on theory"
        } else {
          alert_class <- "alert-success"
          better_text <- paste("The", comparison$Overall_Better, "appears to be more appropriate")
        }
        
        # Create comparison summary
        comparison_summary <- div(
          class = paste("alert", alert_class),
          h4(paste("Comparison:", comparison$Model1, "vs", comparison$Model2)),
          tags$ul(
            tags$li(strong("Chi-Square Difference Test: "), 
                    ifelse(comparison$P_Value_Diff < 0.05, 
                           paste("Significant (p =", round(comparison$P_Value_Diff, 5), "). This suggests the more complex model fits the data significantly better."),
                           paste("Non-significant (p =", round(comparison$P_Value_Diff, 5), "). This suggests the simpler model may be preferred for parsimony."))),
            
            tags$li(strong("Information Criteria (AIC/BIC): "), 
                    paste("Lower values indicate better fit. The", 
                          comparison$Better_AIC, 
                          "model has lower AIC, and the",
                          comparison$Better_BIC,
                          "model has lower BIC.")),
            
            tags$li(strong("Comparative & Absolute Fit: "), 
                    paste("The", comparison$Better_CFI, "model has better CFI and the", 
                          comparison$Better_RMSEA, "model has better RMSEA."))
          ),
          
          p(strong("Overall Assessment: "), better_text)
        )
        
        output_elements <- c(output_elements, list(comparison_summary))
      }
      
      # Add general note
      output_elements <- c(output_elements, list(
        div(class = "alert alert-info",
            strong("Note: "), 
            "Model selection should balance statistical fit with theoretical considerations and parsimony. ",
            "See Burnham & Anderson (2004) for more on model selection using information criteria.",
            tags$br(),
            strong("Citation: "), 
            "Burnham, K. P., & Anderson, D. R. (2004). Multimodel inference: Understanding AIC and BIC in model selection. ",
            "Sociological Methods & Research, 33(2), 261-304."
        )
      ))
      
      # Return all output elements
      do.call(tagList, output_elements)
    })
    
    # Model specifications
    output$modelSpecification <- renderPrint({
      req(cfaResults(), input$run_cfa > 0)
      results <- cfaResults()
      model_type <- input$model_type
      
      if (model_type == "default_factor") {
        if (inherits(results$fit_cfa_six, "try-error")) {
          return(cat("Error in six-factor model"))
        }
        
        cat("Six-Factor Model Specification:\n\n")
        cat(cfaData()$cfa_model_default_factor)
      } else if (model_type == "single_factor") {
        if (inherits(results$fit_cfa_single, "try-error")) {
          return(cat("Error in single-factor model"))
        }
        
        cat("Single-Factor Model Specification:\n\n")
        cat(cfaData()$model_specs_single)
      } else if (model_type == "custom") {
        if (inherits(results$fit_cfa_custom, "try-error")) {
          return(cat("Error in custom model"))
        }
        
        cat("Custom Factor Model Specification:\n\n")
        cat(cfaData()$cfa_model_custom)
      } else if (model_type == "compare") {
        # Show specifications for all selected models
        if (input$compare_six && !inherits(results$fit_cfa_six, "try-error")) {
          cat("Six-Factor Model Specification:\n\n")
          cat(cfaData()$cfa_model_default_factor)
          cat("\n\n------------------------------\n\n")
        }
        
        if (input$compare_single && !inherits(results$fit_cfa_single, "try-error")) {
          cat("Single-Factor Model Specification:\n\n")
          cat(cfaData()$model_specs_single)
          cat("\n\n------------------------------\n\n")
        }
        
        if (input$compare_custom && !is.null(cfaData()$cfa_model_custom) && 
            !inherits(results$fit_cfa_custom, "try-error")) {
          cat("Custom Factor Model Specification:\n\n")
          cat(cfaData()$cfa_model_custom)
        }
      }
    })
    
    # Model summary
    output$modelSummary <- renderPrint({
      req(cfaResults(), input$run_cfa > 0)
      results <- cfaResults()
      model_type <- input$model_type
      
      if (model_type == "default_factor") {
        if (inherits(results$fit_cfa_six, "try-error")) {
          return(cat("Error in six-factor model"))
        }
        
        cat("Six-Factor Model Summary:\n\n")
        print(summary(results$fit_cfa_six, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
      } else if (model_type == "single_factor") {
        if (inherits(results$fit_cfa_single, "try-error")) {
          return(cat("Error in single-factor model"))
        }
        
        cat("Single-Factor Model Summary:\n\n")
        print(summary(results$fit_cfa_single, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
      } else if (model_type == "custom") {
        if (inherits(results$fit_cfa_custom, "try-error")) {
          return(cat("Error in custom model"))
        }
        
        cat("Custom Factor Model Summary:\n\n")
        print(summary(results$fit_cfa_custom, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
      } else if (model_type == "compare") {
        # Show summaries for all selected models
        if (input$compare_six && !inherits(results$fit_cfa_six, "try-error")) {
          cat("Six-Factor Model Summary:\n\n")
          print(summary(results$fit_cfa_six, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
          cat("\n\n------------------------------\n\n")
        }
        
        if (input$compare_single && !inherits(results$fit_cfa_single, "try-error")) {
          cat("Single-Factor Model Summary:\n\n")
          print(summary(results$fit_cfa_single, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
          cat("\n\n------------------------------\n\n")
        }
        
        if (input$compare_custom && !is.null(results$fit_cfa_custom) && 
            !inherits(results$fit_cfa_custom, "try-error")) {
          cat("Custom Factor Model Summary:\n\n")
          print(summary(results$fit_cfa_custom, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
        }
      }
    })
  })
}