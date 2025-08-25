# modules/factorAnalysis.R

library(shiny)
library(dplyr)
library(tidyr)
library(mice)
library(psych)  # Using psych for factor analysis which supports oblimin
library(lavaan)
library(semPlot)
library(DT)
library(htmltools)
library(ggplot2)
library(stringr)
library(formattable) # For better table formatting

# ---------------------- UI Function ------------------------------- #
reliabilityFactorUI <- function(id, label = "Reliability & Factor Analysis") {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Impute/Remove Missingness",
               fluidRow(
                 column(6,
                        wellPanel(
                          h3("Data Preparation"),
                          radioButtons(ns("missing_option"), "Missing Data Handling:",
                                       choices = c("Impute missing values" = "impute",
                                                   "Remove incomplete rows" = "remove"),
                                       selected = "impute"),
                          actionButton(ns("processData"), "Process Data", class = "btn-primary")
                        )
                 ),
                 column(6,
                        wellPanel(
                          h3("Status"),
                          verbatimTextOutput(ns("prepStatus"))
                        )
                 )
               )
      ),
      tabPanel("Reliability Analysis",
               fluidRow(
                 column(12,
                        wellPanel(
                          h3("Reliability Analysis Results"),
                          div(class = "row",
                              div(class = "col-md-6",
                                  uiOutput(ns("alphaCardUI"))
                              ),
                              div(class = "col-md-6",
                                  htmlOutput(ns("alphaInterpretation"))
                              )
                          ),
                          hr(),
                          h4("Item-Total Statistics"),
                          DT::dataTableOutput(ns("itemStatsTable"))
                        )
                 )
               )
      ),
      tabPanel("Exploratory Factor Analysis (EFA)",
               fluidRow(
                 column(12,
                        wellPanel(
                          h3("EFA Methodology"),
                          p("Exploratory Factor Analysis (EFA) is a statistical method used to uncover the underlying structure of a set of variables. 
                            It identifies the number of latent constructs and the underlying factor structure of a set of variables."),
                          div(class = "alert alert-info",
                              h4("Methodology Steps:"),
                              tags$ol(
                                tags$li("Test data adequacy with KMO and Bartlett's tests"),
                                tags$li("Determine optimal number of factors using parallel analysis"),
                                tags$li("Extract factors using Maximum Likelihood Estimation"),
                                tags$li("Apply rotation to improve interpretability"),
                                tags$li("Interpret factor loadings and evaluate construct validity")
                              ),
                              p("References:"),
                              tags$ul(
                                tags$li("Fabrigar, L. R., Wegener, D. T., MacCallum, R. C., & Strahan, E. J. (1999). Evaluating the use of exploratory factor analysis in psychological research. Psychological Methods, 4(3), 272-299."),
                                tags$li("Thompson, B. (2004). Exploratory and confirmatory factor analysis: Understanding concepts and applications. American Psychological Association.")
                              )
                          ),
                          actionButton(ns("runAdequacyTests"), "Run Adequacy Tests & Parallel Analysis", 
                                       class = "btn-primary", style = "margin-bottom: 15px;")
                        )
                 )
               ),
               
               # Make these sections conditional on running adequacy tests
               conditionalPanel(
                 condition = sprintf("input['%s'] > 0", ns("runAdequacyTests")),
                 fluidRow(
                   column(12,
                          wellPanel(
                            h3("Sample Adequacy Tests"),
                            div(class = "row",
                                div(class = "col-md-6",
                                    uiOutput(ns("kmoCardUI"))
                                ),
                                div(class = "col-md-6",
                                    uiOutput(ns("bartlettCardUI"))
                                )
                            )
                          )
                   )
                 ),
                 fluidRow(
                   column(12,
                          wellPanel(
                            h3("Parallel Analysis"),
                            helpText("Parallel analysis helps determine the optimal number of factors to retain."),
                            plotOutput(ns("parallelPlot")),
                            uiOutput(ns("parallelResultsUI"))
                          )
                   )
                 )
               ),
               
               fluidRow(
                 column(12,
                        wellPanel(
                          h3("Factor Analysis Settings"),
                          div(class = "row",
                              div(class = "col-md-4",
                                  numericInput(ns("nFactors"), "Number of Factors:", value = 4, min = 1)
                              ),
                              div(class = "col-md-4",
                                  selectInput(ns("rotation_method"), "Rotation Method:",
                                              choices = c("Oblimin" = "oblimin", 
                                                          "Promax" = "promax", 
                                                          "Varimax" = "varimax", 
                                                          "None" = "none"),
                                              selected = "oblimin")
                              ),
                              div(class = "col-md-4",
                                  actionButton(ns("runFactanal"), "Run Factor Analysis", 
                                               class = "btn-primary")
                              )
                          )
                        )
                 )
               ),
               
               # Make this section conditional on running factor analysis
               conditionalPanel(
                 condition = sprintf("input['%s'] > 0", ns("runFactanal")),
                 fluidRow(
                   column(12,
                          wellPanel(
                            h3("Factor Analysis Results"),
                            htmlOutput(ns("varianceExplained")),
                            br(),
                            tabsetPanel(
                              tabPanel("Factor Loadings Table",
                                       DT::dataTableOutput(ns("factorLoadingsTable")),
                                       helpText("Loadings below 0.3 are hidden for clarity. Colors indicate relationship between factor loadings and expected constructs.")),
                              tabPanel("Factor Diagram",
                                       plotOutput(ns("faDiagramPlot"), height = "600px")),
                              tabPanel("Construct Validation",
                                       DT::dataTableOutput(ns("constructValidationTable")),
                                       helpText("This table shows how items (questions) load on their expected constructs vs. other factors."))
                            )
                          )
                   )
                 )
               )
      )
    )
  )
}

# ---------------------- Server Function --------------------------- #
# This module now accepts an "adjustedData" reactive argument from your Data Preparation module.
reliabilityFactorServer <- function(id, adjustedData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------------- Data Processing ------------------
    # Use only the columns needed for reliability and EFA: respondentID, rCode, and response_value.
    processedData <- eventReactive(input$processData, {
      req(adjustedData())
      withProgress(message = "Processing data for reliability & factor analysis...", value = 0, {
        incProgress(0.1, detail = "Selecting relevant columns")
        rel_data <- adjustedData() %>% 
          dplyr::select(respondentID, rCode, response_value, qText, constructName, constructID)
        
        # Create mapping for variable names to question text and constructs
        var_mapping <- rel_data %>%
          dplyr::select(rCode, qText, constructName, constructID) %>%
          distinct()
        
        incProgress(0.2, detail = "Pivoting data to wide format")
        data_wide <- rel_data %>% 
          dplyr::select(respondentID, rCode, response_value) %>%
          pivot_wider(names_from = rCode, values_from = response_value) %>% 
          dplyr::select(-respondentID)
        
        incProgress(0.2, detail = "Handling missing values")
        if (input$missing_option == "impute") {
          set.seed(867)
          imputed_data <- mice::mice(data_wide, m = 5, method = 'pmm', seed = 867, printFlag = FALSE)
          complete_data <- mice::complete(imputed_data, 1)
        } else {
          complete_data <- na.omit(data_wide)
        }
        
        incProgress(0.2, detail = "Splitting data for EFA")
        total_rows <- nrow(complete_data)
        indices <- sample(1:total_rows, size = floor(total_rows * 0.5), replace = FALSE)
        data_efa <- complete_data[indices, ]
        
        incProgress(0.1, detail = "Done")
        list(
          data_wide = complete_data, 
          data_efa = data_efa,
          var_mapping = var_mapping
        )
      })
    })
    
    output$prepStatus <- renderPrint({
      req(processedData())
      cat("Data processed successfully!\n\n",
          "Rows in complete data:", nrow(processedData()$data_wide), "\n",
          "Rows in EFA dataset:", nrow(processedData()$data_efa), "\n",
          "Variables mapped:", nrow(processedData()$var_mapping))
    })
    
    # ---------------- Reliability Analysis ------------------
    reliabilityResults <- reactive({
      req(processedData())
      data_for_reliability <- processedData()$data_efa
      alpha_result <- psych::alpha(data_for_reliability)
      
      # Extract item statistics for table
      item_stats <- alpha_result$item.stats %>%
        as.data.frame() %>%
        rownames_to_column("Variable") %>%
        left_join(processedData()$var_mapping, by = c("Variable" = "rCode")) %>%
        dplyr::select(Variable, qText, constructName, raw.r, std.r, r.drop, mean, sd) %>%
        mutate(across(where(is.numeric), ~round(., 3)))
      
      list(
        alpha = alpha_result$total$raw_alpha,
        std_alpha = alpha_result$total$std.alpha,
        item_stats = item_stats
      )
    })
    
    # Render alpha results
    output$alphaCardUI <- renderUI({
      req(reliabilityResults())
      results <- reliabilityResults()
      
      # Determine class for styling based on alpha value
      alpha_class <- if(results$alpha >= 0.9) {
        "text-success"
      } else if(results$alpha >= 0.8) {
        "text-primary"
      } else if(results$alpha >= 0.7) {
        "text-info"
      } else if(results$alpha >= 0.6) {
        "text-warning"
      } else {
        "text-danger"
      }
      
      div(class = "panel panel-default",
          div(class = "panel-heading", 
              h4(class = "panel-title", "Cronbach's Alpha")),
          div(class = "panel-body",
              h2(class = alpha_class, style = "text-align: center;", 
                 round(results$alpha, 3)),
              p(style = "text-align: center;", 
                "Standardized Alpha:", round(results$std_alpha, 3))
          )
      )
    })
    
    # Render alpha interpretation with whole survey note
    output$alphaInterpretation <- renderUI({
      req(reliabilityResults())
      results <- reliabilityResults()
      alpha_val <- results$alpha
      
      # Determine interpretation text
      if(alpha_val >= 0.9) {
        interpretation <- "Excellent internal consistency"
        color_class <- "text-success"
      } else if(alpha_val >= 0.8) {
        interpretation <- "Good internal consistency"
        color_class <- "text-primary"
      } else if(alpha_val >= 0.7) {
        interpretation <- "Acceptable internal consistency"
        color_class <- "text-info"
      } else if(alpha_val >= 0.6) {
        interpretation <- "Questionable internal consistency"
        color_class <- "text-warning"
      } else {
        interpretation <- "Poor internal consistency"
        color_class <- "text-danger"
      }
      
      div(
        h4("Interpretation"),
        p("Cronbach's Alpha reflects how closely related a set of items are as a group."),
        div(class = "alert alert-warning",
            strong("Note:"), " This reliability coefficient represents the ",
            strong("entire survey"), " as a whole. For a more meaningful analysis, 
               consider calculating reliability for each individual construct separately."
        ),
        tags$ul(
          tags$li(HTML("<strong>Excellent:</strong> &alpha; ≥ 0.9")),
          tags$li(HTML("<strong>Good:</strong> 0.8 ≤ &alpha; < 0.9")),
          tags$li(HTML("<strong>Acceptable:</strong> 0.7 ≤ &alpha; < 0.8")),
          tags$li(HTML("<strong>Questionable:</strong> 0.6 ≤ &alpha; < 0.7")),
          tags$li(HTML("<strong>Poor:</strong> &alpha; < 0.6"))
        ),
        div(class = "alert alert-info",
            h4(class = color_class, "Your Result:"),
            p(class = color_class, interpretation),
            p("Based on ", tags$em("Kline, R. B. (2011). Principles and practice of structural equation modeling (3rd ed.)"))
        )
      )
    })
    
    # Render item statistics table with fix for column names
    output$itemStatsTable <- DT::renderDataTable({
      req(reliabilityResults())
      item_stats <- reliabilityResults()$item_stats
      
      DT::datatable(
        item_stats,
        rownames = FALSE,
        options = list(
          paging = TRUE,
          pageLength = 25,
          scrollX = TRUE,
          searching = TRUE,
          dom = 'Bfrtip'
        ),
        colnames = c(
          "Variable ID" = "Variable", 
          "Question Text" = "qText", 
          "Construct" = "constructName", 
          "Item-Total Correlation" = "raw.r",
          "Standardized Correlation" = "std.r",
          "Item-Rest Correlation" = "r.drop",
          "Mean" = "mean",
          "SD" = "sd"
        )
      ) %>%
        formatStyle(
          'Item-Rest Correlation',  # Fixed: Changed from 'r.drop' to the displayed column name
          background = styleInterval(c(0.3, 0.5, 0.7), 
                                     c('#FFCCCC', '#FFFFCC', '#CCFFCC', '#99FF99')),
          fontWeight = styleInterval(0.3, c('normal', 'bold'))
        )
    })
    
    # ---------------- Exploratory Factor Analysis (EFA) ------------------
    # Create a reactive for the EFA data.
    efaData <- reactive({
      req(processedData())
      processedData()$data_efa
    })
    
    # Add reactor for the new Run Adequacy Tests button
    observeEvent(input$runAdequacyTests, {
      req(efaData())
      # This observer triggers the reactive KMO and Bartlett's tests
    })
    
    # Run KMO test and interpret the result.
    kmoResult <- reactive({
      req(efaData())
      kmo_result <- psych::KMO(efaData())
      overall_kmo <- kmo_result$MSA
      
      # Determine classification
      if (overall_kmo < 0.5) {
        band <- "unacceptable"
        color_class <- "text-danger"
      } else if (overall_kmo < 0.6) {
        band <- "miserable"
        color_class <- "text-danger"
      } else if (overall_kmo < 0.7) {
        band <- "mediocre"
        color_class <- "text-warning"
      } else if (overall_kmo < 0.8) {
        band <- "middling"
        color_class <- "text-info"
      } else if (overall_kmo < 0.9) {
        band <- "meritorious"
        color_class <- "text-primary"
      } else {
        band <- "marvelous"
        color_class <- "text-success"
      }
      
      list(
        kmo = overall_kmo,
        band = band,
        color_class = color_class
      )
    })
    
    # Render KMO card
    output$kmoCardUI <- renderUI({
      req(kmoResult())
      result <- kmoResult()
      
      div(class = "panel panel-default",
          div(class = "panel-heading", 
              h4(class = "panel-title", "Kaiser-Meyer-Olkin (KMO) Measure")),
          div(class = "panel-body",
              h2(class = result$color_class, style = "text-align: center;", 
                 round(result$kmo, 3)),
              p(style = "text-align: center;", 
                span(class = result$color_class, paste0("Classification: ", toupper(result$band)))),
              p("The KMO measure assesses the adequacy of the sample for factor analysis by examining the proportion of variance among variables that might be common variance."),
              tags$ul(
                tags$li(HTML("<strong>0.90-1.00:</strong> Marvelous")),
                tags$li(HTML("<strong>0.80-0.89:</strong> Meritorious")),
                tags$li(HTML("<strong>0.70-0.79:</strong> Middling")),
                tags$li(HTML("<strong>0.60-0.69:</strong> Mediocre")),
                tags$li(HTML("<strong>0.50-0.59:</strong> Miserable")),
                tags$li(HTML("<strong>0.00-0.49:</strong> Unacceptable"))
              )
          )
      )
    })
    
    # Run Bartlett's test of sphericity.
    bartlettResult <- reactive({
      req(efaData())
      bart_result <- psych::cortest.bartlett(cor(efaData(), use = "pairwise.complete.obs"), n = nrow(efaData()))
      
      # Determine significance
      significant <- bart_result$p.value < 0.05
      color_class <- if(significant) "text-success" else "text-danger"
      
      list(
        chisq = bart_result$chisq,
        df = bart_result$df,
        p_value = bart_result$p.value,
        significant = significant,
        color_class = color_class
      )
    })
    
    # Render Bartlett's card
    output$bartlettCardUI <- renderUI({
      req(bartlettResult())
      result <- bartlettResult()
      
      significance_text <- if(result$significant) {
        "Significant (p < 0.05) - Factor analysis is appropriate"
      } else {
        "Not significant (p ≥ 0.05) - Factor analysis may not be appropriate"
      }
      
      div(class = "panel panel-default",
          div(class = "panel-heading", 
              h4(class = "panel-title", "Bartlett's Test of Sphericity")),
          div(class = "panel-body",
              div(style = "text-align: center;",
                  p(HTML(paste0("<strong>Chi-square:</strong> ", round(result$chisq, 2)))),
                  p(HTML(paste0("<strong>df:</strong> ", result$df))),
                  p(HTML(paste0("<strong>p-value:</strong> ", round(result$p_value, 5))))
              ),
              div(class = if(result$significant) "alert alert-success" else "alert alert-danger",
                  p(class = result$color_class, significance_text)
              ),
              p("Bartlett's test examines whether the correlation matrix is significantly different from an identity matrix. A significant result indicates that there are correlations in the data suitable for factor analysis.")
          )
      )
    })
    
    # Parallel Analysis: Run and extract results
    parallelAnalysisResult <- reactive({
      req(efaData())
      pa <- psych::fa.parallel(efaData(), fa = "fa", n.iter = 100, show.legend = FALSE, plot = FALSE)
      pa
    })
    
    # Render Parallel Analysis plot
    output$parallelPlot <- renderPlot({
      req(parallelAnalysisResult())
      psych::fa.parallel(efaData(), fa = "fa", n.iter = 100, show.legend = TRUE)
    })
    
    # Render Parallel Analysis results
    output$parallelResultsUI <- renderUI({
      req(parallelAnalysisResult())
      pa <- parallelAnalysisResult()
      
      div(class = "alert alert-info",
          h4("Parallel Analysis Suggests:"),
          p(HTML(paste0("<strong>", pa$nfact, " factors</strong> should be retained."))),
          br(),
          p("Parallel analysis compares eigenvalues from your data with those from random data. Factors with eigenvalues greater than those from random data are recommended for retention.")
      )
    })
    
    # Run factor analysis when user clicks the button.
    factanalResult <- eventReactive(input$runFactanal, {
      req(efaData())
      n_facs <- input$nFactors
      rot <- input$rotation_method
      
      # Use psych::fa instead of factanal
      fa_result <- psych::fa(efaData(), 
                             nfactors = n_facs, 
                             rotate = rot, 
                             fm = "ml")  # Maximum likelihood method
      
      fa_result
    })
    
    # Render variance explained
    output$varianceExplained <- renderUI({
      req(factanalResult())
      fa_res <- factanalResult()
      
      # Get cumulative variance
      cum_var <- sum(fa_res$Vaccounted[2,])
      
      div(class = "alert alert-info",
          h4("Variance Explained:"),
          tags$table(class = "table table-bordered table-striped",
                     tags$thead(
                       tags$tr(
                         tags$th("Factor"),
                         tags$th("SS Loadings"),
                         tags$th("Proportion Var"),
                         tags$th("Cumulative Var")
                       )
                     ),
                     tags$tbody(
                       lapply(1:ncol(fa_res$loadings), function(i) {
                         tags$tr(
                           tags$td(paste0("Factor ", i)),
                           tags$td(round(fa_res$Vaccounted[1,i], 3)),
                           tags$td(round(fa_res$Vaccounted[2,i], 3)),
                           tags$td(round(fa_res$Vaccounted[2,1:i] |> sum(), 3))
                         )
                       })
                     )
          ),
          p(HTML(paste0("<strong>Total variance explained: ", round(cum_var * 100, 1), "%</strong>")))
      )
    })
    
    # Format factor loadings table with question text and construct info
    # Fixed to show all loadings above 0.3
    output$factorLoadingsTable <- DT::renderDataTable({
      req(factanalResult(), processedData())
      fa_res <- factanalResult()
      var_mapping <- processedData()$var_mapping
      
      # Get the loadings as a data frame
      loadings_df <- as.data.frame(unclass(fa_res$loadings))
      
      # Rename the columns to Factor 1, Factor 2, etc.
      names(loadings_df) <- paste0("Factor ", 1:ncol(loadings_df))
      
      # Add the variable names as a column
      loadings_df <- loadings_df %>%
        rownames_to_column("Variable") %>%
        # Join with the variable mapping for question text and construct info
        left_join(var_mapping, by = c("Variable" = "rCode")) %>%
        # Move qText and constructName to the front
        dplyr::select(Variable, qText, constructName, constructID, everything())
      
      # Create a table with conditional formatting
      DT::datatable(
        loadings_df,
        rownames = FALSE,
        options = list(
          paging = TRUE,
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtlip'
        ),
        caption = "Factor Loadings (values below 0.3 are hidden)"
      ) %>%
        # Apply formatting to all factor columns
        formatStyle(
          names(loadings_df)[-(1:4)],  # Skip the first four columns
          valueColumns = names(loadings_df)[-(1:4)],
          color = styleInterval(c(0.3), c('transparent', 'black')),
          background = styleInterval(
            c(0.3, 0.4, 0.5, 0.6, 0.7),
            c('transparent', '#FFF3E0', '#FFE0B2', '#FFCC80', '#FFB74D', '#FFA726')
          ),
          fontWeight = styleInterval(0.5, c('normal', 'bold'))
        )
    })
    
    # Create a construct validation table - Fixed to handle NA values properly
    output$constructValidationTable <- DT::renderDataTable({
      req(factanalResult(), processedData())
      fa_res <- factanalResult()
      var_mapping <- processedData()$var_mapping
      
      # Get loadings and add variable info
      loadings_df <- as.data.frame(unclass(fa_res$loadings))
      names(loadings_df) <- paste0("Factor_", 1:ncol(loadings_df))
      
      # Add variable information
      loadings_df <- loadings_df %>%
        rownames_to_column("Variable") %>%
        left_join(var_mapping, by = c("Variable" = "rCode"))
      
      # Create a clean constructID column without the 'C' prefix
      loadings_df$constructID_clean <- as.numeric(gsub("^C", "", loadings_df$constructID))
      
      # For each variable, find the maximum loading - with error handling for NA values
      validation_df <- loadings_df %>%
        rowwise() %>%
        mutate(
          # Only try to get expected factor if constructID_clean exists and is valid
          expected_factor = ifelse(!is.na(constructID_clean) && 
                                     constructID_clean > 0 && 
                                     constructID_clean <= ncol(loadings_df),
                                   paste0("Factor_", constructID_clean), 
                                   NA_character_),
          
          # Only try to get expected_loading if expected_factor exists
          expected_loading = ifelse(!is.na(expected_factor) && 
                                      expected_factor %in% names(loadings_df),
                                    get(expected_factor), 
                                    NA_real_),
          
          # Find maximum loading factor
          max_factor = paste0("Factor_", which.max(c_across(starts_with("Factor_")))),
          max_loading = max(c_across(starts_with("Factor_"))),
          
          # Check if loads on expected factor - with NA handling
          loads_correctly = !is.na(expected_factor) && max_factor == expected_factor,
          
          # Set validation status with better error handling
          validation_status = case_when(
            is.na(expected_factor) ~ "Unknown construct",
            loads_correctly & max_loading >= 0.7 ~ "Excellent",
            loads_correctly & max_loading >= 0.5 ~ "Good",
            loads_correctly & max_loading >= 0.3 ~ "Acceptable",
            !loads_correctly & max_loading >= 0.3 ~ "Cross-loading issue",
            TRUE ~ "Poor loading"
          )
        ) %>%
        ungroup() %>%
        dplyr::select(Variable, qText, constructName, constructID, 
               expected_factor, expected_loading, 
               max_factor, max_loading, 
               validation_status)
      
      # Format for display
      validation_df <- validation_df %>%
        mutate(
          expected_loading = round(expected_loading, 3),
          max_loading = round(max_loading, 3)
        )
      
      # Create a table with conditional formatting
      DT::datatable(
        validation_df,
        rownames = FALSE,
        options = list(
          paging = TRUE,
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtlip'
        ),
        caption = "Construct Validation: Expected vs. Actual Factor Loadings"
      ) %>%
        formatStyle(
          'validation_status',
          backgroundColor = styleEqual(
            c("Excellent", "Good", "Acceptable", "Cross-loading issue", "Poor loading", "Unknown construct"),
            c('#CCFFCC', '#DDFFDD', '#FFFFCC', '#FFE0B2', '#FFCCCC', '#E0E0E0')
          ),
          fontWeight = styleEqual(
            c("Excellent", "Good"),
            c('bold', 'bold')
          )
        )
    })
    
    # Render factor diagram plot
    output$faDiagramPlot <- renderPlot({
      req(factanalResult())
      fa_res <- factanalResult()
      
      # Use psych::fa.diagram for a cleaner plot
      psych::fa.diagram(fa_res, cut = 0.3, simple = FALSE, sort = TRUE)
    })
    
  })
}