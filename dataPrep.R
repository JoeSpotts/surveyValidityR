## R/mod_dataPrep.R
# ----------------

surveyData <- fst::read.fst("../FSP_surveyData_sample.fst")
# ---- UI Function ----
dataPrepUI <- function(id, label = "Data Preparation") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4,
        fileInput(ns("file_survey"), "Upload Survey Data (.fst or .csv)", 
                  accept = c(".fst", ".csv")),
        
        # Checkbox to optionally use example data from your package/app:
        checkboxInput(ns("useExample"), "Use Example Data from package/app", FALSE),
        
        selectInput(
          ns("col_constructName"),
          label = "Column: Construct Name",
          choices = character(0),   # will be updated dynamically
          selected = NULL
        ),
        selectInput(
          ns("col_constructID"),
          label = "Column: Construct ID",
          choices = character(0),
          selected = NULL
        ),
        selectInput(
          ns("col_year"),
          label = "Column: End Year",
          choices = character(0),
          selected = NULL
        ),
        textInput(
          ns("endYears"),
          label = "Filter Years (comma-separated)",
          value = "2022"  # example default
        ),
        textInput(
          ns("constructFilter"),
          label = "Construct Name Filter (regex allowed)",
          value = "Standard"
        )
      ),
      column(
        width = 4,
        selectInput(
          ns("col_responseVal"),
          label = "Column: Response Value (optionOrder)",
          choices = character(0),
          selected = NULL
        ),
        selectInput(
          ns("col_reverseCode"),
          label = "Column: Reverse Code",
          choices = character(0),
          selected = NULL
        ),
        selectInput(
          ns("col_respondentID"),
          label = "Column: Respondent ID",
          choices = character(0),
          selected = NULL
        ),
        selectInput(
          ns("col_rCode"),
          label = "Column: rCode",
          choices = character(0),
          selected = NULL
        ),
        checkboxInput(
          ns("includeDemos"),
          "Include Demographic Columns?",
          value = T
        ),
        uiOutput(ns("demoColumnsUI"))  # dynamic UI for picking demos
      ),
      column(
        width = 4,
        actionButton(ns("goPrep"), "Prepare Data", class = "btn-primary"),
        br(), br(),
        verbatimTextOutput(ns("prepMessage"))
      )
    )
  )
}

# ---- Server Function ----
dataPrepServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive to store the raw data
    rawData_r <- reactive({
      validate(need(!is.null(input$file_survey$datapath) || input$useExample,
                    "Please upload a file or select 'Use Example Data'"))
      
      # If user uploads a file, read it
      if (!is.null(input$file_survey$datapath) && !input$useExample) {
        ext <- tools::file_ext(input$file_survey$name)
        if (ext == "fst") {
          # For .fst files
          data <- fst::read.fst(input$file_survey$datapath)
        } else if (ext == "csv") {
          # For .csv files
          data <- read.csv(input$file_survey$datapath, stringsAsFactors = FALSE)
        } else {
          validate("Please upload a .fst or .csv file.")
        }
        return(data)
      } else if (input$useExample) {
        # This is where you'd load your built-in or pre-loaded data.
        # e.g., data("surveyData", package = "mypackage")
        # For demonstration, let's assume you have a global object "surveyData" in .GlobalEnv:
        data <- get("surveyData", envir = .GlobalEnv)
        return(data)
      }
      
      # If neither condition is met, return NULL (though we used 'validate' above, so unlikely)
      return(NULL)
    })
    
    # The 'official' reactive that will be used for the final data:
    surveyData_r <- reactive({
      rawData_r()  # Just pass through for now
    })
    
    # Dynamically update selectInput choices after data is known
    observe({
      df <- surveyData_r()
      req(df)
      
      columnNames <- names(df)
      
      # Update the drop-down choices
      updateSelectInput(session, "col_constructName", choices = columnNames)
      updateSelectInput(session, "col_constructID",   choices = columnNames)
      updateSelectInput(session, "col_year",          choices = columnNames)
      updateSelectInput(session, "col_responseVal",   choices = columnNames)
      updateSelectInput(session, "col_reverseCode",   choices = columnNames)
      updateSelectInput(session, "col_respondentID",  choices = columnNames)
      updateSelectInput(session, "col_rCode",         choices = columnNames)
      
      # If user checked "Use Example Data", auto-fill recommended columns
      if (input$useExample) {
        # Adjust these as appropriate for your known example data
        updateSelectInput(session, "col_constructName", selected = "constructName")
        updateSelectInput(session, "col_constructID",   selected = "constructID")
        updateSelectInput(session, "col_year",          selected = "EndYear")
        updateSelectInput(session, "col_responseVal",   selected = "optionOrder")
        updateSelectInput(session, "col_reverseCode",   selected = "reverseCode")
        updateSelectInput(session, "col_respondentID",  selected = "respondentID")
        updateSelectInput(session, "col_rCode",         selected = "rCode")
      }
    })
    
    # Add a reactive value to store demo metadata
    demo_metadata_r <- reactiveVal(NULL)
    
    # Dynamic UI for selecting multiple demographic columns - UPDATED
    output$demoColumnsUI <- renderUI({
      req(surveyData_r())
      df <- surveyData_r()
      if (!input$includeDemos) return(NULL)
      
      tagList(
        selectizeInput(
          ns("demoCols"),
          "Demographic Columns",
          choices = names(df),
          selected = if (input$useExample) {
                c("grade_level_bin", "frl_bin", "race_bin", "iep_bin", "gt_bin", "ell_bin")
          } else {
            character(0)
          },
          multiple = TRUE
        ),
        
        # New: Add button to configure demographic columns
        actionButton(ns("configure_demos"), "Configure Demographics", class = "btn-info"),
        
        # Container for dynamic demographic settings
        uiOutput(ns("demoSettingsUI"))
      )
    })
    
    # Add observer for demo configuration button
    observeEvent(input$configure_demos, {
      req(input$demoCols)
      
      # Initialize demo metadata if it doesn't exist
      if (is.null(demo_metadata_r())) {
        # Set defaults based on naming patterns
        types <- sapply(input$demoCols, function(col) {
          if (grepl("_bin$", col)) return("binary")
          else return("categorical")
        })
        
        # Create display names by removing suffixes and capitalizing
        display_names <- sapply(input$demoCols, function(col) {
          # Remove _bin suffix if present
          clean_name <- gsub("_bin$", "", col)
          # Split by underscore and capitalize
          words <- strsplit(clean_name, "_")[[1]]
          words <- sapply(words, function(word) {
            paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))
          })
          # Join words with spaces
          return(paste(words, collapse = " "))
        })
        
        # Create value labels based on type
        value_labels <- lapply(1:length(types), function(i) {
          col <- input$demoCols[i]
          type <- types[i]
          
          if (type == "binary") {
            if (grepl("grade", col, ignore.case = TRUE)) {
              labels <- c("Upper School", "Lower School")
            } else {
              labels <- c("Yes", "No")
            }
            names(labels) <- c("1", "0")
            return(list(labels))
          } else {
            # For categorical, try to discover values from data
            vals <- unique(surveyData_r()[[col]])
            vals <- vals[!is.na(vals)]
            if (length(vals) > 0) {
              labels <- as.character(vals)
              names(labels) <- as.character(vals)
              return(list(labels))
            } else {
              return(list(c()))
            }
          }
        })
        
        # Create the metadata dataframe
        demo_metadata_r(
          data.frame(
            column = input$demoCols,
            display_name = display_names,
            type = types,
            na_label = sapply(input$demoCols, function(col) {
              if (grepl("grade", col, ignore.case = TRUE)) "Grade Not Specified"
              else if (grepl("race", col, ignore.case = TRUE)) "Race Not Specified"
              else "Not Specified"
            }),
            value_labels = value_labels,
            stringsAsFactors = FALSE
          )
        )
      }
      
      # Show modal dialog for configuration
      showModal(modalDialog(
        title = "Configure Demographic Variables",
        DT::dataTableOutput(ns("demo_metadata_table")),
        actionButton(ns("edit_demo"), "Edit Selected Column", class = "btn-info"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_demo_metadata"), "Save Changes", class = "btn-primary")
        )
      ))
    })
    
    # Output for demo metadata table
  
    output$demo_metadata_table <- DT::renderDataTable({
      req(demo_metadata_r())
      metadata <- demo_metadata_r()
      
      # Create a simplified view for the table without the complex values column first
      view_data <- metadata %>%
        select(column, display_name, type, na_label)
      
      # Safely create the values column
      values_col <- character(nrow(view_data))
      
      for (i in 1:nrow(view_data)) {
        if (i <= length(metadata$value_labels) && !is.null(metadata$value_labels[[i]])) {
          # Handle nested list structure
          if (is.list(metadata$value_labels[[i]]) && length(metadata$value_labels[[i]]) > 0) {
            inner_list <- metadata$value_labels[[i]][[1]]
            if (length(inner_list) > 0) {
              values_col[i] <- paste(inner_list, collapse = ", ")
            } else {
              values_col[i] <- "No values defined"
            }
          } else {
            values_col[i] <- "No values defined"
          }
        } else {
          values_col[i] <- "No values defined"
        }
      }
      
      view_data$values <- values_col
      
      DT::datatable(
        view_data,
        selection = 'single',
        options = list(dom = 't', pageLength = 50),
        rownames = FALSE
      )
    })
    
    # Add observer for demo configuration button
    observeEvent(input$configure_demos, {
      req(input$demoCols)
      
      # Initialize demo metadata if it doesn't exist
      if (is.null(demo_metadata_r())) {
        # Create properly structured value_labels list
        value_labels <- vector("list", length(input$demoCols))
        
        for (i in 1:length(input$demoCols)) {
          col <- input$demoCols[i]
          # Default binary labels depend on column type
          if (grepl("grade", col, ignore.case = TRUE)) {
            binary_labels <- c("Upper School", "Lower School")
            names(binary_labels) <- c("1", "0")
          } else {
            binary_labels <- c("Yes", "No")
            names(binary_labels) <- c("1", "0")
          }
          value_labels[[i]] <- list(binary_labels)
        }
        
        # Set defaults based on naming patterns
        types <- sapply(input$demoCols, function(col) {
          if (grepl("_bin$", col)) return("binary")
          else return("categorical")
        })
        
        # Create display names by removing suffixes and capitalizing
        display_names <- sapply(input$demoCols, function(col) {
          # Remove _bin suffix if present
          clean_name <- gsub("_bin$", "", col)
          # Split by underscore and capitalize
          words <- strsplit(clean_name, "_")[[1]]
          words <- sapply(words, function(word) {
            paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))
          })
          # Join words with spaces
          return(paste(words, collapse = " "))
        })
        
        # Create the metadata dataframe
        demo_metadata_r(
          data.frame(
            column = input$demoCols,
            display_name = display_names,
            type = types,
            na_label = sapply(input$demoCols, function(col) {
              if (grepl("grade", col, ignore.case = TRUE)) "Grade Not Specified"
              else if (grepl("race", col, ignore.case = TRUE)) "Race Not Specified"
              else "Not Specified"
            }),
            value_labels = I(value_labels),  # Use I() to preserve the list structure
            stringsAsFactors = FALSE
          )
        )
      }
      
      # Show modal dialog for configuration
      showModal(modalDialog(
        title = "Configure Demographic Variables",
        DT::dataTableOutput(ns("demo_metadata_table")),
        actionButton(ns("edit_demo"), "Edit Selected Column", class = "btn-info"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_demo_metadata"), "Save Changes", class = "btn-primary")
        )
      ))
    })
    
    # Add the edit functionality
    observeEvent(input$edit_demo, {
      req(demo_metadata_r(), input$demo_metadata_table_rows_selected)
      
      metadata <- demo_metadata_r()
      selected_row <- input$demo_metadata_table_rows_selected
      selected_col <- metadata$column[selected_row]
      
      # Create edit UI for the selected column
      showModal(modalDialog(
        title = paste("Edit", selected_col),
        
        textInput(ns("edit_display_name"), "Display Name:", 
                  value = metadata$display_name[selected_row]),
        
        selectInput(ns("edit_type"), "Variable Type:",
                    choices = c("binary", "categorical", "continuous"),
                    selected = metadata$type[selected_row]),
        
        textInput(ns("edit_na_label"), "NA Label:", 
                  value = metadata$na_label[selected_row]),
        
        # Conditional UI based on type
        conditionalPanel(
          condition = paste0("input['", ns("edit_type"), "'] == 'binary'"),
          textInput(ns("edit_bin_yes"), "Label for '1':", 
                    value = if(is.list(metadata$value_labels[[selected_row]]) && 
                               !is.null(metadata$value_labels[[selected_row]][[1]]) && 
                               length(metadata$value_labels[[selected_row]][[1]]) >= 1) 
                      metadata$value_labels[[selected_row]][[1]]["1"] else "Yes"),
          textInput(ns("edit_bin_no"), "Label for '0':", 
                    value = if(is.list(metadata$value_labels[[selected_row]]) && 
                               !is.null(metadata$value_labels[[selected_row]][[1]]) && 
                               length(metadata$value_labels[[selected_row]][[1]]) >= 2) 
                      metadata$value_labels[[selected_row]][[1]]["0"] else "No")
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("edit_type"), "'] == 'categorical'"),
          textAreaInput(ns("edit_cat_values"), "Category Labels (format: value=label, one per line):", 
                        value = if(is.list(metadata$value_labels[[selected_row]]) && 
                                   !is.null(metadata$value_labels[[selected_row]][[1]])) {
                          paste(names(metadata$value_labels[[selected_row]][[1]]), 
                                metadata$value_labels[[selected_row]][[1]], 
                                sep = "=", collapse = "\n")
                        } else "")
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_col_edit"), "Save", class = "btn-primary")
        )
      ))
    })
    
    # Save the edited column
    observeEvent(input$save_col_edit, {
      req(demo_metadata_r(), input$demo_metadata_table_rows_selected)
      
      metadata <- demo_metadata_r()
      selected_row <- input$demo_metadata_table_rows_selected
      
      # Update the display name and type
      metadata$display_name[selected_row] <- input$edit_display_name
      metadata$type[selected_row] <- input$edit_type
      metadata$na_label[selected_row] <- input$edit_na_label
      
      # Update value labels based on type
      if (input$edit_type == "binary") {
        bin_labels <- c(input$edit_bin_yes, input$edit_bin_no)
        names(bin_labels) <- c("1", "0")
        metadata$value_labels[[selected_row]] <- list(bin_labels)
      } 
      else if (input$edit_type == "categorical") {
        # Parse the category labels
        lines <- strsplit(input$edit_cat_values, "\n")[[1]]
        value_pairs <- lapply(lines, function(line) {
          parts <- strsplit(line, "=")[[1]]
          if (length(parts) == 2) {
            return(c(parts[1], parts[2]))
          } else {
            return(NULL)
          }
        })
        
        # Filter out NULL values and create a named vector
        valid_pairs <- value_pairs[!sapply(value_pairs, is.null)]
        if (length(valid_pairs) > 0) {
          values <- sapply(valid_pairs, function(x) x[1])
          labels <- sapply(valid_pairs, function(x) x[2])
          cat_map <- labels
          names(cat_map) <- values
          metadata$value_labels[[selected_row]] <- list(cat_map)
        } else {
          metadata$value_labels[[selected_row]] <- list(c())
        }
      } else {
        # For continuous, just use an empty list
        metadata$value_labels[[selected_row]] <- list(c())
      }
      
      # Update the metadata reactive
      demo_metadata_r(metadata)
      
      # Close the modal
      removeModal()
    })
    
    # Save all metadata changes
    observeEvent(input$save_demo_metadata, {
      removeModal()
    })
    
    # Reactive for the PREPARED/ADJUSTED data
    preparedData_r <- reactive({
      req(input$goPrep)  # Trigger only when user clicks Prepare Data
      
      df <- surveyData_r()
      req(df)
      
      # Parse years from text input
      years <- strsplit(input$endYears, ",")[[1]]
      years <- as.numeric(trimws(years))
      
      # Call your 'prepare_survey_data()' function with the demo metadata
      prepared <- prepare_survey_data(
        surveyData           = df,
        constructNameCol     = input$col_constructName,
        constructIDCol       = input$col_constructID,
        yearCol              = input$col_year,
        responseValCol       = input$col_responseVal,
        reverseCodeCol       = input$col_reverseCode,
        respondentIDCol      = input$col_respondentID,
        rCodeCol             = input$col_rCode,
        endYears             = years,
        constructNameFilter  = input$constructFilter,
        includeDemos         = input$includeDemos,
        demoCols             = if (input$includeDemos) input$demoCols else c(),
        demoMetadata         = demo_metadata_r()
      )
      
      return(prepared)
    }) %>%
      bindEvent(input$goPrep)
    
    # Show a small summary or message
    output$prepMessage <- renderPrint({
      req(preparedData_r())
      cat("Data prepared!\nRows:", nrow(preparedData_r()), 
          "Cols:", ncol(preparedData_r()))
    })
    
    # Return the prepared data as a reactive
    return(preparedData_r)
  })
}