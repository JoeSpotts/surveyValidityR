# R/prepare_survey_data.R
prepare_survey_data <- function(
    surveyData,
    constructNameCol      = "constructName",
    constructIDCol        = "constructID",
    yearCol               = "EndYear",
    responseValCol        = "optionOrder",
    reverseCodeCol        = "reverseCode",
    respondentIDCol       = "respondentID",
    rCodeCol              = "rCode",
    endYears              = NULL,
    constructNameFilter   = "Standard",
    includeDemos          = TRUE,
    demoCols              = NULL,
    demoMetadata          = NULL  # Parameter for demographic metadata
) {
  # Validate input data
  if(is.null(surveyData) || ncol(surveyData) == 0 || nrow(surveyData) == 0) {
    stop("Invalid survey data: data is NULL or empty")
  }
  
  # Check that required columns exist
  required_cols <- c(respondentIDCol, responseValCol, rCodeCol)
  missing_cols <- setdiff(required_cols, names(surveyData))
  if(length(missing_cols) > 0) {
    stop("Required columns missing from survey data: ", paste(missing_cols, collapse=", "))
  }
  
  # Filter by constructName and optionally by EndYear
  data_filtered <- surveyData %>%
    dplyr::filter(
      # Safe check for constructNameFilter
      if (constructNameFilter != "" && constructNameCol %in% names(surveyData)) {
        stringr::str_detect(.data[[constructNameCol]], constructNameFilter)
      } else TRUE,
      # Safe check for endYears
      if (!is.null(endYears) && yearCol %in% names(surveyData)) {
        .data[[yearCol]] %in% endYears
      } else TRUE
    )
  
  # Make sure respondentIDCol is valid
  if(is.null(respondentIDCol) || respondentIDCol == "" || !respondentIDCol %in% names(data_filtered)) {
    warning("Invalid respondentID column. Using row numbers instead.")
    data_filtered$row_id <- 1:nrow(data_filtered)
    respondentIDCol <- "row_id"
  }
  
  # Use safe grouping with sym() to properly quote the column name
  data_adjusted <- data_filtered %>%
    dplyr::group_by(!!rlang::sym(respondentIDCol)) %>%
    dplyr::mutate(
      # Safe handling of response values and reverse coding
      response_value = if(is.null(reverseCodeCol) || !reverseCodeCol %in% names(data_filtered)) {
        # If no reverse code column, just use response value directly
        .data[[responseValCol]]
      } else {
        # If we have reverse code column, apply the logic
        dplyr::if_else(
          is.na(.data[[reverseCodeCol]]),
          .data[[responseValCol]],
          {
            # Safely calculate max value
            max_val <- max(.data[[responseValCol]], na.rm = TRUE)
            if(is.finite(max_val) && max_val > 0) {
              max_val + 1 - .data[[responseValCol]]
            } else {
              .data[[responseValCol]]  # Fallback if max is -Inf or 0
            }
          }
        )
      }
    )
  
  # Handle constructID separately with validation
  data_adjusted <- data_adjusted %>%
    dplyr::mutate(
      # Ensure constructID exists and is properly formatted
      constructID = if(!is.null(constructIDCol) && constructIDCol %in% names(data_filtered)) {
        # Make sure we have a value and it's convertible to character
        id_vals <- .data[[constructIDCol]]
        if(all(is.na(id_vals))) {
          # If all NA, use a default
          "C1"
        } else {
          # If it's already a character that starts with C, use as is
          if(is.character(id_vals) && all(grepl("^C", na.omit(id_vals)))) {
            id_vals
          } else {
            # Otherwise add the C prefix
            paste0("C", as.character(id_vals))
          }
        }
      } else {
        # Default if column doesn't exist
        "C1"
      }
    )
  
  # Handle rCode with validation
  data_adjusted <- data_adjusted %>%
    dplyr::mutate(
      # Ensure rCode exists and is properly formatted
      rCode = if(!is.null(rCodeCol) && rCodeCol %in% names(data_filtered)) {
        # Use existing rCode
        .data[[rCodeCol]]
      } else {
        # Generate sequential codes if missing
        paste0("item", seq_len(n()))
      }
    )
  
  # Add qid if not present
  if(!"qid" %in% names(data_adjusted)) {
    data_adjusted$qid <- paste0(data_adjusted$constructID, "_Q", 1:nrow(data_adjusted))
  }
  
  # Make sure to preserve the original constructName
  if(constructNameCol %in% names(data_filtered) && !"constructName" %in% names(data_adjusted)) {
    data_adjusted <- data_adjusted %>%
      dplyr::mutate(constructName = .data[[constructNameCol]])
  } else if(!"constructName" %in% names(data_adjusted)) {
    # Default constructName if missing
    data_adjusted$constructName <- "Default Construct"
  }
  
  # Ungroup before continuing
  data_adjusted <- data_adjusted %>%
    dplyr::ungroup()
  
  # Always select the main columns
  cols_to_select <- c(respondentIDCol, "qid", "rCode", "qText", "response_value", "constructID", "constructName")
  
  # Filter to only include columns that actually exist
  cols_to_select <- intersect(cols_to_select, names(data_adjusted))
  
  # If the user wants demos, add them and process them
  if (includeDemos && !is.null(demoCols) && length(demoCols) > 0) {
    # Add demographic columns
    demo_cols_exist <- intersect(demoCols, names(data_filtered))
    
    if(length(demo_cols_exist) > 0) {
      cols_to_select <- c(cols_to_select, demo_cols_exist)
      
      # Process demographic columns based on metadata if provided
      if (!is.null(demoMetadata) && nrow(demoMetadata) > 0) {
        # Apply transformations according to metadata
        for (i in 1:nrow(demoMetadata)) {
          demo_col <- demoMetadata$column[i]
          
          # Only process columns that actually exist in the data
          if (demo_col %in% names(data_adjusted)) {
            demo_type <- demoMetadata$type[i]
            
            if (demo_type == "binary") {
              # Process binary variables
              if (!is.null(demoMetadata$value_labels[[i]]) && length(demoMetadata$value_labels[[i]]) > 0) {
                value_map <- demoMetadata$value_labels[[i]][[1]]
                
                data_adjusted <- data_adjusted %>%
                  dplyr::mutate(!!demo_col := dplyr::case_when(
                    is.na(.data[[demo_col]]) ~ demoMetadata$na_label[i],
                    as.character(.data[[demo_col]]) %in% names(value_map) ~ value_map[as.character(.data[[demo_col]])],
                    TRUE ~ as.character(.data[[demo_col]])
                  ))
              }
            } else if (demo_type == "categorical") {
              # Process categorical variables
              if (!is.null(demoMetadata$value_labels[[i]]) && length(demoMetadata$value_labels[[i]]) > 0) {
                value_map <- demoMetadata$value_labels[[i]][[1]]
                
                data_adjusted <- data_adjusted %>%
                  dplyr::mutate(!!demo_col := dplyr::case_when(
                    is.na(.data[[demo_col]]) ~ demoMetadata$na_label[i],
                    as.character(.data[[demo_col]]) %in% names(value_map) ~ value_map[as.character(.data[[demo_col]])],
                    TRUE ~ as.character(.data[[demo_col]])
                  ))
              }
            }
            # For continuous variables, no transformation needed
          }
        }
      }
    } else {
      warning("None of the requested demographic columns exist in the data.")
    }
  }
  
  # Return only the columns that exist
  data_selected <- data_adjusted %>%
    dplyr::select(dplyr::any_of(cols_to_select))
  
  # Store demo metadata as an attribute if provided
  if (!is.null(demoMetadata)) {
    attr(data_selected, "demo_metadata") <- demoMetadata
  }
  
  # Debug output - verify structure of returned data
  cat("Prepared data has", nrow(data_selected), "rows and", ncol(data_selected), "columns\n")
  cat("Column names:", paste(names(data_selected), collapse=", "), "\n")
  
  return(data_selected)
}