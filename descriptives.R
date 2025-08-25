library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(moments)
library(DT)
library(htmltools)
library(plotly)

# ---------------------- UI Function ------------------------------- #
descriptivesUI <- function(id, label = "Descriptives") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        h4("Descriptive Analytics"),
        helpText("This module automatically runs descriptive and exploratory analyses on the prepared survey data."),
        br(),
        # Settings panel instead of checkboxes
        wellPanel(
          h4("Visualization Settings"),
          selectInput(ns("facet_by"), "View Data By:", 
                      choices = c("Construct" = "constructName", "Question" = "qText"),
                      selected = "constructName"),
          uiOutput(ns("demographic_selector")),
          uiOutput(ns("item_selector"))
        ),
        br(),
        # Add description of what researchers can learn
        wellPanel(
          h4("Analysis Guide"),
          tags$p("This module helps researchers:"),
          tags$ul(
            tags$li("Understand central tendency and variability in survey responses"),
            tags$li("Identify demographic differences in response patterns"),
            tags$li("Detect constructs/questions with significant group disparities"),
            tags$li("Visualize response distributions across demographic groups")
          ),
          tags$p("Move between tabs to explore different aspects of your data.")
        )
      ),
      column(
        width = 9,
        tabsetPanel(
          tabPanel("Summary Statistics", 
                   br(),
                   h4("Construct-Level Summary"),
                   DT::DTOutput(ns("step2Table")),
                   br(),
                   h4("Question-Level Detail"),
                   DT::DTOutput(ns("step1Table")),
                   br(),
                   div(
                     em("Note: Asterisks on skewness/kurtosis indicate 'high' values (|skew| > 1 or kurtosis > 3) based on Kline (2011).")
                   )
          ),
          tabPanel("Demographic Analysis", 
                   br(),
                   h4("Group Differences"),
                   DT::DTOutput(ns("step3Table")),
                   br(),
                   div(
                     em("Note: For each question and demographic grouping, a significance test was performed (t-test for 2 groups; ANOVA for >2 groups). The p-values are shown in the 'p' column, with an asterisk (*) indicating p < 0.05. The 'Difference' column shows each group's mean response minus the overall mean.")
                   )
          ),
          tabPanel("Visualizations", 
                   br(),
                   # Navigation for items
                   fluidRow(
                     column(4, actionButton(ns("prev_item"), "← Previous", class = "btn-info")),
                     column(4, align="center", textOutput(ns("current_position"))),
                     column(4, align="right", actionButton(ns("next_item"), "Next →", class = "btn-info"))
                   ),
                   br(),
                   # Tabs for different visualization types
                   tabsetPanel(
                     tabPanel("Box Plots", 
                              br(),
                              plotlyOutput(ns("boxplot"), height = "500px")),
                     tabPanel("Heat Map", 
                              br(),
                              plotlyOutput(ns("heatmap_plot"), height = "500px"))
                    
                   )
          )
        )
      )
    )
  )  
}

# ---------------------- Server Function --------------------------- #
descriptivesServer <- function(id, adjustedData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    alpha_lvl <- 0.05  # significance level
    
    # Store the full analysis results
    # Store the full analysis results
    analysisResults <- reactive({
      req(adjustedData())
      processed_data <- adjustedData()
      
      # Get the demographic metadata if available
      demo_metadata <- attr(processed_data, "demo_metadata")
      
      ### Step 1: Descriptives by Question Text
      descriptives_by_qText <- processed_data %>%
        group_by(constructName, qText) %>%
        summarise(
          `Mean` = mean(response_value, na.rm = TRUE),
          `Median` = median(response_value, na.rm = TRUE),
          `Mode` = as.numeric(names(sort(table(response_value), decreasing = TRUE)[1])),
          `Std Dev` = sd(response_value, na.rm = TRUE),
          `Skewness` = moments::skewness(response_value, na.rm = TRUE),
          `Kurtosis` = moments::kurtosis(response_value, na.rm = TRUE),
          `Sample Size` = sum(!is.na(response_value)),
          .groups = "drop"
        ) %>%
        arrange(constructName) %>%
        mutate(
          `Mean` = round(`Mean`, 2),
          `Median` = round(`Median`, 2),
          `Std Dev` = round(`Std Dev`, 2),
          `Skewness` = paste0(round(`Skewness`, 2), ifelse(abs(as.numeric(`Skewness`)) > 1, "*", "")),
          `Kurtosis` = paste0(round(`Kurtosis`, 2), ifelse(as.numeric(`Kurtosis`) > 3, "*", ""))
        ) %>%
        rename(`Construct` = constructName, `Question` = qText)
      
      ### Step 2: Descriptives by constructName
      descriptives_by_constructName <- processed_data %>%
        group_by(constructName) %>%
        summarise(
          `Mean` = mean(response_value, na.rm = TRUE),
          `Median` = median(response_value, na.rm = TRUE),
          `Mode` = as.numeric(names(sort(table(response_value), decreasing = TRUE)[1])),
          `Std Dev` = sd(response_value, na.rm = TRUE),
          `Min` = min(response_value, na.rm = TRUE),
          `Max` = max(response_value, na.rm = TRUE),
          `Sample Size` = sum(!is.na(response_value)),
          `Question Count` = n_distinct(qText),
          .groups = "drop"
        ) %>%
        arrange(constructName) %>%
        mutate(
          `Mean` = round(`Mean`, 2),
          `Median` = round(`Median`, 2),
          `Std Dev` = round(`Std Dev`, 2)
        ) %>%
        rename(`Construct` = constructName)
      
      ### Step 3: Demographic Breakdowns with Group Differences
      # Identify demographic variables dynamically
      demographic_vars <- if (!is.null(demo_metadata)) {
        demo_metadata$column
      } else {
        # Fallback to finding columns that might be demographics
        # Look for columns that aren't the main analytical columns
        setdiff(
          names(processed_data),
          c("respondentID", "qid", "rCode", "qText", "response_value", 
            "constructID", "constructName")
        )
      }
      
      # Pivot data into long format
      long_demo <- processed_data %>%
        dplyr::select(constructName, qText, all_of(demographic_vars), response_value) %>%
        pivot_longer(
          cols = all_of(demographic_vars),
          names_to = "demographic",
          values_to = "group"
        )
      
      # Create helper function to handle mapping for each row properly
      map_group_value <- function(demo_val, group_val, demographic_vars, demo_metadata) {
        # First map the demographic display name
        if (demo_val %in% demo_metadata$column) {
          display_name <- demo_metadata$display_name[match(demo_val, demo_metadata$column)]
        } else {
          display_name <- demo_val
        }
        
        # Next handle the group value mapping
        if (is.na(group_val)) {
          # For NA values
          metadata_idx <- match(demo_val, demo_metadata$column)
          if (!is.na(metadata_idx)) {
            group_label <- demo_metadata$na_label[metadata_idx]
          } else {
            group_label <- "Not Specified"
          }
        } else {
          # For non-NA values
          metadata_idx <- match(demo_val, demo_metadata$column)
          group_str <- as.character(group_val)
          
          if (!is.na(metadata_idx) && 
              !is.null(demo_metadata$value_labels) && 
              length(demo_metadata$value_labels) >= metadata_idx &&
              !is.null(demo_metadata$value_labels[[metadata_idx]]) &&
              length(demo_metadata$value_labels[[metadata_idx]]) > 0 &&
              !is.null(demo_metadata$value_labels[[metadata_idx]][[1]])) {
            
            value_map <- demo_metadata$value_labels[[metadata_idx]][[1]]
            
            if (!is.null(value_map) && group_str %in% names(value_map)) {
              group_label <- value_map[group_str]
            } else {
              group_label <- group_str
            }
          } else {
            group_label <- group_str
          }
        }
        
        return(list(demographic = display_name, group = group_label))
      }
      
      # Process the data
      if (!is.null(demo_metadata)) {
        # Apply metadata transformations using a row-by-row approach
        result_list <- lapply(1:nrow(long_demo), function(i) {
          demo_val <- long_demo$demographic[i]
          group_val <- long_demo$group[i]
          mapped_values <- map_group_value(demo_val, group_val, demographic_vars, demo_metadata)
          
          # Return a data frame row with all original columns plus mapped values
          data.frame(
            constructName = long_demo$constructName[i],
            qText = long_demo$qText[i],
            response_value = long_demo$response_value[i],
            demographic = mapped_values$demographic,
            group = mapped_values$group,
            stringsAsFactors = FALSE
          )
        })
        
        # Combine results back into a data frame
        long_demo <- do.call(rbind, result_list)
      } else {
        # Basic demographic processing if no metadata
        long_demo <- long_demo %>%
          mutate(
            group = ifelse(is.na(group), "Not Specified", as.character(group)),
            # Basic formatting for demographic column names
            demographic = stringr::str_replace_all(demographic, "_", " ") %>%
              stringr::str_to_title()
          )
      }
      
      # Helper function to run significance tests (t-test if 2 groups; ANOVA if >2)
      run_group_test <- function(subdf, alpha = alpha_lvl) {
        subdf <- subdf %>% filter(!is.na(group), !is.na(response_value))
        unique_groups <- unique(subdf$group)
        if (length(unique_groups) < 2) {
          return(NA_real_)
        } else if (length(unique_groups) == 2) {
          t_res <- try(t.test(response_value ~ group, data = subdf), silent = TRUE)
          if (inherits(t_res, "try-error")) {
            return(NA_real_)
          }
          return(t_res$p.value)
        } else {
          aov_res <- try(aov(response_value ~ group, data = subdf), silent = TRUE)
          if (inherits(aov_res, "try-error")) {
            return(NA_real_)
          }
          return(summary(aov_res)[[1]][["Pr(>F)"]][1])
        }
      }
      
      # Compute p-value for each (constructName, qText, demographic)
      long_demo_test <- long_demo %>%
        group_by(constructName, qText, demographic) %>%
        mutate(p_val = run_group_test(cur_data())) %>%
        ungroup()
      
      # First calculate overall means for each construct/question from the original data
      overall_means <- processed_data %>%
        group_by(constructName, qText) %>%
        summarise(
          `True Overall Mean` = mean(response_value, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Summarize by (constructName, qText, demographic, group) and add p-value and significance flag
      demographic_summary_qid <- long_demo_test %>%
        group_by(constructName, qText, demographic, group, p_val) %>%
        summarise(
          `Mean` = mean(response_value, na.rm = TRUE),
          `Std Dev` = sd(response_value, na.rm = TRUE),
          `N` = sum(!is.na(response_value)),
          .groups = "drop"
        ) %>%
        arrange(constructName, qText, demographic, group) %>%
        mutate(
          `Mean` = round(`Mean`, 2),
          `Std Dev` = round(`Std Dev`, 2),
          `p` = round(p_val, 3),
          `Sig.` = ifelse(!is.na(p_val) & p_val < alpha_lvl, "*", "")
        ) %>%
        dplyr::select(-p_val) %>%
        # Join with the true overall means
        left_join(overall_means, by = c("constructName", "qText")) %>%
        # Calculate group average and difference from true overall mean
        group_by(constructName, qText, demographic) %>%
        mutate(
          `Group Average` = mean(`Mean`, na.rm = TRUE),
          `Difference` = round(`Mean` - `True Overall Mean`, 2)
        ) %>%
        ungroup() %>%
        rename(`Construct` = constructName, `Question` = qText, `Demographic` = demographic, `Group` = group)
      
      list(
        step1 = descriptives_by_qText,
        step2 = descriptives_by_constructName,
        step3 = demographic_summary_qid
      )
    })
    # ---- Render Step 1 Table (Descriptives by Question Text) ----
    output$step1Table <- DT::renderDT({
      res <- analysisResults()
      req(res$step1)
      
      DT::datatable(
        res$step1,
        rownames = FALSE,
        options = list(
          paging = TRUE,
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf')
        ),
        extensions = 'Buttons',
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left;",
          htmltools::HTML("
            <strong>Methodology Note:</strong> 
            For each question, descriptive statistics are computed with values rounded to two decimals. 
            Asterisks on skewness/kurtosis indicate values above thresholds (|skew| > 1 or kurtosis > 3). 
            (Kline, 2011)
          ")
        )
      ) %>%
        formatStyle(
          columns = c('Skewness', 'Kurtosis'),
          valueContains = '*',
          backgroundColor = 'rgba(255, 235, 235, 0.8)',
          fontWeight = 'bold'
        )
    })
    
    # ---- Render Step 2 Table (Descriptives by constructName) ----
    output$step2Table <- DT::renderDT({
      res <- analysisResults()
      req(res$step2)
      
      DT::datatable(
        res$step2,
        rownames = FALSE,
        options = list(
          paging = TRUE,
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf')
        ),
        extensions = 'Buttons',
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left;",
          htmltools::HTML("
            <strong>Note:</strong> All numeric columns are rounded to two decimals.
          ")
        )
      )
    })
    
    # ---- Render Step 3 Table (Demographic Breakdowns with Group Differences) ----
    output$step3Table <- DT::renderDT({
      res <- analysisResults()
      req(res$step3)
      
      # Calculate range for difference column to create symmetric color scale
      diff_range <- max(abs(range(res$step3$Difference, na.rm = TRUE)))
      
      DT::datatable(
        res$step3,
        rownames = FALSE,
        options = list(
          paging = TRUE,
          pageLength = 15,
          scrollX = TRUE,
          searching = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf')
        ),
        extensions = 'Buttons',
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left;",
          htmltools::HTML("
            <strong>Methodology Note:</strong> 
            For each question and demographic grouping, a significance test (t-test for 2 groups; ANOVA for >2 groups) was performed. 
            The 'p' column is rounded to three decimals, with an asterisk (*) indicating p < 0.05. 
            The 'Difference' column represents the difference between a group's mean response and the TRUE overall mean calculated from ALL respondents.
            Positive values (blue) indicate above-average responses and negative values (red) indicate below-average responses.
          ")
        )
      ) %>%
        formatStyle(
          columns = 'Difference',
          background = styleInterval(
            cuts = c(-diff_range/2, -diff_range/4, diff_range/4, diff_range/2),
            values = c('rgba(255, 150, 150, 0.8)', 'rgba(255, 200, 200, 0.5)', 
                       'rgba(255, 255, 255, 0)', 
                       'rgba(200, 200, 255, 0.5)', 'rgba(150, 150, 255, 0.8)')
          ),
          fontWeight = styleInterval(c(-diff_range/2, diff_range/2), c('bold', 'normal', 'bold'))
        ) %>%
        formatStyle(
          columns = 'Difference',
          color = styleInterval(0, c('darkred', 'darkblue'))
        ) %>%
        formatStyle(
          columns = 'Sig.',
          valueContains = '*',
          backgroundColor = 'rgba(255, 235, 235, 0.7)',
          fontWeight = 'bold'
        )
    })
    
    # ---- Dynamic filters for visualizations ----
    # Get available demographics
    available_demographics <- reactive({
      res <- analysisResults()
      req(res$step3)
      unique(res$step3$Demographic)
    })
    
    # Get available items (constructs or questions) based on facet_by selection
    available_items <- reactive({
      res <- analysisResults()
      req(res$step3)
      req(input$facet_by)
      
      if(input$facet_by == "constructName") {
        return(unique(res$step3$Construct))
      } else {
        return(unique(res$step3$Question))
      }
    })
    
    # Track current item index for navigation
    current_item_index <- reactiveVal(1)
    
    # Reset index when facet_by changes
    observeEvent(input$facet_by, {
      current_item_index(1)
    })
    
    # Create demographic dropdown
    output$demographic_selector <- renderUI({
      selectInput(ns("selected_demographic"), 
                  "Select Demographic:",
                  choices = available_demographics(),
                  selected = available_demographics()[1])
    })
    
    # Create item dropdown (construct or question)
    output$item_selector <- renderUI({
      selectInput(ns("selected_item"),
                  paste("Select", ifelse(input$facet_by == "constructName", "Construct:", "Question:")),
                  choices = available_items(),
                  selected = available_items()[current_item_index()])
    })
    
    # Update selected item when navigation buttons are clicked
    observeEvent(input$next_item, {
      items <- available_items()
      curr_idx <- current_item_index()
      if(curr_idx < length(items)) {
        current_item_index(curr_idx + 1)
        updateSelectInput(session, "selected_item", selected = items[current_item_index()])
      }
    })
    
    observeEvent(input$prev_item, {
      items <- available_items()
      curr_idx <- current_item_index()
      if(curr_idx > 1) {
        current_item_index(curr_idx - 1)
        updateSelectInput(session, "selected_item", selected = items[current_item_index()])
      }
    })
    
    # Update current_item_index when selected_item changes
    observeEvent(input$selected_item, {
      selected <- input$selected_item
      items <- available_items()
      match_idx <- match(selected, items)
      if(!is.na(match_idx)) {
        current_item_index(match_idx)
      }
    })
    
    # Show current position
    output$current_position <- renderText({
      items <- available_items()
      curr_idx <- current_item_index()
      paste("Item", curr_idx, "of", length(items))
    })
    
    # Filtered plot data
    filtered_plot_data <- reactive({
      res <- analysisResults()
      req(res$step3, input$selected_demographic, input$selected_item)
      
      filter_var <- ifelse(input$facet_by == "constructName", "Construct", "Question")
      filter_val <- input$selected_item
      
      filtered <- res$step3 %>%
        filter(Demographic == input$selected_demographic,
               !!sym(filter_var) == filter_val)
      
      # If no data after filtering, return NULL
      if(nrow(filtered) == 0) return(NULL)
      
      return(filtered)
    })
    

    
    # Heatmap
    output$heatmap_plot <- renderPlotly({
      res <- analysisResults()
      req(res$step3, input$selected_demographic)
      
      # Filter for selected demographic
      heatmap_data <- res$step3 %>%
        filter(Demographic == input$selected_demographic) %>%
        mutate(DisplayName = if(input$facet_by == "constructName") Construct else Question)
      
      # Handle empty data
      if(nrow(heatmap_data) == 0) {
        return(plot_ly() %>% 
                 add_annotations(
                   text = "No data available for this selection",
                   showarrow = FALSE,
                   font = list(size = 14)
                 )
        )
      }
      
      # Get unique items and groups
      items <- unique(heatmap_data$DisplayName)
      groups <- unique(heatmap_data$Group)
      
      # Prepare data in long format for plotly
      plot_data <- data.frame()
      
      for (item in items) {
        for (group in groups) {
          row_data <- heatmap_data %>% 
            filter(DisplayName == item, Group == group)
          
          if(nrow(row_data) > 0) {
            plot_data <- rbind(plot_data, data.frame(
              Item = item,
              Group = group,
              Difference = row_data$Difference,
              Mean = row_data$Mean,
              N = row_data$N,
              Sig = row_data$Sig.,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      # Only proceed if we have data
      if(nrow(plot_data) == 0) {
        return(plot_ly() %>% 
                 add_annotations(
                   text = "No data available for this selection",
                   showarrow = FALSE,
                   font = list(size = 14)
                 )
        )
      }
      
      # Max absolute value for symmetric color scale
      max_abs <- max(abs(plot_data$Difference), na.rm = TRUE)
      
      # Create the heatmap
      p <- plot_ly(
        data = plot_data,
        x = ~Group, 
        y = ~Item,
        z = ~Difference,
        type = "heatmap",
        colorscale = list(
          c(0, "rgb(255,0,0)"),
          c(0.5, "rgb(255,255,255)"),
          c(1, "rgb(0,0,255)")
        ),
        zmin = -max_abs,
        zmax = max_abs,
        text = ~paste0(
          "Item: ", Item, "<br>",
          "Group: ", Group, "<br>",
          "Mean: ", Mean, "<br>",
          "Difference: ", Difference, "<br>",
          "Sample Size: ", N, "<br>",
          ifelse(Sig == "*", "Statistically significant", "Not statistically significant")
        ),
        hoverinfo = "text"
      ) 
      
      # Update the layout
      p <- p %>% layout(
        title = paste("Differences by", ifelse(input$facet_by == "constructName", "Construct", "Question"), 
                      "for", input$selected_demographic),
        xaxis = list(title = "Group"),
        yaxis = list(title = ifelse(input$facet_by == "constructName", "Construct", "Question")),
        colorbar = list(
          title = "Difference from Population Mean"
        )
      )
      
      # Add information text
      info_text <- "Interpretation Guide: This heatmap shows differences between group means and the overall population mean.
                   Blue cells indicate groups scoring above the overall average (positive difference),
                   while red cells indicate groups scoring below average (negative difference).
                   The intensity of color represents the magnitude of difference.
                   Use this visualization to quickly identify patterns across multiple constructs/questions
                   and spot where certain demographic groups consistently differ from the population average."
      
      # Add annotation with descriptive text
      p <- p %>% layout(
        annotations = list(
          list(
            x = 0.5,
            y = -0.15,
            text = info_text,
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            font = list(size = 10)
          )
        )
      )
      
      p
    })
    
    # Box Plots
    output$boxplot <- renderPlotly({
      plot_data <- filtered_plot_data()
      req(plot_data)
      req(adjustedData())
      data_merge_grade <- adjustedData()
      
      # Get raw data for boxplot
      filter_var <- ifelse(input$facet_by == "constructName", "constructName", "qText")
      filter_val <- input$selected_item
      
      # Extract demographic column name - try several patterns to match bin columns
      possible_patterns <- c(
        paste0("^", tolower(gsub(" .*$", "", input$selected_demographic)), "_bin$"),
        paste0("^", tolower(gsub("\\W+", "", input$selected_demographic)), "_bin$"),
        "_bin$"
      )
      
      demo_col <- NULL
      for(pattern in possible_patterns) {
        matches <- grep(pattern, names(data_merge_grade), value = TRUE, ignore.case = TRUE)
        if(length(matches) > 0) {
          demo_col <- matches[1]
          break
        }
      }
      
      # If we still can't find the column, try a fallback method
      if(is.null(demo_col)) {
        # Get the first binary column as a fallback
        bin_cols <- grep("_bin$", names(data_merge_grade), value = TRUE)
        if(length(bin_cols) > 0) {
          demo_col <- bin_cols[1]
        } else {
          # Create a dummy plot with error message
          p <- plot_ly() %>%
            add_annotations(
              text = "Unable to find matching demographic column",
              showarrow = FALSE,
              font = list(size = 14)
            )
          return(p)
        }
      }
      
      # Filter raw data 
      boxplot_data <- data_merge_grade %>%
        filter(!!sym(filter_var) == filter_val) %>%
        dplyr::select(!!sym(demo_col), response_value) %>%
        rename(group = !!sym(demo_col)) %>%
        # Handle NA and convert to factor
        mutate(
          group = case_when(
            is.na(group) & grepl("ell", demo_col, ignore.case = TRUE) ~ "Not ELL",
            is.na(group) & grepl("frl", demo_col, ignore.case = TRUE) ~ "Not FRL",
            is.na(group) & grepl("grade", demo_col, ignore.case = TRUE) ~ "Grade Not Specified",
            is.na(group) & grepl("gt", demo_col, ignore.case = TRUE) ~ "Not GT",
            is.na(group) & grepl("iep", demo_col, ignore.case = TRUE) ~ "Not IEP",
            is.na(group) & grepl("race", demo_col, ignore.case = TRUE) ~ "No Race Specified",
            TRUE ~ as.character(group)
          ),
          # Convert binary values to meaningful labels if needed
          group = case_when(
            group == "1" & grepl("grade", demo_col, ignore.case = TRUE) ~ "Upper School",
            group == "0" & grepl("grade", demo_col, ignore.case = TRUE) ~ "Lower School",
            group == "1" ~ "Yes",
            group == "0" ~ "No",
            TRUE ~ group
          )
        )
      
      # Create boxplot
      p <- plot_ly(boxplot_data, y = ~response_value, color = ~group, type = "box")
      
      # Update layout
      p <- p %>% layout(
        title = if(input$facet_by == "constructName") {
          paste("Distribution for Construct:", input$selected_item, "- Demographic:", input$selected_demographic)
        } else {
          paste("Distribution for Question:", input$selected_item, "- Demographic:", input$selected_demographic)
        },
        xaxis = list(title = ""),
        yaxis = list(title = "Response Value")
      )
      
      # Add information text
      info_text <- "Interpretation Guide: Box plots show the full distribution of responses by group, not just averages.
                   The box shows the interquartile range (middle 50% of data) with the horizontal line marking the median.
                   Whiskers extend to the min/max values (excluding outliers, shown as individual points).
                   Use this visualization to identify not just differences in central tendency, but also in spread,
                   skew, and presence of outliers between demographic groups."
      
      # Add annotation with descriptive text
      p <- p %>% layout(
        annotations = list(
          list(
            x = 0.5,
            y = -0.15,
            text = info_text,
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            font = list(size = 10)
          )
        )
      )
      
      p
    })
  })
}
