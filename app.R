# app.R - Non-reactive version for current year only

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(readr)
library(dplyr)
library(ggplot2)
library(flextable)
library(htmltools)
library(lubridate)

# Source your helper functions
source("R/report_helpers.R")

# Set current year (no user selection)
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Enhanced UI section for app.R

ui <- dashboardPage(
  dashboardHeader(title = "Lostine River Weir - Chinook Summary"),
  
  dashboardSidebar(
    h4(paste("Current Year:", current_year), style = "color: white; text-align: center; margin: 20px 0;"),
    
    br(), br(),
    div(style = "margin: 20px 10px;",
        p("Last Data Update:", style = "font-size: 12px; color: gray; margin-bottom: 5px;"),
        textOutput("last_update", inline = TRUE)
    ),
  
  br(), br(),
  
  # Contact Information Section
  div(style = "margin: 20px 10px; padding: 15px; background-color: rgba(255,255,255,0.1); border-radius: 5px;",
      h5("Contact Information", style = "color: white; text-align: center; margin-bottom: 15px; font-weight: bold;"),
      
      div(style = "color: white; font-size: 11px; line-height: 1.4; margin-bottom: 15px;",
          p(strong("Neal Espinosa"), style = "margin: 0; font-size: 12px;"),
          p("Northeast Oregon Natural and Hatchery Salmonid Monitoring", style = "margin: 2px 0;"),
          p("Biologist II", style = "margin: 2px 0;"),
          p("541-432-2502", style = "margin: 2px 0;"),
          p(a("neale@nezperce.org", href = "mailto:neale@nezperce.org", 
              style = "color: #87CEEB; text-decoration: none;"), style = "margin: 2px 0;")
      ),
      
      div(style = "color: white; font-size: 11px; line-height: 1.4; margin-bottom: 15px;",
          p(strong("Brian Simmons"), style = "margin: 0; font-size: 12px;"),
          p("Northeast Oregon Natural and Hatchery Salmonid Monitoring", style = "margin: 2px 0;"),
          p("Project Leader", style = "margin: 2px 0;"),
          p("541-432-2515", style = "margin: 2px 0;"),
          p(a("brians@nezperce.org", href = "mailto:brians@nezperce.org", 
              style = "color: #87CEEB; text-decoration: none;"), style = "margin: 2px 0;")
      ),
      
      div(style = "color: white; font-size: 11px; line-height: 1.4; text-align: center; border-top: 1px solid rgba(255,255,255,0.3); padding-top: 10px;",
          p(strong("Nez Perce Tribe"), style = "margin: 2px 0; font-size: 12px;"),
          p("Joseph Field Office", style = "margin: 2px 0;"),
          p("500 North Main Street", style = "margin: 2px 0;"),
          p("P.O. Box 909", style = "margin: 2px 0;"),
          p("Joseph, OR 97846", style = "margin: 2px 0;")
      )
  )
),

  dashboardBody(
    # Enhanced CSS to match PDF styling
    tags$head(
      tags$style(HTML("
        .main-header .navbar {
          background-color: #2c3e50 !important;
        }
        .content-wrapper {
          background-color: #ecf0f1;
        }
        .box {
          border-top-color: #2c3e50;
        }
        .no-data-message {
          font-style: italic;
          text-align: center;
          padding: 20px;
          background-color: #f8f9fa;
          border: 1px solid #dee2e6;
          border-radius: 5px;
          margin: 10px 0;
        }
        #last_update {
          font-size: 12px;
          color: white;
        }
        .npt-header {
          background-color: white;
          padding: 20px;
          margin-bottom: 20px;
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .npt-logo-side {
          max-height: 120px;
          width: auto;
          margin: 0 15px;
        }
        .header-title {
          color: #2c3e50;
          font-weight: bold;
          margin: 0;
          text-align: center;
        }
        .header-subtitle {
          color: #34495e;
          margin: 5px 0 0 0;
          text-align: center;
          font-style: italic;
        }
        .disposition-box .box-header {
          background-color: #3498db;
          color: white;
        }
        .table-box .box-header {
          background-color: #5bc0de;
          color: white;
        }
        .plot-box .box-header {
          background-color: #5cb85c;
          color: white;
        }
        .river-box .box-header {
          background-color: #f0ad4e;
          color: white;
        }
        /* Make disposition summary text more readable */
        .disposition-summary ul {
          font-size: 14px;
          line-height: 1.6;
        }
        .disposition-summary li {
          margin-bottom: 8px;
        }
      "))
    ),
    
    fluidRow(
      # Enhanced Header with dual NPT logos
      column(12,
             div(class = "npt-header",
                 div(style = "display: flex; align-items: center; justify-content: space-between;",
                     # Left logo - Treaty
                     div(style = "flex: 0 0 auto;",
                         img(src = "npt_treaty_logo.png", class = "npt-logo-side", 
                             alt = "Nez Perce Tribe")
                     ),
                     # Center titles
                     div(style = "flex: 1; text-align: center;",
                         h1("Lostine River Weir", class = "header-title"),
                         h3(paste("Weekly Chinook Summary:", format(Sys.Date(), "%B %d, %Y")), 
                            class = "header-subtitle")
                     ),
                     # Right logo - Fisheries
                     div(style = "flex: 0 0 auto;",
                         img(src = "npt_fisheries_logo.png", class = "npt-logo-side", 
                             alt = "Nez Perce Tribe Fisheries")
                     )
                 )
             )
      )
    ),
    
    fluidRow(
      # Enhanced Disposition Summary
      column(12,
             div(class = "disposition-box",
                 box(width = 12, title = "Forecasts, Goals, and Disposition Summary", 
                     status = "primary", solidHeader = TRUE,
                     div(class = "disposition-summary",
                         htmlOutput("disposition_summary")
                     )
                 )
             )
      )
    ),
    
    fluidRow(
      # Enhanced Tables with Always-Visible Captions
      column(6,
             div(class = "table-box",
                 box(width = 12, title = "Hatchery Chinook Disposition", 
                     status = "info", solidHeader = TRUE,
                     # Add caption above table
                     div(style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #5bc0de; font-size: 12px; font-style: italic;",
                         textOutput("caption_table1")
                     ),
                     htmlOutput("hatchery_table")
                 )
             )
      ),
      column(6,
             div(class = "table-box",
                 box(width = 12, title = "Natural Chinook Disposition", 
                     status = "info", solidHeader = TRUE,
                     # Add caption above table
                     div(style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #5bc0de; font-size: 12px; font-style: italic;",
                         textOutput("caption_table2")
                     ),
                     htmlOutput("natural_table")
                 )
             )
      )
    ),
    
    fluidRow(
      # Enhanced Broodstock table with Caption
      column(12,
             div(class = "table-box",
                 box(width = 12, title = "Broodstock Collection Summary", 
                     status = "info", solidHeader = TRUE,
                     # Add caption above table
                     div(style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #5bc0de; font-size: 12px; font-style: italic;",
                         textOutput("caption_table3")
                     ),
                     htmlOutput("broodstock_table")
                 )
             )
      )
    ),
    
    fluidRow(
      # Enhanced Main plot with Caption
      column(12,
             div(class = "plot-box",
                 box(width = 12, title = "Current and Historic Catch and River Flows", 
                     status = "success", solidHeader = TRUE,
                     plotlyOutput("megaplot", height = "600px"),
                     # Add caption below plot
                     div(style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #5cb85c; font-size: 12px; font-style: italic;",
                         textOutput("caption_plot")
                     )
                 )
             )
      )
    ),
    
    fluidRow(
      # Enhanced NOAA link section
      column(12,
             div(class = "river-box",
                 box(width = 12, title = "Current River Conditions", 
                     status = "warning", solidHeader = TRUE,
                     div(style = "text-align: center; padding: 20px;",
                         h4("🌊 Real-Time River Data"),
                         p("Current gauge shows both observed flow data and official 7-day forecasts for the Lostine River above Lostine (NWSLI: LSTO3)."),
                         br(),
                         actionButton("noaa_link", "View Live NOAA Gauge Data & Forecast", 
                                      onclick = "window.open('https://water.noaa.gov/gauges/lsto3', '_blank')",
                                      class = "btn-primary btn-lg"),
                         br(), br(),
                         p("Click above for interactive graphs, current readings, and flood predictions", 
                           style = "font-style: italic; color: gray;")
                     )
                 )
             )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Load all data at startup (non-reactive) - ENHANCED
  estimates_data <- load_yearly_estimates(current_year)
  
  trap_data <- get_trap_data(trap.year = current_year)
  grsme_df <- trap_data$grsme_df
  
  # Store the actual data timestamp and source
  actual_data_timestamp <- trap_data$data_timestamp
  data_source_info <- trap_data$data_source
  
  # Calculate dispositions and extract components (updated section in app.R server)
  dispositions_result <- calculate_dispositions(grsme_df, current_year)
  h_df <- dispositions_result$h_df
  n_df <- dispositions_result$n_df
  h_upstream_calc <- dispositions_result$h_upstream_calc
  n_brood_calc <- dispositions_result$n_brood_calc
  
  # Extract broodstock summary numbers (now including jacks)
  n_brood_sum <- dispositions_result$n_brood_sum
  h_brood_sum <- dispositions_result$h_brood_sum
  hj_brood_sum <- dispositions_result$hj_brood_sum
  total_brood_sum <- dispositions_result$total_brood_sum
  
  # Extract adult capture totals
  n_adults <- dispositions_result$n_adults
  h_adults <- dispositions_result$h_adults
  total_adults <- dispositions_result$total_adults
  
  # Use broodstock data from dispositions result
  broodstock_data <- dispositions_result$broodstock_data
  
  # Prepare plot data
  mega_data <- prepare_megadf(
    trap.year = current_year,
    grsme_df = grsme_df,
    weir_data_clean = trap_data$AdultWeirData_clean
  )
  
  # UPDATED: Last update time - prioritize GitHub data timestamp
  output$last_update <- renderText({
    # Always use actual_data_timestamp if available (from GitHub)
    if (!is.null(actual_data_timestamp)) {
      format(actual_data_timestamp, "%m/%d/%Y %H:%M")
    } 
    # Only fall back to local file if no GitHub timestamp
    else if (file.exists("data/TrappingData.csv")) {
      file_time <- file.info("data/TrappingData.csv")$mtime
      format(file_time, "%m/%d/%Y %H:%M")
    } else {
      "No data available"
    }
  })
  
  # Forecasts, Goals, and Disposition Summary (updated with adult captures and jacks)
  output$disposition_summary <- renderUI({
    estimates <- estimates_data
    
    # Calculate total broodstock goal
    total_brood_goal <- estimates$n_brood_goal + estimates$h_brood_goal + estimates$hj_brood_goal
    
    HTML(paste(
      "<ul>",
      paste0("<li>", estimates$estimate_type, " adult return-to-tributary estimates were updated on ", 
             estimates$estimate_date, " to ", estimates$nat_adults, " natural-origin and ", 
             estimates$hat_adults, " hatchery-origin adults.</li>"),
      paste0("<li><strong>Adult summer Chinook Salmon trapped to date: ", total_adults, " total (", 
             n_adults, " natural-origin adults, ", 
             h_adults, " hatchery-origin adults).</strong></li>"),
      paste0("<li>Brood stock collection goals: ", total_brood_goal, " total (", 
             estimates$n_brood_goal, " natural-origin adults, ", 
             estimates$h_brood_goal, " hatchery-origin adults, ", 
             estimates$hj_brood_goal, " hatchery-origin jacks).</li>"),
      paste0("<li><strong>Brood stock collected to date: ", total_brood_sum, " of ", total_brood_goal, 
             " total (", n_brood_sum, " natural-origin adults, ", 
             h_brood_sum, " hatchery-origin adults, ", 
             hj_brood_sum, " hatchery-origin jacks).</strong></li>"),
      paste0("<li>Composition of adults passed upstream: ", h_upstream_calc, "% Hatchery (Sliding scale goal ≤ ", estimates$ss_upstream, ")</li>"),
      paste0("<li>Composition of adults kept for brood: ", n_brood_calc, "% Natural (Sliding scale goal ≥ ", estimates$ss_brood, ")</li>"),
      "</ul>"
    ))
  })
  
  # Caption outputs for tables and plot
  output$caption_table1 <- renderText({
    prepare_caption_table1(current_year)
  })
  
  output$caption_table2 <- renderText({
    prepare_caption_table2(current_year)
  })
  
  output$caption_table3 <- renderText({
    prepare_caption_table3(current_year)
  })
  
  output$caption_plot <- renderText({
    prepare_caption_plot(current_year)
  })
  
  # Helper function to create safe HTML tables
  create_safe_table <- function(data, table_type, trap_year) {
    if(is.null(data) || nrow(data) == 0) {
      message <- case_when(
        table_type == "hatchery" ~ paste0("There is currently no data available for the capture of hatchery-origin Chinook for ", trap_year, "."),
        table_type == "natural" ~ paste0("There is currently no data available for the capture of natural-origin Chinook for ", trap_year, "."),
        table_type == "broodstock" ~ paste0("There is currently no broodstock collection data available for ", trap_year, "."),
        TRUE ~ paste0("There is currently no data available for ", trap_year, ".")
      )
      return(HTML(paste0('<div class="no-data-message">', message, '</div>')))
    }
    
    # Check if all numeric values are zero (for disposition tables)
    if(table_type %in% c("hatchery", "natural") && nrow(data) > 0) {
      count_cols <- data[, 2:ncol(data), drop = FALSE]
      numeric_values <- c()
      for(i in 1:nrow(count_cols)) {
        for(j in 1:ncol(count_cols)) {
          cell_value <- as.character(count_cols[i, j])
          numbers <- as.numeric(unlist(regmatches(cell_value, gregexpr("\\d+", cell_value))))
          numeric_values <- c(numeric_values, numbers)
        }
      }
      numeric_values <- numeric_values[!is.na(numeric_values)]
      if(length(numeric_values) > 0 && all(numeric_values == 0)) {
        message <- paste0("There is currently no data available for the capture of ", 
                          ifelse(table_type == "hatchery", "hatchery", "natural"), 
                          "-origin Chinook for ", trap_year, ".")
        return(HTML(paste0('<div class="no-data-message">', message, '</div>')))
      }
    }
    
    # Create DT table for display
    datatable(data, options = list(
      dom = 't',  # Only show table
      pageLength = -1,  # Show all rows
      ordering = FALSE,
      searching = FALSE,
      info = FALSE
    )) |> 
      formatStyle(columns = 1:ncol(data), textAlign = 'center')
  }
  
  # Disposition Tables (non-reactive)
  output$hatchery_table <- renderUI({
    create_safe_table(h_df, "hatchery", current_year)
  })
  
  output$natural_table <- renderUI({
    create_safe_table(n_df, "natural", current_year)
  })
  
  output$broodstock_table <- renderUI({
    create_safe_table(broodstock_data, "broodstock", current_year)
  })
  
  # Main plot (non-reactive)
  output$megaplot <- renderPlotly({
    plot <- generate_lrw_megaplot(
      megadf = mega_data$lrw_megadf,
      lrw_catch = mega_data$lrw_megadf |> filter(facet == as.character(current_year)),
      save_plot = FALSE
    )
    
    ggplotly(plot, tooltip = c("x", "y")) |>
      layout(showlegend = TRUE)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)