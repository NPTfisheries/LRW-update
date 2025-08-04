# R/report_helpers.R

library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(flextable)
# library(cuyem)  # REMOVED - using local functions instead
library(stringr)

# ---- Source local cuyem functions and other dependencies ----
# Determine correct paths based on working directory
if (basename(getwd()) == "documents") {
  # Running from documents/ folder (Quarto)
  source("../R/local_cuyem_functions.R")  # Local cuyem functions
  source("../R/sumGRSMEdisp.R")           # FINS Disposition Summary
  source("../R/sumGRSMEbrood.R")          # Brood Collection Summary
} else {
  # Running from root directory (Shiny app)
  source("R/local_cuyem_functions.R")     # Local cuyem functions
  source("R/sumGRSMEdisp.R")              # FINS Disposition Summary
  source("R/sumGRSMEbrood.R")             # Brood Collection Summary
}

# ---- Load Yearly Estimates ----
load_yearly_estimates <- function(year, path = NULL) {
  
  # Determine correct path based on working directory if not specified
  if (is.null(path)) {
    if (basename(getwd()) == "documents") {
      path <- "../data/yearly_estimates.csv"
    } else {
      path <- "data/yearly_estimates.csv"
    }
  }
  
  read_csv(path, show_col_types = FALSE) |>
    filter(year == !!year) |>
    mutate(estimate_date = mdy(estimate_date)) |>
    filter(estimate_date == max(estimate_date, na.rm = TRUE)) |>
    slice(1)
}
#---- Load and clean Weir Data ----

# Replace your get_trap_data function in report_helpers.R with this GitHub-only version:

    get_trap_data <- function(trap.year = NULL) {
      
      # Initialize variables
      data_timestamp <- NULL
      
      # Public GitHub URL - always use this
      github_url <- "https://raw.githubusercontent.com/NPTfisheries/LRW-update/refs/heads/master/data/TrappingData.csv"
      
      message("Loading data from public GitHub repository...")
      
      # Load data directly from GitHub
      fins_data <- read_csv(github_url, show_col_types = FALSE)
      
      # Get actual file timestamp from GitHub API (public access)
      tryCatch({
        if (requireNamespace("httr", quietly = TRUE)) {
          library(httr)
          
          # GitHub API for public repos - no auth needed
          api_url <- "https://api.github.com/repos/NPTfisheries/LRW-update/commits?path=data/TrappingData.csv&per_page=1"
          response <- GET(api_url)
          
          if (status_code(response) == 200) {
            commits <- content(response, as = "parsed")
            if (length(commits) > 0) {
              commit_date <- commits[[1]]$commit$author$date
              data_timestamp <- as.POSIXct(commit_date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
              # Convert to Pacific Time
              data_timestamp <- as.POSIXct(format(data_timestamp, tz = "America/Los_Angeles"), tz = "America/Los_Angeles")
              message("✅ Got actual file timestamp from GitHub: ", format(data_timestamp, "%Y-%m-%d %H:%M:%S"))
            }
          }
        }
      }, error = function(e) {
        message("Could not get GitHub timestamp, using current time")
        data_timestamp <- Sys.time()
      })
      
      # Fallback to current time if API didn't work
      if (is.null(data_timestamp)) {
        data_timestamp <- Sys.time()
      }
      
      message("✅ Successfully loaded data from public GitHub repository")
      
      # Clean weir data
      AdultWeirData_clean <- clean_weirData(fins_data) |>
        mutate(
          MonthDay = format(as.Date(trapped_date), "%m/%d"),
          count = as.double(count)
        )
      
      # Apply year filter if specified
      if (!is.null(trap.year)) {
        grsme_df <- AdultWeirData_clean |> filter(trap_year == !!trap.year)
      } else {
        grsme_df <- AdultWeirData_clean
      }
      
      # Return data with timestamp and source info
      list(
        AdultWeirData_clean = AdultWeirData_clean,
        grsme_df = grsme_df,
        data_timestamp = data_timestamp,
        data_source = "GitHub (public)"
      )
    }

#--- OLD Load and Clean Weir Data with local backup----

            # get_trap_data <- function(trap.year = NULL, use_github = TRUE) {
            #   
            #   # Initialize variables
            #   fins_data <- NULL
            #   data_timestamp <- NULL
            #   data_source <- "Unknown"
            #   
            #   if (use_github) {
            #     # Simple public GitHub URL - no authentication needed!
            #     github_url <- "https://raw.githubusercontent.com/NPTfisheries/LRW-update/refs/heads/master/data/TrappingData.csv"
            #     
            #     tryCatch({
            #       message("Loading data from public GitHub repository...")
            #       
            #       # Simple direct read - no authentication needed
            #       fins_data <- read_csv(github_url, show_col_types = FALSE)
            #       
            #       # Get actual file timestamp from GitHub API (public access)
            #       tryCatch({
            #         if (requireNamespace("httr", quietly = TRUE)) {
            #           library(httr)
            #           
            #           # GitHub API for public repos - no auth needed
            #           api_url <- "https://api.github.com/repos/NPTfisheries/LRW-update/commits?path=data/TrappingData.csv&per_page=1"
            #           response <- GET(api_url)
            #           
            #           if (status_code(response) == 200) {
            #             commits <- content(response, as = "parsed")
            #             if (length(commits) > 0) {
            #               commit_date <- commits[[1]]$commit$author$date
            #               data_timestamp <- as.POSIXct(commit_date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
            #               # Convert to Pacific Time
            #               data_timestamp <- as.POSIXct(format(data_timestamp, tz = "America/Los_Angeles"), tz = "America/Los_Angeles")
            #               message("✅ Got actual file timestamp from GitHub: ", format(data_timestamp, "%Y-%m-%d %H:%M:%S"))
            #             }
            #           }
            #         }
            #       }, error = function(e) {
            #         message("Could not get GitHub timestamp, using current time")
            #       })
            #       
            #       # Fallback to current time if API didn't work
            #       if (is.null(data_timestamp)) {
            #         data_timestamp <- Sys.time()
            #       }
            #       
            #       data_source <- "GitHub (public)"
            #       message("✅ Successfully loaded data from public GitHub repository")
            #       
            #     }, error = function(e) {
            #       message("⚠️ GitHub load failed, trying local file...")
            #       message("Error details: ", conditionMessage(e))
            #       
            #       # Fallback to local file
            #       local_path <- if (basename(getwd()) == "documents") {
            #         "../data/TrappingData.csv"
            #       } else {
            #         "data/TrappingData.csv"
            #       }
            #       
            #       if (!file.exists(local_path)) {
            #         stop("Neither GitHub data nor local TrappingData.csv found.")
            #       }
            #       
            #       fins_data <<- read_csv(local_path, show_col_types = FALSE)
            #       file_info <- file.info(local_path)
            #       data_timestamp <<- file_info$mtime
            #       data_source <<- "Local file"
            #       message("📁 Using local fallback data")
            #     })
            #     
            #   } else {
            #     # Local file mode
            #     local_path <- if (basename(getwd()) == "documents") {
            #       "../data/TrappingData.csv"
            #     } else {
            #       "data/TrappingData.csv"
            #     }
            #     
            #     if (!file.exists(local_path)) {
            #       stop("TrappingData.csv not found locally.")
            #     }
            #     
            #     fins_data <- read_csv(local_path, show_col_types = FALSE)
            #     file_info <- file.info(local_path)
            #     data_timestamp <- file_info$mtime
            #     data_source <- "Local file"
            #     message("📁 Using local data file")
            #   }
            #   
            #   # Clean weir data (existing logic)
            #   AdultWeirData_clean <- clean_weirData(fins_data) |>
            #     mutate(
            #       MonthDay = format(as.Date(trapped_date), "%m/%d"),
            #       count = as.double(count)
            #     )
            #   
            #   # Apply year filter if specified
            #   if (!is.null(trap.year)) {
            #     grsme_df <- AdultWeirData_clean |> filter(trap_year == !!trap.year)
            #   } else {
            #     grsme_df <- AdultWeirData_clean
            #   }
            #   
            #   # Return enhanced data with timestamp and source info
            #   list(
            #     AdultWeirData_clean = AdultWeirData_clean,
            #     grsme_df = grsme_df,
            #     data_timestamp = data_timestamp,
            #     data_source = data_source
            #   )
            # }

# # ---- Make Trap Date ----

make_trap_date <- function(month_day, year) {
  # Improved error handling for date parsing
  result <- suppressWarnings(ymd(paste(year, month_day, sep = "-")))

  # Handle cases where date parsing fails
  if (any(is.na(result))) {
    # Try parsing with different separator
    result <- suppressWarnings(ymd(paste(year, gsub("/", "-", month_day), sep = "-")))
  }

  return(result)
}

    # make_trap_date <- function(month_day, year) {
    #   ymd(paste(year, month_day, sep = "-"))
    # }

#---- Extract Broodstock Summary Numbers (Updated with Jacks) ----
extract_broodstock_summary <- function(broodstock_data) {
  
  # Check if data exists and has a "Total" row
  if (is.null(broodstock_data) || nrow(broodstock_data) == 0) {
    return(list(
      n_brood_sum = 0,
      h_brood_sum = 0,
      hj_brood_sum = 0,
      total_brood_sum = 0
    ))
  }
  
  # Find the "Total" row
  total_row <- broodstock_data |>
    filter(str_detect(`Week Start`, "Total"))
  
  if (nrow(total_row) == 0) {
    return(list(
      n_brood_sum = 0,
      h_brood_sum = 0,
      hj_brood_sum = 0,
      total_brood_sum = 0
    ))
  }
  
  # Extract numbers from the parentheses in each column
  # Format is typically "captures (broodstock)"
  
  # Natural Chinook - extract number in parentheses
  nat_text <- total_row$`Natural Chinook`[1]
  n_brood_sum <- as.numeric(str_extract(nat_text, "(?<=\\()\\d+(?=\\))"))
  if (is.na(n_brood_sum)) n_brood_sum <- 0
  
  # Hatchery Chinook - extract number in parentheses  
  hat_text <- total_row$`Hatchery Chinook`[1]
  h_brood_sum <- as.numeric(str_extract(hat_text, "(?<=\\()\\d+(?=\\))"))
  if (is.na(h_brood_sum)) h_brood_sum <- 0
  
  # For jacks, we need to get them from the original data since sumGRSMEbrood 
  # only shows adults in the summary table. We'll need to calculate separately.
  # This requires access to the original data, so we'll modify the function signature
  
  # For now, set to 0 - we'll handle this in the updated calculate_dispositions
  hj_brood_sum <- 0
  
  # Calculate total (adults only for now)
  total_brood_sum <- n_brood_sum + h_brood_sum + hj_brood_sum
  
  return(list(
    n_brood_sum = n_brood_sum,
    h_brood_sum = h_brood_sum,
    hj_brood_sum = hj_brood_sum,
    total_brood_sum = total_brood_sum
  ))
}

#---- jack broodstock counts ----
extract_jack_broodstock <- function(data, trap_year) {
  
  # Filter for hatchery jacks collected for broodstock
  jack_brood <- data |>
    filter(
      trap_year == !!trap_year,
      species == "Chinook",
      origin == "Hatchery",
      age_designation %in% c('Jack/Jill', 'Mini-Jack'),
      moved_to == "Lookingglass Fish Hatchery Inbox"
    ) |>
    summarise(hj_brood_sum = sum(count, na.rm = TRUE)) |>
    pull(hj_brood_sum)
  
  # Return 0 if no data
  if (length(jack_brood) == 0 || is.na(jack_brood)) {
    return(0)
  }
  
  return(jack_brood)
}

#---- Calculate Adult Captures from Disposition Tables (>630mm, Consistent) ----
calculate_adult_captures_from_disposition <- function(data, trap_year) {
  
  # Generate the same disposition tables used elsewhere
  h_df <- sumGRSMEdisp(data = data, origin_ = "Hatchery", trap.year = trap_year)
  n_df <- sumGRSMEdisp(data = data, origin_ = "Natural", trap.year = trap_year)
  
  # Extract the "Total [>630]" column from the "Total" row (excluding recaps - numbers in parentheses)
  h_total_row <- h_df[nrow(h_df), "Total [>630]"]
  n_total_row <- n_df[nrow(n_df), "Total [>630]"]
  
  # Extract numbers in parentheses (these exclude recaptures)
  h_adults <- as.numeric(str_extract(h_total_row, "(?<=\\()\\d+(?=\\))"))
  n_adults <- as.numeric(str_extract(n_total_row, "(?<=\\()\\d+(?=\\))"))
  
  # Handle cases where extraction fails
  if (is.na(h_adults)) h_adults <- 0
  if (is.na(n_adults)) n_adults <- 0
  
  # Calculate total
  total_adults <- h_adults + n_adults
  
  return(list(
    n_adults = n_adults,
    h_adults = h_adults,
    total_adults = total_adults
  ))
}

# UPDATE the calculate_dispositions() function call:
#---- Calculate Dispositions ----
calculate_dispositions <- function(data, trap_year) {
  # Summary tables
  h_df <- sumGRSMEdisp(data = data, origin_ = "Hatchery", trap.year = trap_year)
  n_df <- sumGRSMEdisp(data = data, origin_ = "Natural", trap.year = trap_year)
  
  # Extract counts for upstream composition
  hat_up <- as.numeric(stringr::str_extract(h_df[[1, 5]], "^\\d+"))
  nat_up <- as.numeric(stringr::str_extract(n_df[[1, 5]], "^\\d+"))
  h_upstream_calc <- round((hat_up / (hat_up + nat_up)) * 100, 0)
  
  # Extract counts for brood composition  
  hat_bs <- as.numeric(stringr::str_extract(h_df[[2, 5]], "^\\d+"))
  nat_bs <- as.numeric(stringr::str_extract(n_df[[2, 5]], "^\\d+"))
  n_brood_calc <- round((nat_bs / (hat_bs + nat_bs)) * 100, 0)
  
  # Generate broodstock data and extract summary
  broodstock_data <- sumGRSMEbrood(data = data, trap.year = trap_year)
  broodstock_summary <- extract_broodstock_summary(broodstock_data)
  
  # Get jack broodstock count separately
  hj_brood_sum <- extract_jack_broodstock(data, trap_year)
  
  # Calculate total broodstock including jacks
  total_brood_sum <- broodstock_summary$n_brood_sum + broodstock_summary$h_brood_sum + hj_brood_sum
  
  # UPDATED: Calculate total adult captures using SAME logic as disposition tables
  adult_captures <- calculate_adult_captures_from_disposition(data, trap_year)
  
  list(
    h_df = h_df,
    n_df = n_df,
    h_upstream_calc = h_upstream_calc,
    n_brood_calc = n_brood_calc,
    broodstock_data = broodstock_data,
    n_brood_sum = broodstock_summary$n_brood_sum,
    h_brood_sum = broodstock_summary$h_brood_sum,
    hj_brood_sum = hj_brood_sum,
    total_brood_sum = total_brood_sum,
    # Use disposition table logic for consistency
    n_adults = adult_captures$n_adults,
    h_adults = adult_captures$h_adults,
    total_adults = adult_captures$total_adults
  )
}

# ---- Plot Data Prep Function Prepare Mega DF ----

prepare_megadf <- function(trap.year, grsme_df, weir_data_clean) {
 
  # ---- Flow Data: Current Year ----
  start_date <- paste0(trap.year, "-05-15") #changed trap_year to trap.year
  end_date <- paste0(trap.year, "-09-30") #changed trap_year to trap.year
  
  req_url <- paste0(
    "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx?station_nbr=13330000",
    "&start_date=", start_date, "%2012:00:00%20AM",
    "&end_date=", end_date, "%2012:00:00%20AM",
    "&dataset=MDF&format=csv"
  )
  
  flow_df <- read.delim(req_url, sep = "\t") |>
    mutate(
      record_date = lubridate::mdy(record_date),
      MonthDay = format(record_date, "%m/%d"),
      facet = as.character(trap.year) #changed trap_year to trap.year
    ) |>
    select(MonthDay, MeanDailyFlow = mean_daily_flow_cfs, facet)
  
  # ---- Flow Data: Historic (5-year average) ----
  start_date_h <- paste0(trap.year - 5, "-05-15") #changed trap_year to trap.year
  end_date_h <- paste0(trap.year - 1, "-09-21") #changed trap_year to trap.year
  
  req_url2 <- paste0(
    "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx?station_nbr=13330000",
    "&start_date=", start_date_h, "%2012:00:00%20AM",
    "&end_date=", end_date_h, "%2012:00:00%20AM",
    "&dataset=MDF&format=csv"
  )

  
  flow_df_h <- read.delim(req_url2, sep = "\t") |>
    mutate(
      record_date = mdy(record_date),
      legend = paste(Sys.Date() - 1, "Discharge"),
      MonthDay = format(as.Date(record_date), "%m/%d")
    ) |>
    group_by(MonthDay) |>
    summarise(MeanDailyFlow = mean(mean_daily_flow_cfs, na.rm = TRUE), .groups = "drop") |>
    mutate(
      facet = paste0(trap.year - 5, "-", trap.year - 1, " Average")
    )
  
  # ---- Combine Flow----
  
  flow_all <- bind_rows(flow_df, flow_df_h) |>
    mutate(
      trapped_date = make_trap_date(MonthDay, trap.year)
    ) |>
    # Filter out rows where date parsing failed
    filter(!is.na(trapped_date)) |>
    filter(
      between(
        trapped_date,
        ymd(paste0(trap.year, "-05-15")),
        ymd(paste0(trap.year, "-09-21"))
      )
    )
  
  # flow_all <- bind_rows(flow_df, flow_df_h) |>
  #   mutate(
  #     trapped_date = make_trap_date(MonthDay, trap.year)
  #   ) |>
  #   filter( # This filter is klling this section when imported from helpers.
  #     between(
  #       trapped_date,
  #       ymd(paste0(trap.year, "-05-15")),
  #       ymd(paste0(trap.year, "-09-21"))
  #     )
  #   )

  
  lrw_catch <- grsme_df |>
    filter(
      species == "Chinook",
      recap == FALSE,
      trap_year == trap.year,
      age_designation == "Adult"
    ) |>
    group_by(trapped_date, MonthDay, origin) |>
    summarise(Catch = sum(count, na.rm = TRUE), .groups = "drop") |>
    mutate(facet = as.character(trap.year))
  
  # ---- Catch: Historic Mean ----
  
  lrw_historic <- weir_data_clean |>
    filter(
      facility == "NPT GRSME Program",
      species == "Chinook",
      recap == FALSE,
      !trap_year %in% c(1997:(trap.year - 6), trap.year),
      age_designation == "Adult"
    ) |>
    group_by(MonthDay, origin) |>
    summarise(AllCatch = sum(count, na.rm = TRUE), .groups = "drop") |>
    mutate(
      Catch = AllCatch / 5,
      trapped_date = make_trap_date(MonthDay, trap.year),
      facet = paste0(trap.year - 5, "-", trap.year - 1, " Average")
    )
  
  # Combine Catch
  lrw_all <- bind_rows(lrw_catch, lrw_historic)
  
  # Merge with Flow
  lrw_megadf <- full_join(
    lrw_all, flow_all, 
    by = c("trapped_date", "facet", "MonthDay"))
  
  # Order facet
  lrw_megadf$facet <- factor(
    lrw_megadf$facet,
    levels = c(
      as.character(trap.year), #changed from trap_year
      paste0(trap.year - 5, "-", trap.year - 1, " Average")) #changed trap_year to trap.year
  )
  
  list(lrw_megadf = lrw_megadf, 
       lrw_catch = lrw_catch)
}

# ---- Generate Plot ----
generate_lrw_megaplot <- function(megadf,
                                  lrw_catch,
                                  save_plot = FALSE,
                                  output_path = NULL) {
  
  # Determine correct output path based on working directory if not specified
  if (is.null(output_path)) {
    if (basename(getwd()) == "documents") {
      # Running from documents/ folder (Quarto)
      output_path <- "../LRW_megaplot.jpg"
    } else {
      # Running from root directory (Shiny app)
      output_path <- "LRW_megaplot.jpg"
    }
  }
  
  # ---- Compute Y-Axis Max ---
  plot_max_df <- lrw_catch |>
    group_by(trapped_date) |>
    summarise(Count = sum(Catch), .groups = "drop")
  
  plot_max_df2 <- megadf |>
    group_by(trapped_date) |>
    summarise(Count = sum(Catch, na.rm = TRUE), .groups = "drop")
  
  plot_max <- if (max(plot_max_df$Count, na.rm = TRUE) > max(plot_max_df2$Count, na.rm = TRUE)) {
    round(max(plot_max_df$Count, na.rm = TRUE) + 2, 0)
  } else {
    round(max(plot_max_df2$Count, na.rm = TRUE) + 2, 0)
  }
  
  # ---- Scale Factor for Dual Axis ---
  scale_factor <- round(
    max(megadf$Catch, na.rm = TRUE) / max(megadf$MeanDailyFlow, na.rm = TRUE),
    3
  )
  
  # ---- Create Plot ---
  p <- ggplot(megadf, aes(x = trapped_date)) +
    geom_bar(
      aes(y = Catch, fill = origin),
      color = "black",
      stat = "identity",
      position = "stack",
      width = 1
    ) +
    geom_line(
      aes(y = MeanDailyFlow * scale_factor, linetype = "Discharge"),
      color = "blue",
      linewidth = 1  # Fixed: Changed from size = 1 to linewidth = 1
    ) +
    scale_y_continuous(
      name = "Number of Chinook Adults",
      breaks = scales::breaks_pretty(7),
      limits = c(0, max(0, plot_max)),
      expand = c(0, 0),
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = expression(paste("Discharge (" * ft^3 * "/s)")),
        breaks = scales::breaks_pretty(7)
      )
    ) +
    scale_x_date(
      name = "",
      labels = scales::label_date("%m/%d"),
      breaks = scales::breaks_pretty(7),
      expand = c(0.001, 0.001)
    ) +
    scale_fill_manual(values = c("Natural" = "#FDE735FF", "Hatchery" = "#482677FF")) +
    facet_grid(rows = vars(facet)) +
    guides(color = "none") +  # Fixed: Changed from FALSE to "none"
    theme_bw() +
    theme(
      axis.text.x = element_text(hjust = 1, angle = 45, size = 14),
      axis.ticks.length.x = unit(0.15, "cm"),
      axis.title.y.left = element_text(size = 16),
      axis.text.y.left = element_text(size = 14),
      axis.title.y.right = element_text(color = "blue", size = 16),
      axis.text.y.right = element_text(color = "blue", size = 14),
      legend.position = "top",
      legend.title = element_blank(),
      legend.box.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(2, "lines")
    )
  
  # ---- Optionally Save Plot ---
  if (save_plot) {
    ggsave(
      filename = output_path,
      plot = p,
      device = "jpeg",
      width = 10,
      height = 7,
      units = "in"
    )
  }
  
  # ---- Return Plot Object ---
  return(p)
}

# ---- Prepare Captions ----

prepare_caption_table1 <- function(trap_year) {
  paste0(
    "Return year ", trap_year, " capture and disposition summary of Hatchery Chinook Salmon ",
    "(numbers in parentheses exclude recaptures)."
  )
}

prepare_caption_table2 <- function(trap_year) {
  paste0(
    "Return year ", trap_year, " capture and disposition summary of Natural Chinook Salmon ",
    "(numbers in parentheses exclude recaptures)."
  )
}

prepare_caption_table3 <- function(trap_year) {
  paste0(
    "Return year ", trap_year, " weekly summary of captured adult Chinook Salmon and Bull Trout, ",
    "excluding recaptures. Broodstock collection for Chinook Salmon is shown in parentheses. ",
    "*Asterisk indicates an incomplete week."
  )
}

prepare_caption_plot <- function(trap_year) {
  paste0(
    "Return year ", trap_year, " (top panel) and five-year average (bottom panel) of mean daily discharge ",
    "(cubic feet per second) and daily captures of hatchery- and natural-origin adult Chinook salmon ",
    "at the Lostine River Weir. Discharge recorded at USGS station 1333000 located upstream of the town of Lostine."
  )
}

# ---- Function to safely create flextable or show no-data message ----
safe_flextable <- function(data, trap_year, table_type = "hatchery") {
  
  # Check if data exists and has rows
  is_empty_data <- is.null(data) || nrow(data) == 0
  
  # If we have data, check if it's meaningful (not all zeros)
  if (!is_empty_data && nrow(data) > 0) {
    
    # For disposition tables, check if all numeric data is zeros
    if (table_type %in% c("hatchery", "natural")) {
      # Look for columns that should contain counts (columns 2-6 based on your table structure)
      count_cols <- data[, 2:ncol(data), drop = FALSE]
      
      # Extract just the numbers from each cell (in case they're formatted like "0 (0)")
      numeric_values <- c()
      for (i in 1:nrow(count_cols)) {
        for (j in 1:ncol(count_cols)) {
          cell_value <- as.character(count_cols[i, j])
          # Extract all numbers from the cell (handles "0 (0)" format)
          numbers <- as.numeric(unlist(regmatches(cell_value, gregexpr("\\d+", cell_value))))
          numeric_values <- c(numeric_values, numbers)
        }
      }
      
      # Remove NAs and check if all remaining values are zero
      numeric_values <- numeric_values[!is.na(numeric_values)]
      is_all_zeros <- length(numeric_values) > 0 && all(numeric_values == 0)
      is_empty_data <- is_all_zeros
    }
    
    # For broodstock table, check if it's just empty/placeholder data
    if (table_type == "broodstock") {
      # Check if all cells are empty, NA, or contain only zeros
      all_cells <- unlist(data)
      non_empty_cells <- all_cells[!is.na(all_cells) & all_cells != "" & all_cells != " "]
      
      # Extract numbers from non-empty cells
      numeric_values <- c()
      for (cell in non_empty_cells) {
        numbers <- as.numeric(unlist(regmatches(as.character(cell), gregexpr("\\d+", as.character(cell)))))
        numeric_values <- c(numeric_values, numbers[!is.na(numbers)])
      }
      
      is_empty_data <- length(numeric_values) == 0 || all(numeric_values == 0)
    }
  }
  
  if (is_empty_data) {
    # Create appropriate no-data message based on table type
    if (table_type == "hatchery") {
      message <- paste0("There is currently no data available for the capture of hatchery-origin Chinook for ", trap_year, ".")
    } else if (table_type == "natural") {
      message <- paste0("There is currently no data available for the capture of natural-origin Chinook for ", trap_year, ".")
    } else if (table_type == "broodstock") {
      message <- paste0("There is currently no broodstock collection data available for ", trap_year, ".")
    } else {
      message <- paste0("There is currently no data available for ", trap_year, ".")
    }
    
    # Return a simple flextable with the message
    no_data_df <- data.frame(Message = message)
    return(
      flextable(no_data_df) |>
        delete_part(part = "header") |>
        align(align = "center", part = "all") |>
        fontsize(size = 11, part = "all") |>
        italic(part = "all") |>
        set_table_properties(layout = "autofit", width = 0.95)
    )
  } else {
    # Data exists, create normal flextable
    if (table_type == "hatchery" || table_type == "natural") {
      return(
        flextable(
          data,
          cwidth = c(1.3, 0.7, 0.7, 0.7, 1.2, 1.2)
        ) |>
          align(j = 2:6, align = "right", part = "all") |>
          hline(i = nrow(data) - 1) |>
          set_table_properties(layout = "autofit", width = 0.95)
      )
    } else if (table_type == "broodstock") {
      return(
        flextable(
          data,
          cwidth = c(1, 1.5, 1.5, 1.2)
        ) |>
          align(j = 2:4, align = "right", part = "all") |>
          hline(i = nrow(data) - 1) |>
          set_table_properties(layout = "autofit", width = 0.95)
      )
    }
  }
}
