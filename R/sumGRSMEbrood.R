#' @title sumGRSMEbrood
#'
#' @description takes prepared FINS Data and generates summaries for weir report.
#'
#' @param data FINS data filtered for desired year (GRSME_df)
#' @author Tyler Stright
#'
#' @examples 
#'
#' @import tidyverse, lubridate, ggplot2
#' @export
#' @return NULL


sumGRSMEbrood <- function(data, trap.year) {
  
  t3_df <- data %>%
    filter(trap_year == trap.year) %>%
    mutate(EpiWeek = epiweek(trapped_date),
           FloorDate = floor_date(trapped_date, unit = 'week'),
           Week = format(FloorDate, "%b %d"))
  
  # Check if we have any data after filtering
  if (nrow(t3_df) == 0) {
    # Return empty table with proper structure
    empty_table <- data.frame(
      `Week Start` = character(0),
      `Natural Chinook` = character(0),
      `Hatchery Chinook` = character(0),
      `Bull Trout` = numeric(0),
      stringsAsFactors = FALSE
    )
    return(empty_table)
  }
  
  # Chinook dates for table.
  ch_df <- t3_df %>%
    filter(species == 'Chinook')
  
  # Check if we have Chinook data
  if (nrow(ch_df) == 0) {
    # Return empty table if no Chinook data
    empty_table <- data.frame(
      `Week Start` = character(0),
      `Natural Chinook` = character(0),
      `Hatchery Chinook` = character(0),
      `Bull Trout` = numeric(0),
      stringsAsFactors = FALSE
    )
    return(empty_table)
  }
  
  w1_ch <- min(ch_df$EpiWeek)  # first week of trapping
  wf_ch <- max(ch_df$EpiWeek)  # 'last' week of trapping (most recent)
  
  w1_ch_date <- ch_df %>%   # Date of first catch
    ungroup() %>%
    filter(EpiWeek == w1_ch) %>%
    distinct(Week) %>%
    pull(Week)
  
  # Create Groups by Week
  t3_tmp <- t3_df %>%
    ungroup() %>%
    mutate(`Week Start` = case_when(
      EpiWeek < w1_ch ~ paste0('< ', w1_ch_date),
      EpiWeek >= w1_ch & EpiWeek != wf_ch ~ Week,
      EpiWeek == wf_ch ~ paste0(Week, '*')),
      EpiWeek = case_when(
        grepl(pattern = '<', `Week Start`) ~ 1,
        !grepl(pattern = '<', `Week Start`) ~ EpiWeek
      ))
  
  # Tally Broodstock
  broodstock_df <- t3_tmp %>%
    filter(species == 'Chinook',
           age_designation == 'Adult',
           moved_to == "Lookingglass Fish Hatchery Inbox") %>%
    group_by(origin, EpiWeek, `Week Start`) %>%
    summarize(Brood = sum(count), .groups = "drop") %>%
    mutate(Cohort = case_when(
      origin == 'Hatchery' ~ 'H Chinook Brood',
      origin == 'Natural' ~ 'N Chinook Brood'
    )) %>% ungroup()
  
  b_hat <- broodstock_df %>% filter(origin == 'Hatchery') %>%
    spread(key= Cohort, value = Brood, fill = 0) %>%
    select(-origin)
  b_nat <- broodstock_df %>% filter(origin == 'Natural') %>%
    spread(key= Cohort, value = Brood, fill = 0) %>%
    select(-origin) 
  

  # Tally Captures - FIXED to match sumGRSMEdisp logic exactly
  captures_df <- t3_tmp %>%
    filter(species %in% c('Chinook', 'Bull Trout'),
           recap == FALSE) %>%  # Use logical comparison
    # Apply the same filtering logic as sumGRSMEdisp for Chinook
    filter(
      if_else(species == 'Chinook',
              # For Chinook: match sumGRSMEdisp exactly
              age_designation == 'Adult' &
                origin %in% c("Hatchery", "Natural") &
                sex %in% c("Male", "Female"),
              # For Bull Trout: keep original logic
              age_designation %in% c(NA, 'Adult')
      )
    ) %>%
    group_by(species, origin, EpiWeek, `Week Start`) %>%
    summarize(Captured = sum(count), .groups = "drop") %>%
    mutate(Cohort = case_when(
      origin == 'Hatchery' & species == 'Chinook' ~ 'H Chinook Captures',
      origin == 'Natural' & species == 'Chinook' ~ 'N Chinook Captures',
      species == 'Bull Trout' ~ 'Bull Trout'
    )) %>% ungroup()
  
  c_hat <- captures_df %>%
    filter(Cohort == 'H Chinook Captures') %>%
    spread(key=Cohort, value = Captured, fill = 0) %>%
    select(-species, -origin) 
  
  c_nat <- captures_df %>%
    filter(Cohort == 'N Chinook Captures') %>%
    spread(key=Cohort, value = Captured, fill = 0) %>%
    select(-species, -origin) 
  
  c_bt <- captures_df %>%
    filter(Cohort == 'Bull Trout') %>%
    spread(key=Cohort, value = Captured, fill = 0) %>%
    select(-species, -origin) 
  
  # Table
  table3_raw <- full_join(b_hat, b_nat, by = c('Week Start', 'EpiWeek')) %>%
    full_join(c_hat, by = c('Week Start', 'EpiWeek')) %>%
    full_join(c_nat, by = c('Week Start', 'EpiWeek')) %>%
    full_join(c_bt, by = c('Week Start', 'EpiWeek')) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    arrange(EpiWeek) %>% 
    select(-EpiWeek)
  
  # Check if table3_raw has the expected columns before trying to sum them
  expected_cols <- c("H Chinook Brood", "N Chinook Brood", "H Chinook Captures", "N Chinook Captures", "Bull Trout")
  
  # Add missing columns with 0 values
  for (col in expected_cols) {
    if (!col %in% names(table3_raw)) {
      table3_raw[[col]] <- 0
    }
  }
  
  # Ensure proper column order
  table3_raw <- table3_raw %>%
    select(`Week Start`, all_of(expected_cols))
  
  # Now safely calculate totals
  if (ncol(table3_raw) >= 6) {
    t3_totals <- apply(table3_raw[,c(2:6)], 2, sum)
  } else {
    # If we still don't have enough columns, create zero totals
    t3_totals <- setNames(rep(0, 5), expected_cols)
  }
  
  table3_final <- table3_raw %>%
    add_row(`Week Start` = 'Total', 
            `H Chinook Brood` = t3_totals[1], 
            `N Chinook Brood` = t3_totals[2], 
            `H Chinook Captures`= t3_totals[3], 
            `N Chinook Captures` = t3_totals[4], 
            `Bull Trout` = t3_totals[5]) %>%
    mutate(`Natural Chinook` = paste0(`N Chinook Captures`, ' (', `N Chinook Brood`, ')'),
           `Hatchery Chinook` = paste0(`H Chinook Captures`, ' (', `H Chinook Brood`, ')')) %>%
    select(`Week Start`, `Natural Chinook`, `Hatchery Chinook`, `Bull Trout`)
  
  return(table3_final)
}