# R/weekly_broodstock_progress.R
#
# Weekly broodstock collection goal vs. actual table.
# Displays directly below Table 3 (Broodstock Collection Summary).
#
# Data sources:
#   - Goals:   data/brood_schedule.csv   (static, pasted in once per season)
#   - Actuals: grsme_df / TrappingData.csv (via get_trap_data(), same source
#              sumGRSMEbrood() already uses)
#
# Jacks are intentionally excluded from this table (goal schedule has none).

library(dplyr)
library(tidyr)
library(lubridate)
library(htmltools)

# ---- Load the static weekly goal schedule ----
#
# Expects data/brood_schedule.csv with columns:
#   trap_year, week_label, week_start, nor_timing_pct,
#   female_no_goal, female_ho_goal, male_no_goal, male_ho_goal
#
# week_start is a day-month string (e.g. "8-Jun") with no year; the year comes
# from trap_year. Each week runs Monday (week_start) through the following
# Sunday.

load_brood_schedule <- function(trap.year, path = NULL) {

  if (is.null(path)) {
    path <- "data/brood_schedule.csv"
  }
  
  schedule <- read_csv(path, show_col_types = FALSE) |>
    filter(trap_year == !!trap.year)
  
  if (nrow(schedule) == 0) {
    warning("No brood_schedule.csv rows found for trap year ", trap.year, ".")
    return(schedule)
  }
  
  schedule <- schedule |>
    mutate(
      week_start = dmy(paste0(week_start, "-", trap_year)),
      week_end   = week_start + days(6),
      week_display = paste0(format(week_start, "%d-%b"), " to ", format(week_end, "%d-%b"))
    ) |>
    arrange(week_start)
  
  # Prepend a synthetic pre-season row to catch anything trapped before the
  # schedule's first defined week (e.g. an early season opening). Goals
  # default to 0 -- there's no planned collection target before the season
  # formally starts. week_start is left NA on purpose; it's never a "real"
  # calendar week and is handled as a special case downstream.
  pre_season <- tibble(
    trap_year        = trap.year,
    week_label       = min(schedule$week_label) - 1L,
    week_start       = as.Date(NA),
    nor_timing_pct   = NA_real_,
    female_no_goal   = 0, female_ho_goal = 0,
    male_no_goal     = 0, male_ho_goal   = 0,
    week_end         = min(schedule$week_start) - days(1),
    week_display     = paste0("Before ", format(min(schedule$week_start), "%d-%b"))
  )
  
  bind_rows(pre_season, schedule) |> arrange(week_label)
}

# ---- Summarize actual weekly broodstock collections from FINS data ----
#
# `data` should already be filtered to the year of interest (e.g. grsme_df
# from get_trap_data()). `schedule` supplies the week boundaries so actuals
# bin identically to the goal table (Monday-start weeks, matching
# week_start = 1 below -- sumGRSMEbrood() uses the lubridate default of
# Sunday-start weeks, so that logic is NOT reused here).

summarize_weekly_broodstock_actual <- function(data, trap.year, schedule) {
  
  empty_actuals <- tibble(
    week_label = integer(0),
    female_no_actual = integer(0), female_ho_actual = integer(0),
    male_no_actual = integer(0),   male_ho_actual = integer(0)
  )
  
  if (nrow(schedule) == 0) return(empty_actuals)
  
  brood_df <- data |>
    filter(
      trap_year == !!trap.year,
      species == "Chinook",
      moved_to == "Lookingglass Fish Hatchery Inbox"
    )
  
  if (nrow(brood_df) == 0) return(empty_actuals)
  
  # Flag (don't silently drop) broodstock records missing age_designation --
  # same failure mode as the June 2026 FINS fork-length parameter issue that
  # dropped fish from Table 3 / Figure 1. Surfacing this here instead of
  # letting it disappear into the Adult-only filter below.
  missing_age <- brood_df |> filter(is.na(age_designation))
  if (nrow(missing_age) > 0) {
    warning(
      nrow(missing_age), " broodstock record(s) for ", trap.year,
      " have a missing age_designation (check FINS fork-length parameters ",
      "for the season) and are excluded from the weekly progress table."
    )
  }
  
  adult_brood_df <- brood_df |>
    filter(
      age_designation == "Adult",
      origin %in% c("Natural", "Hatchery"),
      sex %in% c("Male", "Female")
    )
  
  if (nrow(adult_brood_df) == 0) return(empty_actuals)
  
  # Bin trapped_date into the schedule's Monday-start weeks. The schedule's
  # first row is a synthetic pre-season bucket (week_start = NA) covering
  # anything trapped before the first real week -- build breaks from the
  # real weeks only, with an early sentinel date standing in for that
  # pre-season row's lower bound so those dates land in week_idx == 1
  # (the pre-season row) instead of the invalid index 0.
  real_schedule <- schedule |> filter(!is.na(week_start))
  pre_season_label <- schedule$week_label[is.na(schedule$week_start)]
  
  week_breaks <- c(
    as.Date("1900-01-01"),
    real_schedule$week_start,
    max(real_schedule$week_start) + days(7)
  )
  week_labels_vec <- c(pre_season_label, real_schedule$week_label)
  
  week_idx <- findInterval(adult_brood_df$trapped_date, week_breaks, left.open = FALSE)
  week_idx[week_idx == 0 | week_idx > length(week_labels_vec)] <- NA
  
  adult_brood_df <- adult_brood_df |>
    mutate(week_label = week_labels_vec[week_idx])
  
  out_of_schedule <- sum(is.na(adult_brood_df$week_label))
  if (out_of_schedule > 0) {
    warning(
      out_of_schedule, " broodstock record(s) for ", trap.year,
      " were trapped after ", format(max(real_schedule$week_end), "%d-%b"),
      " (outside the brood_schedule.csv week range) and are excluded from ",
      "the weekly progress table."
    )
  }
  
  adult_brood_df |>
    filter(!is.na(week_label)) |>
    mutate(
      col = paste0(
        ifelse(sex == "Female", "female_", "male_"),
        ifelse(origin == "Natural", "no", "ho"),
        "_actual"
      )
    ) |>
    group_by(week_label, col) |>
    summarize(n = sum(count), .groups = "drop") |>
    pivot_wider(names_from = col, values_from = n, values_fill = 0) |>
    { \(df) {
      for (col in c("female_no_actual", "female_ho_actual",
                    "male_no_actual", "male_ho_actual")) {
        if (!col %in% names(df)) df[[col]] <- 0
      }
      df
    }
    }()
}

# ---- Build the combined goal + actual weekly table ----
#
# Returns a list with:
#   weekly - one row per schedule week, goal + actual columns, is_current_week flag
#   totals - season-to-date totals, computed (never stored) so they can't drift
#            out of sync with the weekly numbers

build_weekly_broodstock_progress <- function(data, trap.year, schedule_path = NULL) {
  
  schedule <- load_brood_schedule(trap.year, path = schedule_path)
  if (nrow(schedule) == 0) return(NULL)
  
  actual <- summarize_weekly_broodstock_actual(data, trap.year, schedule)
  
  weekly <- schedule |>
    left_join(actual, by = "week_label") |>
    mutate(across(
      c(female_no_actual, female_ho_actual, male_no_actual, male_ho_actual),
      ~ replace_na(.x, 0)
    )) |>
    mutate(is_current_week = !is.na(week_start) & Sys.Date() >= week_start & Sys.Date() <= week_end) |>
    mutate(is_future_week = !is.na(week_start) & week_start > Sys.Date()) |>
    arrange(week_label) |>
    # Running (season-to-date) totals, used for the "Season Progress" column.
    # These are cumulative THROUGH each row's week, not the season-final total.
    mutate(
      female_no_goal_cum   = cumsum(female_no_goal),
      female_no_actual_cum = cumsum(female_no_actual),
      female_ho_goal_cum   = cumsum(female_ho_goal),
      female_ho_actual_cum = cumsum(female_ho_actual),
      male_no_goal_cum      = cumsum(male_no_goal),
      male_no_actual_cum    = cumsum(male_no_actual),
      male_ho_goal_cum      = cumsum(male_ho_goal),
      male_ho_actual_cum    = cumsum(male_ho_actual)
    ) |>
    select(
      week_label, week_display, week_start, week_end, is_current_week, is_future_week,
      female_no_actual, female_no_goal, female_no_actual_cum, female_no_goal_cum,
      female_ho_actual, female_ho_goal, female_ho_actual_cum, female_ho_goal_cum,
      male_no_actual,   male_no_goal,   male_no_actual_cum,   male_no_goal_cum,
      male_ho_actual,   male_ho_goal,   male_ho_actual_cum,   male_ho_goal_cum
    )
  
  totals <- weekly |>
    summarize(
      week_display     = "Season Total",
      female_no_actual = sum(female_no_actual), female_no_goal = sum(female_no_goal),
      female_ho_actual = sum(female_ho_actual), female_ho_goal = sum(female_ho_goal),
      male_no_actual    = sum(male_no_actual),   male_no_goal   = sum(male_no_goal),
      male_ho_actual    = sum(male_ho_actual),   male_ho_goal   = sum(male_ho_goal)
    ) |>
    # For the totals row, "season progress" is just actual-of-goal for the
    # full season -- same numbers as actual/goal, duplicated into the _cum
    # fields so format_weekly_broodstock_display() can treat every row the same way.
    mutate(
      is_future_week = FALSE,
      female_no_actual_cum = female_no_actual, female_no_goal_cum = female_no_goal,
      female_ho_actual_cum = female_ho_actual, female_ho_goal_cum = female_ho_goal,
      male_no_actual_cum    = male_no_actual,   male_no_goal_cum   = male_no_goal,
      male_ho_actual_cum    = male_ho_actual,   male_ho_goal_cum   = male_ho_goal
    )
  
  list(weekly = weekly, totals = totals)
}

# ---- Shared display formatting (used by both renderers below) ----
#
# Produces one "Week" column plus 4 groups (Natural Females, Hatchery Females,
# Natural Males, Hatchery Males), each with 3 sub-columns:
#   _goal     - this week's goal
#   _actual   - this week's collected count
#   _progress - season-to-date collected vs. season-to-date goal, e.g. "10 of 13"
# Column names are generic (group_metric) since the visible grouped headers
# are built separately in each renderer.

format_weekly_broodstock_display <- function(df) {
  df |>
    transmute(
      Week = week_display,
      
      nor_f_goal     = female_no_goal,
      nor_f_actual   = female_no_actual,
      nor_f_progress = if_else(is_future_week, "-",
                               paste0(female_no_actual_cum, " of ", female_no_goal_cum)),
      
      hor_f_goal     = female_ho_goal,
      hor_f_actual   = female_ho_actual,
      hor_f_progress = if_else(is_future_week, "-",
                               paste0(female_ho_actual_cum, " of ", female_ho_goal_cum)),
      
      nor_m_goal     = male_no_goal,
      nor_m_actual   = male_no_actual,
      nor_m_progress = if_else(is_future_week, "-",
                               paste0(male_no_actual_cum, " of ", male_no_goal_cum)),
      
      hor_m_goal     = male_ho_goal,
      hor_m_actual   = male_ho_actual,
      hor_m_progress = if_else(is_future_week, "-",
                               paste0(male_ho_actual_cum, " of ", male_ho_goal_cum))
    )
}

# ---- Shiny renderer (DT) ----
#
# DT doesn't support grouped headers natively -- build a custom two-row
# <thead> via a container (see ?DT::datatable "container" argument).

render_weekly_broodstock_dt <- function(built) {
  
  if (is.null(built)) {
    return(DT::datatable(
      data.frame(Message = "No weekly broodstock schedule available."),
      options = list(dom = 't'), rownames = FALSE
    ))
  }
  
  display <- format_weekly_broodstock_display(built$weekly)
  total_row <- format_weekly_broodstock_display(
    built$totals |> mutate(week_display = "Season Total")
  )
  display_full <- bind_rows(display, total_row)
  
  current_week_label <- display$Week[built$weekly$is_current_week]
  
  sub_headers <- rep(c("Weekly Goal", "Collected", "Season Progress"), 4)
  
  # Vertical separators between origin/sex groups -- same boundaries as the
  # flextable version's vline(j = c(1, 4, 7, 10)): a left border on the first
  # column of each new group (data columns 2, 5, 8, 11).
  group_border <- "border-left: 2px solid #999999;"
  
  header_sketch <- htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(rowspan = 2, style = "text-align: center; vertical-align: middle;", "Week"),
        th(colspan = 3, style = paste("text-align: center;", group_border), "Natural Females"),
        th(colspan = 3, style = paste("text-align: center;", group_border), "Hatchery Females"),
        th(colspan = 3, style = paste("text-align: center;", group_border), "Natural Males"),
        th(colspan = 3, style = paste("text-align: center;", group_border), "Hatchery Males")
      ),
      tr(
        lapply(seq_along(sub_headers), function(i) {
          is_group_start <- (i - 1) %% 3 == 0
          style <- if (is_group_start) paste("text-align: center;", group_border) else "text-align: center;"
          th(style = style, sub_headers[i])
        })
      )
    )
  ))
  
  dt <- DT::datatable(
    display_full,
    container = header_sketch,
    options = list(dom = 't', pageLength = -1, ordering = FALSE,
                   searching = FALSE, info = FALSE),
    rownames = FALSE
  ) |>
    DT::formatStyle(columns = 1:ncol(display_full), textAlign = 'center') |>
    DT::formatStyle(
      columns = c("nor_f_goal", "hor_f_goal", "nor_m_goal", "hor_m_goal"),
      `border-left` = "2px solid #999999"
    ) |>
    DT::formatStyle(
      "Week", target = "row",
      fontWeight = DT::styleEqual("Season Total", "bold")
    )
  
  if (length(current_week_label) > 0) {
    dt <- dt |> DT::formatStyle(
      "Week", target = "row",
      backgroundColor = DT::styleEqual(current_week_label, "#d4f7d4")
    )
  }
  
  dt
}