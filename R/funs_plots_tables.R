opts_int <- function(x, ...) {
  x |> 
    opt_interactive(use_compact_mode = TRUE, use_highlight = TRUE, ...)
}

opts_theme <- function(x) {
  x |> 
    opt_table_font(font = "IBM Plex Sans") |> 
    tab_options(column_labels.font.weight = "bold",
      row_group.font.weight = "bold")
}

gt_linebreak <- function(x) {
  if (fmt_out == "latex") {
    x %>% 
      str_replace_all("\\[", "{[}") %>% 
      str_replace_all("\\]", "{]}") %>% 
      kableExtra::linebreak(align = "c")
  } else {
    str_replace(x, "\\n", "<br>")
  }
}

fmt_markdown_latex <- function(x, columns) {
  if (fmt_out == "latex") {
    fmt_passthrough(x, columns = columns, escape = FALSE)
  } else {
    fmt_markdown(x, columns = columns)
  }
}

gt_pct <- function(x, page_width = 6) {
  if (knitr::pandoc_to("latex")) {
    paste0(page_width * (x / 100), "in")
  } else {
    gt::pct(x)
  }
}

build_modelsummary <- function(models) {
  suppressPackageStartupMessages(library(modelsummary))
  
  nice_coefs <- c(
    "b_Intercept" = "Intercept",
    "b_issue_arts_and_cultureTRUE" = "Issue [Arts and culture]",
    "b_issue_educationTRUE" = "Issue [Education]",
    "b_issue_industry_associationTRUE" = "Issue [Industry association]",
    "b_issue_economy_and_tradeTRUE" = "Issue [Economy and trade]",
    "b_issue_charity_and_humanitarianTRUE" = "Issue [Charity and humanitarian]",
    "b_issue_generalTRUE" = "Issue [General]",
    "b_issue_healthTRUE" = "Issue [Health]",
    "b_issue_environmentTRUE" = "Issue [Environment]",
    "b_issue_science_and_technologyTRUE" = "Issue [Science and technology]",
    "b_local_connectTRUE" = "Local connections",
    "b_years_since_law" = "Years since law",
    "b_year_registered_cat2018" = "Year registered [2018]",
    "b_year_registered_cat2019" = "Year registered [2019]",
    "b_year_registered_cat2020" = "Year registered [2020]",
    "b_year_registered_cat2021" = "Year registered [2021]",
    "b_local_connectTRUE:years_since_law" = "Local connections × years since law",
    "phi" = "φ"
  )
  
  msl <- modelsummary(
    models, 
    output = "modelsummary_list",
    statistic = "[{conf.low}, {conf.high}]",
    ci_method = "hdi",
    metrics = c("R2")
  )
  
  return(lst(msl, nice_coefs))
}


# Storing ggplot objects as rds files is BAD 
#   (https://github.com/hadley/ggplot2-book/issues/344)
#
# But in this project, the posterior predictions plots take a while to build and
# making them targets would speed things up. And storing plots as targets is
# recommended when working with {targets}---the basic walkthrough in the manual
# even shows how to create a plot as a target.
# 
# ggplot objects are strange beasts because they store a copy of the overall
# environment when serialized into rds files. When working with regular-sized
# datasets, this isn't really ever a problem. But when working with tidy
# data frames of MCMC chains with millions and millions of rows, this can create
# rds files that are hundreds or thousands of MBs, which is wild.
#
# This post goes into more details about how to fix it: 
# https://ropensci.org/blog/2022/12/06/save-ggplot2-targets/
#
# The easiest way to fix it is to just not store the plot and instead store the
# underlying data, since that takes the most computational time. Here, we build
# the plot-specific data frames so that the manuscript renders more quickly.
build_preds_issue_plot_data <- function(preds, lookup) {
  preds_issue_plot <- preds |> 
    left_join(lookup, by = join_by(issue_area)) |> 
    mutate(outcome = case_when(
      .prediction == 0 ~ "Only 1 province",
      .prediction > 0 & .prediction < 1 ~ "Between 1–32 provinces",
      .prediction == 1 ~ "All 32 provinces",
      .ptype = factor(
        levels = c("Only 1 province", "Between 1–32 provinces", "All 32 provinces"), 
        ordered = TRUE)
    )) |> 
    group_by(outcome, issue_area_nice, draw) |> 
    summarize(count = n()) |> 
    group_by(issue_area_nice, draw) |> 
    mutate(prop = count / sum(count))
  
  return(preds_issue_plot)
}

build_preds_local_plot_data <- function(preds) {
  preds_local_plot <- preds |> 
    unnest(preds) |> 
    mutate(local_connect = factor(local_connect, labels = c("No local connections", "Local connections"))) |> 
    mutate(outcome = case_when(
      .prediction == 0 ~ "Only 1 province",
      .prediction > 0 & .prediction < 1 ~ "Between 1–32 provinces",
      .prediction == 1 ~ "All 32 provinces",
      .ptype = factor(
        levels = c("Only 1 province", "Between 1–32 provinces", "All 32 provinces"), 
        ordered = TRUE)
    )) |> 
    group_by(outcome, local_connect, draw) |> 
    summarize(count = n()) |> 
    group_by(local_connect, draw) |> 
    mutate(prop = count / sum(count))
  
  return(preds_local_plot)
}

build_preds_timing_plot_data <- function(preds) {
  preds_timing_plot <- preds |> 
    unnest(preds) |> 
    mutate(outcome = case_when(
      .prediction == 0 ~ "Only 1 province",
      .prediction > 0 & .prediction < 1 ~ "Between 1–32 provinces",
      .prediction == 1 ~ "All 32 provinces",
      .ptype = factor(
        levels = c("Only 1 province", "Between 1–32 provinces", "All 32 provinces"), 
        ordered = TRUE)
    )) |> 
    mutate(years_since_law = 2017 + years_since_law) |> 
    group_by(outcome, years_since_law, draw) |>
    summarize(count = n()) |> 
    group_by(years_since_law) |> 
    mutate(prop = count / sum(count) * 100)
  
  return(preds_timing_plot)
}
