suppressPackageStartupMessages(library(tidybayes))
library(marginaleffects)

# Model definitions
f_full_ordbeta <- function(data) {
  library(ordbetareg)
  
  BAYES_SEED <- 860215  # From random.org
  
  model <- ordbetareg(
    bf(
      province_count ~ 
        issue_arts_and_culture + issue_education +
        issue_industry_association + issue_economy_and_trade + 
        issue_charity_and_humanitarian + issue_general + issue_health +
        issue_environment + issue_science_and_technology +
        local_connect + years_since_law + year_registered_cat
    ), 
    data = data,
    true_bounds = c(1, 32),
    coef_prior_mean = 0,
    coef_prior_SD = 2.5,
    phi_prior = 1/100,
    seed = BAYES_SEED)
  
  return(model)
}

f_full_zoib <- function(data) {
  BAYES_SEED <- 102819  # From random.org
  
  zoib_priors <- c(prior(student_t(1, 0, 3), class = Intercept),
    prior(student_t(1, 0, 3), class = b),
    prior(student_t(1, 0, 3), class = b, dpar = zoi),
    prior(student_t(1, 0, 3), class = b, dpar = coi))
  
  model <- brm(
    bf(
      # mu: mean of the 0-1 values; logit scale
      province_pct ~ issue_arts_and_culture + issue_education +
        issue_industry_association + issue_economy_and_trade + 
        issue_charity_and_humanitarian + issue_general + issue_health +
        issue_environment + issue_science_and_technology +
        local_connect + years_since_law + year_registered_cat,
      # zoi: zero-or-one-inflated part; logit scale
      zoi ~ issue_arts_and_culture + issue_education +
        issue_industry_association + issue_economy_and_trade + 
        issue_charity_and_humanitarian + issue_general + issue_health +
        issue_environment + issue_science_and_technology +
        local_connect + years_since_law + year_registered_cat,
      # coi: one-inflated part, conditional on the 0s; logit scale
      coi ~ issue_arts_and_culture + issue_education +
        issue_industry_association + issue_economy_and_trade + 
        issue_charity_and_humanitarian + issue_general + issue_health +
        issue_environment + issue_science_and_technology +
        local_connect + years_since_law + year_registered_cat), 
    data = data,
    family = zero_one_inflated_beta(), 
    prior = zoib_priors,
    seed = BAYES_SEED)
  
  return(model)
}

# f_lotsa_preds <- function(model, column_name, ndraws = 100) {
#   # This dynamically generates the grid, essentially building this command:
#   #   datagrid(model = model, local_connect = unique)
#   # but with any variable in place of "local_connect"
#   args <- list(model = model)
#   args[[column_name]] <- unique
#   grid <- exec(datagrid, !!!args)
#   # Or with do.call():
#   #   grid <- do.call(datagrid, args)
#   
#   tibble(draw = 1:ndraws) |> 
#     mutate(preds = map(draw, ~predicted_draws(model, grid, ndraws = 1000)))
# }

f_preds_issue <- function(model, lookup, ndraws = 500) {
  issue_matrix <- matrix(FALSE, nrow = nrow(lookup), ncol = nrow(lookup))
  diag(issue_matrix) <- TRUE
  colnames(issue_matrix) <- lookup$issue_area
  
  newdata <- datagrid(model = model) |> 
    select(-starts_with("issue")) |> 
    cross_join(as_tibble(issue_matrix))
  
  tibble(draw = 1:ndraws) |> 
    mutate(preds = map(draw, ~{
      model |> 
        predicted_draws(newdata, ndraws = 1000)
    })) |> 
    unnest(preds) |> 
    ungroup() |> 
    pivot_longer(cols = starts_with("issue_"), names_to = "issue_area") |> 
    filter(value == TRUE) |> 
    select(-value)
}

f_preds_local <- function(model, ndraws = 500) {
  tibble(draw = 1:ndraws) |> 
    mutate(preds = map(draw, ~{
      model |> 
        predicted_draws(datagrid(model = model, local_connect = unique), 
                        ndraws = 1000, seed = 58214)
    }))
}

f_preds_timing <- function(model, ndraws = 100) {
  tibble(draw = 1:ndraws) |> 
    mutate(preds = map(draw, ~{
      model |> 
        predicted_draws(datagrid(model = model, years_since_law = seq(0, 5, by = 0.1)), 
                        ndraws = 1000, seed = 58214)
    }))
}

f_epreds_issue <- function(model, lookup, ndraws = 1000) {
  issue_matrix <- matrix(FALSE, nrow = nrow(lookup), ncol = nrow(lookup))
  diag(issue_matrix) <- TRUE
  colnames(issue_matrix) <- lookup$issue_area
  
  newdata <- datagrid(model = model) |> 
    select(-starts_with("issue")) |> 
    cross_join(as_tibble(issue_matrix))
  
  model |> 
    epred_draws(newdata, ndraws = ndraws, seed = 58214) |> 
    ungroup() |> 
    pivot_longer(cols = starts_with("issue_"), names_to = "issue_area") |> 
    filter(value == TRUE) |> 
    select(-value)
}

f_epreds_local <- function(model, ndraws = 1000) {
  model |> 
    epred_draws(datagrid(model = model, local_connect = unique), 
                ndraws = ndraws, seed = 58214) |> 
    ungroup()
}

f_epreds_timing <- function(model, ndraws = 1000) {
  model |> 
    epred_draws(datagrid(model = model, years_since_law = 0:5), 
                ndraws = ndraws, seed = 58214) |> 
    ungroup()
}

f_epreds_timing_local <- function(model, ndraws = 1000) {
  model |> 
    epred_draws(datagrid(model = model, 
                         local_connect = unique, years_since_law = 0:5), 
                ndraws = ndraws, seed = 58214) |> 
    ungroup()
}
