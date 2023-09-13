suppressPackageStartupMessages(library(tidybayes))
library(marginaleffects)

# Model definitions
f_full_ordbeta <- function(data) {
  library(ordbetareg)
  
  BAYES_SEED <- 860215  # From random.org
  
  model <- ordbetareg(
    bf(
      province_count ~ work_field_code1 + local_connect + years_since_law + (1 | province_code)
    ), 
    data = data,
    true_bounds = c(1, 32),
    seed = BAYES_SEED)
  
  return(model)
}

f_full_interaction_ordbeta <- function(data) {
  library(ordbetareg)
  
  BAYES_SEED <- 860215  # From random.org
  
  model <- ordbetareg(
    bf(
      province_count ~ work_field_code1 + local_connect * years_since_law + (1 | province_code)
    ), 
    data = data,
    true_bounds = c(1, 32),
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

f_preds_issue <- function(model, ndraws = 500) {
  tibble(draw = 1:ndraws) |> 
    mutate(preds = map(draw, ~{
      model |> 
        predicted_draws(datagrid(model = model, work_field_code1 = unique), 
                        ndraws = 1000)
    }))
}

f_preds_local <- function(model, ndraws = 100) {
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
        predicted_draws(datagrid(model = model, years_since_law = 0:5), 
                        ndraws = 1000, seed = 58214)
    }))
}

f_epreds_issue <- function(model, ndraws = 1000) {
  model |> 
    epred_draws(datagrid(model = model, work_field_code1 = unique), 
                ndraws = ndraws, seed = 58214) |> 
    ungroup()
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
