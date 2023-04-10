# Packages required to define the pipeline:
library(targets)
library(tarchetypes)
suppressPackageStartupMessages(library(dplyr))


# General options
options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

# Bayes stuff
suppressPackageStartupMessages(library(brms))
options(mc.cores = 4,
        brms.backend = "cmdstanr",
        brms.threads = 2)

set.seed(58214)  # From random.org


# Global target options
tar_option_set(
  packages = c("tidyverse"),  # Packages available to all targets
  format = "qs",  # Storage format
  workspace_on_error = TRUE  # Automatically create a debug workspace on errors
)


# here::here() returns an absolute path, which then gets stored in tar_meta and
# becomes computer-specific (i.e. /Users/andrew/Research/blah/thing.Rmd).
# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}


# Load all the scripts in the R/ folder that contain the functions to be used in
# the pipeline
tar_source()


# Pipeline ----------------------------------------------------------------
list(
  ## Raw data files ----
  tar_target(ongo_manual_file,
             here_rel("data", "manual_data", "ongo-manual-clean.csv"),
             format = "file"),
  
  ## Graphics ----
  tar_target(graphic_functions, lst(theme_ongo, set_annotation_fonts, clrs)),

  ## Process and clean data ----
  tar_target(chinafile_clean, load_clean_chinafile()),
  tar_target(province_name, province_cn_to_en()),
  tar_target(ongo,
             clean_ongo_data(ongo_manual_file, chinafile_clean, province_name)),
  tar_target(ongo_mapdata,
             clean_map_data(ongo)),
  
  ## Models ----
  tar_target(m_basic_zoib, f_basic_zoib(ongo)),
  tar_target(m_basic_ologit, f_basic_ologit(ongo)),
  
  tar_target(m_full_zoib, f_full_zoib(ongo)),
  tar_target(m_full_ordbeta, f_full_ordbeta(ongo)),
  tar_target(m_full_interaction_ordbeta, f_full_interaction_ordbeta(ongo)),
  
  ## Posterior predictions ----
  tar_target(preds_issue, f_preds_issue(m_full_ordbeta)),
  tar_target(preds_local, f_preds_local(m_full_ordbeta)),
  tar_target(preds_timing, f_preds_timing(m_full_ordbeta)),
  
  tar_target(epreds_issue, f_epreds_issue(m_full_ordbeta)),
  tar_target(epreds_local, f_epreds_local(m_full_ordbeta)),
  tar_target(epreds_timing, f_epreds_timing(m_full_ordbeta)),
  tar_target(epreds_timing_local, f_epreds_timing_local(m_full_ordbeta)),
  
  ## Analysis notebook ----
  tar_quarto(manuscript_nice, path = "manuscript", quiet = FALSE),
  tar_quarto(website, path = ".", quiet = FALSE),
  
  tar_target(deploy_script, here_rel("deploy.sh"), format = "file"),
  tar_target(deploy, {
    # Force a dependency
    website
    # Run the deploy script, but only on Andrew's computer, which has SSH access to the server
    if (Sys.getenv("UPLOAD_WEBSITES") == "TRUE") processx::run(paste0("./", deploy_script))
  })
)
