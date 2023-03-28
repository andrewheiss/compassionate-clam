# Packages required to define the pipeline:
library(targets)
library(tarchetypes)
suppressPackageStartupMessages(library(dplyr))


# General options
options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

set.seed(58214)  # From random.org


# Global target options
tar_option_set(
  packages = c("tibble"),  # Packages available to all targets
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
  
  ## Process and clean data ----
  tar_target(chinafile_clean, load_clean_chinafile()),
  tar_target(province_name, province_cn_to_en()),
  tar_target(ongo,
             clean_ongo_data(ongo_manual_file, chinafile_clean, province_name)),
  
  ## Analysis notebook ----
  tar_quarto(website, path = ".", quiet = FALSE)
)
