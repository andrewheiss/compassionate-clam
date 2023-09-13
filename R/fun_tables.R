opts_int <- function(x, ...) {
  x %>% 
    opt_interactive(use_compact_mode = TRUE, use_highlight = TRUE, ...)
}

opts_theme <- function(x) {
  x %>% 
    opt_table_font(font = "IBM Plex Sans") %>% 
    tab_options(column_labels.font.weight = "bold",
      row_group.font.weight = "bold")
}
