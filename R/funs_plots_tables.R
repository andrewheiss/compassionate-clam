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
