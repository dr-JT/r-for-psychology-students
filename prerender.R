# code and functions to render before each document

table_theme <- function(x, digits = 3, title = NULL, note = NULL,
                        table_align = "left") {
  x |>
    tab_options(table.align = table_align,
                table.border.top.width = 0,
                table.border.bottom.width = 0,
                table_body.border.bottom.color = "black",
                heading.align = "center",
                heading.padding = 8,
                heading.border.bottom.color = "black",
                heading.title.font.size = 14,
                heading.title.font.weight = "bolder",
                column_labels.border.top.width = 0,
                column_labels.border.bottom.color = "black",
                column_labels.padding = 6,
                footnotes.font.size = 13,
                quarto.disable_processing = TRUE) |>
    opt_row_striping(row_striping = FALSE) |>
    tab_header(title = title) |>
    tab_footnote(note) |>
    fmt_number(use_seps = FALSE, decimals = digits)
}
