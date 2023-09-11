#' cross_tabulate
#'
#' @description Return a 2 x 2 cross tabs from a dataframe with rows and columns
#' given as parameters
#' @param data a dataframe
#' @param rows the variable for the rows, as a string
#' @param columns the variable for the columns, as a string
#' @param by_row Boolean, if to take the column names from the row or column variable
#'
#' @return dataframe
#' @noRd
cross_tabulate <- function(data, rows, columns, by_row = FALSE) {
  data |>
    dplyr::count(.data[[rows]], .data[[columns]]) |>
    tidyr::pivot_wider(
      names_from = ifelse(by_row, rows, columns),
      values_from = n, names_sort = TRUE
    )
}

get_crossway_data <- function(data, input) {
  selection <- input$crosstab_table_cell_clicked |> unlist()
  row_id <- selection["row"]
  column_id <- selection["col"]
  no_comments <- selection["value"]

  # return empty dataframe if the cell has no value or the row column is selected
  if (is.na(no_comments) | column_id == 0) {
    return(data.frame())
  }

  row_values <- data |>
    dplyr::pull(input$rows) |>
    unique() |>
    sort()

  column_values <- data |>
    dplyr::pull(input$columns) |>
    unique() |>
    sort()

  selected_row <- as.character(row_values[row_id])
  selected_column <- as.character(column_values[column_id])

  return_data <- data |>
    dplyr::filter(
      !!rlang::sym(input$rows) == selected_row,
      !!rlang::sym(input$columns) == selected_column
    )

  cat("Selected row:", selected_row, "\n")
  cat("Selected column:", selected_column, "\n")
  cat("Rows in cross way data is :", nrow(return_data), "\n")

  # the value in the cross tab should be the same as the number of rows in return_data
  stopifnot("The expected number of rows is not returned" = nrow(return_data) == no_comments)

  return(return_data)
}

# # tests
# cross_tabulate(tidy_filter_data |>
#                dplyr::mutate(fft = factor(fft)),
#                'category',
#                'fft',
# )
# 
# get_crossway_data(tidy_filter_data, list(crosstab_table_cell_clicked = list(row = 1, col = 1, value = 5159),
#                            rows = 'sentiment', columns = 'fft')) |> View()
