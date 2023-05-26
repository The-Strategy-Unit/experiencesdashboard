#' Make a trend data to use in visualization
#'
#' @param data a data with each super-category or sub-category per row
#' @param selected_super_category a list of super-categories to filter by before grouping by sub categories
#'
#' @noRd
#' @return a dataframe with count of unique comment ID in each group
make_trend_data <- function(data, selected_super_category = NULL) {
  if (is.null(selected_super_category)) {
    data %>%
      dplyr::mutate(date = as.Date(cut(date, "month"))) %>%
      dplyr::group_by(date, super_category) %>%
      dplyr::summarize(n_comments = length(unique(comment_id)))
  } else {
    data %>%
      dplyr::filter(super_category %in% selected_super_category) %>%
      dplyr::mutate(date = as.Date(cut(date, "month"))) %>%
      dplyr::group_by(date, category) %>%
      dplyr::summarize(n_comments = length(unique(comment_id)))
  }
}

#' Plot interactive trend plot
#'
#' @param data dataframe made using `make_trend_data()`
#' @param category_column column name of the super or sub-category e.g to plot the sub-categories use 'category'
#' @param yaxis label of the yaxis e.g 'Sub Category'
#' @param source a string to match the source value in `event_data()` to retrieve the event data corresponding to this specific plot
#' @param super_category a list of super-categories that was used to filter the trend data. This is require for hover text. Default is NULL
#'
#' @noRd
#' @return a plotly object
plot_trend <- function(data, category_column, source, super_category = NULL) {
  p <- data %>%
    plotly::plot_ly(
      x = ~date,
      y = ~ .data[[category_column]], #   category,
      size = ~n_comments,
      type = "scatter",
      mode = "markers",
      color = I("#005EB8"),
      symbol = I("square"),
      alpha = .8,
      showlegend = FALSE,
      source = source
    ) %>%
    plotly::config(
      displaylogo = FALSE,
      modeBarButtons = list(list("toImage")),
      toImageButtonOptions = list(
        format = "png"
      )
    )

  if (is.null(super_category)) {
    p %>%
      plotly::add_markers(
        hovertemplate = ~ paste(
          "Category:", .data[[category_column]],
          "<br>Date: ", date,
          "<br>Number of Comments:", n_comments
        )
      ) %>%
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = "High-level Categories")
      )
  } else {
    p %>%
      plotly::add_markers(
        showlegend = FALSE,
        hovertemplate = ~ paste(
          "Super-Category:", unique(super_category),
          "<br>Sub-Category:", .data[[category_column]],
          "<br>Date: ", date,
          "<br>Number of Comments:", n_comments
        )
      ) %>%
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Sub Categorie")
      )
  }
}
