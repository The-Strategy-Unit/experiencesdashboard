#' plot a plotly bar chart for the aggregated sentiment data
#'
#' @param data dataframe with the sentiment score column. typical data can be made using `multigroup_calculated_data()` 
#' @param x x-xis data 
#' @param y y-axis data
#' @param event_id string to match the source of the `event_data()`
#' @param plot_title string, plot title
#' @param xaxis_title string, a-xis label
#' @param yaxis_title string, y-axis label
#' @param barmode  string, one of ( "stack" | "group" | "overlay" | "relative" ) 
#' @param yaxis_type string, one of ( "-" | "linear" | "log" | "date" | "category" | "multicategory" )
#'
#' @return plotly object
#' @noRd
plot_sentiment_trend <- function(data, x, y, event_id = 'A', barmode = "stack", plot_title = "", 
                                 xaxis_title = "", yaxis_title = "", yaxis_type = "-", ...) {
  data %>%
    plotly::plot_ly(
      x = x,
      y = y,
      source = event_id,
      type = "bar",
      hovertemplate = ~ paste(
        "Date:", date,
        "<br>Sentiment score:", sentiment,
        "<br>%total: ", round(percent, 1), "%",
        "<br>No. comments:", n
      ),
      ...
    ) %>%
    plotly::config(
      modeBarButtons = list(list("toImage")),
      toImageButtonOptions = list(
        format = "png"
      )
    ) %>%
    plotly::layout(
      title = plot_title,
      xaxis = list(
        title = xaxis_title
      ),
      yaxis = list(
        title = yaxis_title,
        type = yaxis_type,
        ticksuffix = "%" # to add % and reformat the numbers then use tickformat = "%"
      ),
      barmode = barmode
    )
}

#' Match sentiment text to its code
#'
#' @param value list of numeric sentiment score. should be between 1 - 5
#' @noRd
#' @examples get_sentiment_text(c(1, 2, 3, 3, 5))
get_sentiment_text <- function(value) {
  dplyr::case_when(
    value == 1 ~ "Very Negative",
    value == 2 ~ "Negative",
    value == 3 ~ "Neutral",
    value == 4 ~ "Positive",
    value == 5 ~ "Very Positive",
    TRUE ~ NA
  )
}
