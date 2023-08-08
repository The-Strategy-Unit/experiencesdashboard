#' Compares the average scores between the groups in a demography feature
#'
#' @param pass_data dataframe
#' @param variable name of the demography column
#' @param score_column name of the score columns
#'
#' @return a plotly object
#' @noRd
compare_demographics <- function(pass_data, variable, score_column = list("fft")) {
  score_column <- unlist(score_column[!vapply(score_column, is.null, TRUE)])

  p <- pass_data %>%
    dplyr::filter(!is.na(.data[[variable]])) %>%
    dplyr::group_by(.data[[variable]]) %>%
    dplyr::summarise(across(all_of(score_column), ~ mean(.x, na.rm = TRUE)),
      n = dplyr::n()
    ) %>%
    dplyr::filter(n > 10) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(. * 20, 1))) %>%
    dplyr::select(-n) %>%
    tidyr::pivot_longer(-all_of(variable)) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[variable]], y = value,
      group = name, fill = name
    )) +
    ggplot2::geom_col(position = "dodge") +
    add_theme_nhs() +
    ggplot2::ylab("%") +
    ggplot2::ylim(0, 100) +
    ggplot2::coord_flip() +
    ggplot2::theme(
      legend.position = "none"
    )

  p %>%
    plotly::ggplotly(tooltip = c(variable, "value")) %>%
    plotly::config(
      displaylogo = FALSE,
      modeBarButtons = list(list("toImage")),
      toImageButtonOptions = list(
        format = "png"
      )
    )
}

#' Draw the distribution of demographics in the sample
#' @param pass_data A dataframe, filtered to unique individuals
#' @param variable String. Name of variable - e.g. "age", "gender", "ethnicity"
#' @param return_ggplot logical if the plot should be returned as a ggplot or plotly object.
#'
#' @return a ggplot2 graph
#' @export
demographic_distribution <- function(pass_data, variable, return_ggplot = FALSE) {
  p <- pass_data %>%
    dplyr::count(.data[[variable]]) %>%
    dplyr::mutate(across(
      all_of(variable),
      ~ dplyr::coalesce(., "Unknown")
    )) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[variable]], y = n)) +
    ggplot2::geom_col(fill = "#005EB8") +
    add_theme_nhs() +
    ggplot2::theme(
      legend.position = "none",
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 11),
    )

  if (return_ggplot) {
    return(
      p +
        ggplot2::geom_text(
          ggplot2::aes(label = n),
          position = ggplot2::position_dodge(width = 0.9),
          vjust = -0.1
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, vjust = 1.2, hjust=1)
        ) +
        ggplot2::ylab("Number of comments")
    )
  }

  p <- p +
    ggplot2::coord_flip() 
  
  p %>%
    plotly::ggplotly(tooltip = c(variable, "n")) %>%
    plotly::config(
      displaylogo = FALSE,
      modeBarButtons = list(list("toImage")),
      toImageButtonOptions = list(
        format = "png"
      )
    )
}
