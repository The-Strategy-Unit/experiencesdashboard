compare_demographics <- function(pass_data, variable) {
  questions <- purrr::map(paste0("question_", 1:6), ~ get_golem_config(.x))

  questions <- unlist(questions[!vapply(questions, is.null, TRUE)])

  p <- pass_data %>%
    dplyr::filter(!is.na(.data[[variable]])) %>%
    dplyr::group_by(.data[[variable]]) %>%
    dplyr::summarise(across(questions, ~ mean(.x, na.rm = TRUE)),
      n = dplyr::n()
    ) %>%
    dplyr::filter(n > 10) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(. * 20, 1))) %>%
    dplyr::select(-n) %>%
    tidyr::pivot_longer(-.data[[variable]]) %>%
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
      legend.position = "none") 

  p %>%
    plotly::ggplotly(tooltip = c(variable, "value")) %>%
    plotly::config(displayModeBar = FALSE)
}

#' Draw the distribution of demographics in the sample
#' @param pass_data A dataframe, filtered to unique individuals
#' @param variable String. Name of variable - e.g. "age", "gender", "ethnicity"
#'
#' @return a ggplot2 graph
#' @export

demographic_distribution <- function(pass_data, variable) {
  pass_data %>%
    dplyr::count(.data[[variable]]) %>%
    dplyr::mutate(across(
      all_of(variable),
      ~ dplyr::coalesce(., "Unknown")
    )) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[variable]], y = n)) +
    ggplot2::geom_col(fill = "#005EB8") +
    add_theme_nhs() +
    ggplot2::coord_flip() +
    ggplot2::theme(
      legend.position = "none",
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 11),
      )
}
