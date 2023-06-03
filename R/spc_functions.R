#' Get data ready for SPC plotting
#'
#' @param data dataframe containing "date" column and an FFT column
#' @param variable string, indicating the name of the FFT column
#' @param chunks either "monthly", which divides the data in months
#' or the number of chunks to divide the data into (each chunk will have the
#' same number of rows)
#' @param return dataframe suitable for plotting with NHSRplotthedots::ptd_spc
#'
#' @export
split_data_spc <- function(data, variable = "fft", chunks) {
  if (chunks == "monthly") {
    return(
      data %>%
        dplyr::filter(.data[[variable]] %in% 1:5) %>%
        dplyr::mutate(date = as.Date(cut(date, "month"))) %>%
        dplyr::mutate(fft = .data[[variable]] * 20) %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(fft = mean(fft, na.rm = TRUE))
    )
  } else {
    return(
      data %>%
        dplyr::filter(.data[[variable]] %in% 1:5) %>%
        dplyr::mutate(group = ceiling(dplyr::cur_group_rows() / nrow(.) * chunks)) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(date = min(date)) %>%
        dplyr::mutate(fft = .data[[variable]] * 20) %>%
        dplyr::filter(dplyr::n() > 2) %>%
        dplyr::group_by(group, date) %>%
        dplyr::summarise(fft = mean(fft, na.rm = TRUE))
    )
  }
}

#' Plot data
#'
#' @param data dataframe, that you probably made with the split_data_spc function
#' @param return SPC plot
#'
#' @export
plot_fft_spc <- function(data) {
  data %>%
    NHSRplotthedots::ptd_spc(
      value_field = fft,
      date_field = date,
      screen_outliers = F
    ) %>%
    plot(
      y_axis_label = "% FFT score",
      main_title = "SPC of Monthly FFT score",
      x_axis_breaks = "4 months",
      x_axis_date_format = "%b %y",
      icons_position = "none"
    ) +
    add_theme_nhs() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 11),
    )
}
