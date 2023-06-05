#' find the previous quarter
#' @param date date, probably \code{\link{Sys.Date()}}
#'
#' @return vector of two dates, one at the beginning and one at the end of the
#' quarter previous to the date
previous_quarter <- function(date) {
  previous_quarter <- (lubridate::quarter(date)) - 1 %% 4
  previous_year <- lubridate::year(date)

  if (previous_quarter == 0) {
    previous_quarter <- 4
    previous_year <- previous_year - 1
  }

  first_date <- lubridate::yq(paste0(previous_year, ": Q", previous_quarter))

  end_date <- lubridate::yq(paste0(
    lubridate::year(date), ": Q",
    lubridate::quarter(date)
  )) - 1

  return(c(first_date, end_date))
}

#' produce a list of comments concatenated with the location after the comment
#'
#' @param return_data dataframe - clean version of the data feed into the module
#' @param category_selection list of category to filter the data by
#'
#' @return Vector list of strings
#' @export
verbatim_summary <- function(return_data, category_selection) {
  return_data %>%
    dplyr::filter(category %in% category_selection) %>%
    dplyr::arrange(location_3) %>%
    dplyr::mutate(comment_with_location = paste0(
      comment_txt,
      " (", location_3, ")"
    )) %>%
    dplyr::pull(comment_with_location)
}

#' produce a summary of the comments from comment 1 OR 2 and paste the
#' location after the comment
#'
#' @param data dataframe - the main data fed into the module
#' @param comment_selection string. comment_1 or comment_2
#'
#' @return string. A massive string with line returns suitable for inclusion
#' in RMarkdown report
#' @export
verbatim_comments <- function(data, comment_selection) {
  comment_df <- data %>%
    dplyr::filter(comment_type == comment_selection) %>%
    dplyr::filter(!is.na(comment_txt) & !is.na(category)) %>%
    dplyr::filter(category != "Labelling not possible") %>%
    dplyr::filter(category != "")

  if (nrow(comment_df) == 0) {
    cat("No comment available. Please expand your selection  \n")
  } else {
    categories <- comment_df$category %>%
      unique() %>%
      sort()

    all_cat_comment <- verbatim_summary(comment_df, categories)

    if (length(all_cat_comment) > 1000) {
      cat("Error. Too many comments. There are ", length(all_cat_comment), " comments in this selection
          (maximum number of comments expected is 1000). Please reduce your selection  \n")

      cat("  \n")
    } else {
      for (cty in categories) {
        cat("#### Category:", cty, "  \n")

        cat_comment <- verbatim_summary(comment_df, cty)

        cat(
          paste0("<p>", cat_comment, "</p>", collapse = "")
        )

        cat("  \n")
      }
    }
  }
}
