#' get_complex_comments 
#'
#' @param data a dataframe containing comment_txt column and column assigned to multilabel_column`
#' @param multilabel_column the column holding the labels
#' @param long_words rule 1 for defining complex comments
#' @param many_labels rule 2 for defining complex comment
#' @param data 
#'
#' @description A fct function to get the rows of data with complex comments. complex
#' complex comment is defined as comments with labels more than `many_labels` and words more than `long_words`
#'
#' @return a dataframe 
#'
#' @noRd
get_complex_comments <- function(data, multilabel_column = 'labels', long_words = 50, many_labels = 5){
  
  data |> 
    dplyr::mutate(n_word = lengths(stringr::str_split(comment_txt, ' ')),
                  n_label = lengths(stringr::str_split(.data[[multilabel_column]], ','))) |> 
    dplyr::filter(n_word > long_words | n_label > many_labels) 
  
}