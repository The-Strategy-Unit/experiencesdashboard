#' find the previous quarter
#' @param date date, probably \code{\link{Sys.Date()}}
#' 
#' @return vector of two dates, one at the beginning and one at the end of the
#' quarter previous to the date
previous_quarter <- function(date){
  
  previous_quarter <- (lubridate::quarter(date)) - 1 %% 4
  previous_year <- lubridate::year(date)
  
  if(previous_quarter == 0){
    
    previous_quarter <- 4
    previous_year <- previous_year - 1
  }
  
  first_date <- lubridate::yq(paste0(previous_year, ": Q", previous_quarter))
  
  end_date <- lubridate::yq(paste0(lubridate::year(date), ": Q", 
                                   lubridate::quarter(date))) - 1
  
  return(c(first_date, end_date))
}
#' produce a summary of the comments from comment 1 OR 2 and paste the 
#' location after the comment 
#' @param data dataframe- the main data fed in to the module
#' @param comment_selection string. comment_1 or comment_2
#' 
#' @return string. A massive string with line returns suitable for inclusion
#' in RMarkdown report
#' @export
verbatim_summary <- function(data, comment_selection){
  
  data %>% 
    dplyr::filter(comment_type == comment_selection) %>% 
    dplyr::filter(!is.na(comment_txt) & !is.na(category)) %>% 
    dplyr::arrange(location_3, category) %>% 
    dplyr::mutate(comment_with_location = paste0(comment_txt, 
                                                 " (", location_3, "- ",
                                                 category, ")")) %>% 
    dplyr::pull(comment_with_location)
  
}