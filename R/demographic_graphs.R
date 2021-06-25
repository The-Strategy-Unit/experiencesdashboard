compare_demographics <- function(pass_data, variable){
  
  questions <- purrr::map(paste0("question_", 1 : 6), ~ get_golem_config(.x))
  
  questions <- unlist(questions[!vapply(questions, is.null, TRUE)])
  
  pass_data %>% 
    dplyr::filter(!is.na(.data[[variable]])) %>% 
    dplyr::group_by(.data[[variable]]) %>% 
    dplyr::summarise(across(questions, ~ mean(.x, na.rm = TRUE)),
                     n = dplyr::n()) %>% 
    dplyr::filter(n > 10) %>% 
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(. * 20, 1))) %>% 
    dplyr::select(-n) %>% 
    tidyr::pivot_longer(- .data[[variable]]) %>% 
    ggplot2::ggplot(ggplot2::aes(x = .data[[variable]], y = value, 
                                 group = name, fill = name)) + 
    ggplot2::geom_col(position = "dodge") + nottshcMethods::theme_nottshc() +
    ggplot2::ylab("%") + ggplot2::ylim(0, 100) + ggplot2::coord_flip()
}

demographic_distribution <- function(pass_data, variable){
  
  pass_data %>% 
    dplyr::count(.data[[variable]]) %>% 
    dplyr::mutate(across(all_of(variable), ~ dplyr::coalesce(
      ., "Unknown"))) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[variable]], y = n)) + 
    ggplot2::geom_col() + 
    nottshcMethods::theme_nottshc() + ggplot2::coord_flip()
}
