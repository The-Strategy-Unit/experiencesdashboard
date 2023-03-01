#' overlap_heatmap_plot 
#'
#' @description A function to plot the heatmap of the overlapping matrix
#'
#' @param data 
#' @param legend
#' 
#' @noRd
overlap_heatmap_plot <- function(data, legend='Legend'){
  
  return(
    data %>%
      # filter(value > .7) %>%
      dplyr::filter(!is.na(value)) %>%
      ggplot2::ggplot(ggplot2::aes(x=item1, y=item2, fill=value)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = value), size = 5) +
      ggplot2::scale_fill_gradient2(low = "#999999", high = "#0072B2",
                           midpoint = mean(data$value[!is.na(data$value)]),
                           name=stringr::str_to_title(legend)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, 
                                                         hjust = 1, face = "bold", size = 12),
                     axis.text.y = ggplot2::element_text(face = "bold", size = 12),
                     plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::ggtitle(label = "Overlapping themes")
  )
  
}

# function to reorder the correlation matrix or similar matrix by using the values between variables as distance 
reorder_cormat <- function(cormat){
  
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  
  return(cormat[hc$order, hc$order])
}

# Function to change the multilabeled data to a tidy data

multi_to_single_label <- function(mt_data, column_name, n_labels = 10){
  
  mt_data %>% 
    tidyr::separate(column_name, into = paste('label_',1:n_labels),
                    sep=',') %>%
    dplyr::mutate(rn = dplyr::row_number()) %>% # create a row id
    tidyr::pivot_longer(dplyr::starts_with("label_")) %>%  
    dplyr::mutate(value = stringi::stri_trim_both (.data$value)) %>% 
    tidyr::drop_na()
  
}

# function to remove redundant information from correlation matrix (or similar matrix)
# by setting upper half of the matrix to NA
# and reshape it to a dataframe 

reshape_cormat <- function(wide_overlapping_theme){
  
  cormat = as.matrix(wide_overlapping_theme[, -1])
  rownames(cormat) <- wide_overlapping_theme %>% dplyr::pull(item2)
  cormat[is.na(cormat)] = 0
  
  cormat <- reorder_cormat(cormat) 
  
  cormat[upper.tri(cormat)] <- NA
  diag(cormat) <- NA
  
  return(reshape2::melt(cormat) %>% 
           dplyr::rename('item1'=Var1, 'item2'=Var2))
} 

make_overlap_theme <- function(tidy_multilabeled_data, group_type=c('count', 'correlation')){
  
  # check argument is valid and choose the correct logical predicate
  group_type <- match.arg(group_type)
  stopifnot("group type must be one of 'count', or 'correlation'" = length(group_type) == 1)
  
  if (group_type == 'count'){
    
    w_overlapping_theme <- tidy_multilabeled_data %>% 
      dplyr::inner_join(tidy_multilabeled_data, by = "rn") %>% 
      dplyr::count(value.x, value.y) %>% 
      dplyr::filter(value.x != value.y) %>% 
      dplyr::rename('item1' = value.x, 'item2' = value.y) %>% 
      dplyr::arrange(item1) %>%
      tidyr::pivot_wider(names_from = item1, values_from = n) %>%
      dplyr::arrange(item2)
    
  } else{ 
    
    w_overlapping_theme <- tidy_multilabeled_data %>%
      dplyr::count(value, comment_txt, sort = TRUE) %>%
      widyr::pairwise_cor(value, comment_txt, n, sort = TRUE) %>%
      dplyr::mutate(correlation = round(correlation, 2)) %>%
      dplyr::arrange(item1) %>%
      tidyr::pivot_wider(names_from = item1, values_from = correlation) %>%
      dplyr::arrange(item2)
    
  }
  
  return (reshape_cormat(w_overlapping_theme))
  
}



# make a sample multilabled data

make_multilabled_data <- function(filter_data){
  
  multilabel_data <- filter_data %>% 
    dplyr::mutate(category_1 = sample(c(rep(NA,10000), .data$category, NA), nrow(.), replace = T),
           category_2 = sample(c(rep(NA,10000), .data$category, NA, NA), nrow(.), replace = F),
           category_3 = sample(c(rep(NA,10000), .data$category, NA, NA), nrow(.), replace = F))
  
  multilabel_data$labels = multilabel_data %>% 
    dplyr::select(dplyr::starts_with('category')) %>% 
    apply(1, function(x) paste(unique(x[!is.na(x)]), collapse = ", "))
  
  multilabel_data <- multilabel_data %>% 
    dplyr::select(-dplyr::starts_with('category_')) 
  
  return(multilabel_data)
}


# # test
# plot_type = 'correlation'
# filter_data %>%
#   make_multilabled_data() %>%
#   multi_to_single_label(column_name = 'labels') %>%
#   make_overlap_theme(group_type = plot_type) %>%
#   overlap_heatmap_plot(legend = plot_type)
