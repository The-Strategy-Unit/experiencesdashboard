#' reorder_cormat
#'
#' @description function to reorder the correlation matrix or similar matrix by
#'              using values between variables as distance
#'
#' @param cormat a correlation matrix or similar matrix
#'
#' @return A reordered matrix with same dimension as input matrix
#' @noRd
reorder_cormat <- function(cormat) {
  dd <- as.dist((1 - cormat) / 2)
  hc <- hclust(dd)

  return(cormat[hc$order, hc$order])
}


#' clean  dataframe column
#'
#' @description Remove c(, ), and "" from a dataframe column
#'
#' @param data A dataframe
#' @param column_name  The name of the column holding the comma separated labels
#'
#' @return a dataframe
#' @noRd
tidy_label_column <- function(data, column_name) {
  data[[column_name]] <- str_remove_all(
    data[[column_name]],
    "(c[(])|[)]|[\"\"]"
  )
  return(data)
}


#' multilabeled_to_singlelabel
#'
#' @description Convert a multi-labeled data to a tidy single-label data
#'
#' @param mt_data A dataframe with a single column for the comma separated labels for each row
#' @param column_name The name of the column holding the comma separated labels
#' @param n_labels maximum number of labels assigned to any row
#'
#' @return A dataframe
#' @export
multi_to_single_label <- function(mt_data, column_name, n_labels = 10) {
  mt_data %>%
    dplyr::mutate(original_label = .data[[column_name]]) %>%
    tidyr::separate(column_name,
      into = paste("label_", 1:n_labels),
      sep = ","
    ) %>%
    tidyr::pivot_longer(dplyr::starts_with("label_")) %>%
    dplyr::mutate(value = stringi::stri_trim_both(.data$value)) %>%
    tidyr::drop_na(value)
}


#' make overlap topics
#'
#' @description Convert a multi-labeled data that has been tidied using
#'              `multi_to_single_label()` into a wide format
#'
#' @param tidy_multilabeled_data A multi-labeled data that has been tidied with `multi_to_single_label()`
#' @param group_type string value to group by  either 'count' for number of comment
#'        or 'correlation' for Pearson correlation.
#'
#' @return A dataframe in a wide format
#' @noRd
make_overlap_theme <- function(tidy_multilabeled_data, group_type = c("count", "correlation")) {
  # check argument is valid and choose the correct logical predicate
  group_type <- match.arg(group_type)
  stopifnot("group type must be one of 'count', or 'correlation'" = length(group_type) == 1)

  if (group_type == "count") {
    return(
      tidy_multilabeled_data %>%
        dplyr::inner_join(tidy_multilabeled_data, by = "row_id") %>%
        dplyr::count(value.x, value.y) %>%
        dplyr::filter(value.x != value.y) %>%
        dplyr::rename("item1" = value.x, "item2" = value.y) %>%
        dplyr::arrange(item1) %>%
        tidyr::pivot_wider(names_from = item1, values_from = n) %>%
        dplyr::arrange(item2)
    )
  } else {
    return(
      tidy_multilabeled_data %>%
        dplyr::count(value, comment_txt, sort = TRUE) %>%
        widyr::pairwise_cor(value, comment_txt, n, sort = TRUE) %>%
        dplyr::mutate(correlation = round(correlation, 2)) %>%
        dplyr::filter(!(item1==''), !(item2=='')) %>% 
        dplyr::arrange(item1) %>% 
        tidyr::pivot_wider(names_from = item1, values_from = correlation) %>% 
        dplyr::arrange(item2)
    )
  }
}


#' reshape overlapping topic
#'
#' @description A function to remove redundant information from data in wide format,
#'              reorder its values and convert it to a long data format
#'
#' @param wide_overlapping_theme a dataframe made from `make_overlap_theme()`
#'
#' @return Dataframe in long format needed for plotting
#' @noRd
reshape_overlapping_theme <- function(wide_overlapping_theme) {
  cormat <- as.matrix(wide_overlapping_theme[, -1])
  rownames(cormat) <- wide_overlapping_theme %>% dplyr::pull(item2)
  cormat[is.na(cormat)] <- 0

  cormat <- reorder_cormat(cormat)

  cormat[upper.tri(cormat)] <- NA
  diag(cormat) <- NA
  cormat[(cormat) == 0] <- NA

  return(reshape2::melt(cormat) %>%
    dplyr::rename("item1" = Var1, "item2" = Var2))
}


#' plot overlap heatmap plot
#'
#' @description A function to plot the heatmap of the overlapping data
#'
#' @param data the dataframe to plot
#' @param legend A string to rename the legend title
#' @param filter_value threshold of values to include in the plot
#'
#' @return An heatmap
#' @noRd
overlap_heatmap_plot <- function(data, legend = "Legend", threshold_value = -10) {
  return(
    data %>%
      dplyr::filter(value > threshold_value) %>%
      dplyr::filter(!is.na(value)) %>%
      ggplot2::ggplot(ggplot2::aes(x = item1, y = item2, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = value), size = 5) +
      ggplot2::scale_fill_gradient2(
        low = "#999999", high = "#0072B2",
        midpoint = mean(data$value[!is.na(data$value)]),
        name = stringr::str_to_title(legend)
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 45, vjust = 1,
          hjust = 1, face = "bold", size = 12
        ),
        axis.text.y = ggplot2::element_text(face = "bold", size = 12),
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
      ) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::ggtitle(label = stringr::str_to_sentence(paste(legend, "between overlapping topics")))
  )
}


#' plot overlap network graph
#'
#' @description function to plot the network graph of the overlapping data with `attr_column`
#' deciding the thickness of the links
#'
#' @param data dataframe of edges with columns for “from”, “to”, and edge attributes. “from” and “to” column names can be any name
#' @param attr_column String, a name of the column for the edge attributes
#' @param filter_value threshold of values to include in the plot
#'
#' @return a network graph
#' @noRd
visualize_network <- function(data, attr_column = "value", threshold_value = 0) {
  set.seed(2023)

  # filter for stronger correlations among categories and visualize them in a network
  data %>%
    dplyr::filter(.data[[attr_column]] > threshold_value) %>%
    igraph::graph_from_data_frame() %>%
    ggraph::ggraph(layout = "fr") +
    ggraph::geom_edge_link(
      aes(
        alpha = .data[[attr_column]],
        width = .data[[attr_column]]
      ),
      edge_colour = "#fdbf6f"
    ) +
    ggraph::geom_node_point(color = "#999989") +
    ggraph::geom_node_text(aes(label = name),
      size = 3, repel = TRUE,
      color = "#1f78b4"
    ) +
    ggplot2::theme_void()
}


#' Plot interactive heatmap with plotly
#'
#' @param data the dataframe to plot
#' @param group_type string value to group by  either 'count' for number of comment
#'        or 'correlation' for Pearson correlation
#'
#' @return a plotly figure
#' @noRd
interactive_heatmap <- function(data, group_type = c("count", "correlation"), source='A') {
    # check argument is valid and choose the correct logical predicate
  group_type <- match.arg(group_type)
  stopifnot("group type must be one of 'count', or 'correlation'" = length(group_type) == 1)
  
    p <- data %>% 
      plotly::plot_ly(x = ~item1, y=~item2, z=~value, 
              type = "heatmap",
              hoverinfo = "none",
              colors =colorRamp(c("#ffffff", "#0072B2")),
              # height = '500',
              source = source
      ) %>%
      plotly::layout(
        xaxis = list(title="",
                     tickangle=-45), 
        yaxis = list(title=list(text= ""))
      ) %>% 
      # edit hover information
      plotly::add_markers(
        inherit = F,
        x = ~item1, y = ~item2,
        data = data,
        showlegend = F,
        text = ~value,
        color = I("transparent"), 
        hovertemplate = paste0(group_type, ": %{text}<br>", 
                               "%{y}<br>",
                               "%{x}")
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    # add the text value to count plot but not the correlation plot
    if (group_type == 'count'){
      p <- p %>%  
        plotly::add_text(
            inherit = F,
            x = ~item1, y = ~item2,
            data = data,
            showlegend = F,
            text = ~value
        )
    }
    
    return(p)
}


#' find the common comments between two categories 
#'
#' @param data a dataframe with a unique row identifier 
#' @param theme_column the column where the look values to compare is
#' @param filter_by_themes list of the two values to compare
#' 
#' @return strings
#' @noRd
show_multilabeled_text <- function(data, theme_column, filter_by_themes) {
  
  data %>%
    dplyr::filter(.data[[theme_column]] == filter_by_themes[1]) %>%
    dplyr::semi_join(
      data %>%
        dplyr::filter(.data[[theme_column]] == filter_by_themes[2]),
      by = "row_id"
    ) %>%
    dplyr::pull(comment_txt) %>%
    paste0(., hr())
  
}


#' make_sample_multilabeled_data
#'
#' @description this is a temporary function needed to make a sample
#'              multilabeled data from the single labeled data we have.
#'              it will be remove when the actual multi-labeled  data is available
#'
#' @param filter_data A single labeled data with label column named 'category'
#'
#' @return Dataframe, a multilabeled data
#' @noRd
make_sample_multilabeled_data <- function(filter_data) {
  multilabel_data <- filter_data %>%
    dplyr::mutate(
      category_1 = sample(c(rep(NA, 10000), .data$category, NA), nrow(.), replace = T),
      category_2 = sample(c(rep(NA, 10000), .data$category, NA, NA), nrow(.), replace = F),
      category_3 = sample(c(rep(NA, 10000), .data$category, NA, NA), nrow(.), replace = F)
    )

  multilabel_data$labels <- multilabel_data %>%
    dplyr::select(dplyr::starts_with("category")) %>%
    apply(1, function(x) paste(unique(x[!is.na(x)]), collapse = ", "))

  multilabel_data <- multilabel_data %>%
    dplyr::select(-dplyr::starts_with("category"))

  return(multilabel_data)
}
