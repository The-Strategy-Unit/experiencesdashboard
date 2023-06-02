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
  data[[column_name]] <- stringr::str_remove_all(
    data[[column_name]],
    # "(c[(])|[)]|[\"\"]" # to remove c() and ""
    # "(c[(])|[)]" # to remove c()
    "\\[|\\]"  # to remove []
  )
  return(data)
}

#' Convert an vector to a Character String
#' @param x a vector e.g. c("a", "b", "aaa")
#' @return A character vector of length 1
#' @noRd
to_string <- function(x){
  (\(.x) paste0('"', .x, '"', collapse = ", "))(x)
}

#' multilabeled_to_singlelabel
#'
#' @description internal function to Convert a single-label data like such made using `multi_to_single_label()`
#'  function into a multi-labeled format with a unique comment per row
#'
#' @param sl_data A dataframe with a unique row per category per comment type per comment ID
#' @param column_name The name of the column holding the comma separated labels
#' @param n_labels maximum number of labels assigned to any row
#'
#' @return A dataframe with a row per comment per comment type
#' @noRd
single_to_multi_label <- function(sl_data) {
  mt_data <- sl_data %>% 
    dplyr::group_by(comment_id, comment_type) %>%
    dplyr::summarise(across(c(date, comment_txt, fft), unique),
                     across(c(category, super_category), \(x) list(unique(x))), # remove duplicated
                     ) %>% 
    dplyr::ungroup()  %>% 
    dplyr::arrange(date) %>% 
    dplyr::mutate(across(c(category, super_category), ~lapply(.x, to_string))) # for user friendly print
  
  stopifnot("values in 'comment ID' should be unique" = mt_data$comment_id %>% duplicated() %>% sum() == 0)

  return(mt_data)
}

#' multilabeled_to_singlelabel
#'
#' @description Convert a multi-labeled data to a single-label data with a category per comment per row
#'
#' @param mt_data A dataframe with a single column for the comma separated labels for each row
#' @param column_name The name of the column holding the comma separated labels
#' @param n_labels maximum number of labels assigned to any row
#'
#' @return A dataframe
#' @export
multi_to_single_label <- function(mt_data, column_name, n_labels = 10) {
  mt_data %>%
    dplyr::filter(
      !is.na(mt_data[[column_name]]),
      mt_data[[column_name]] != ""
    ) %>%
    dplyr::mutate(original_label = .data[[column_name]]) %>%
    tidyr::separate(column_name,
      into = paste("label_", 1:n_labels),
      sep = ",",
      fill = "right"
    ) %>%
    tidyr::pivot_longer(dplyr::starts_with("label_")) %>%
    dplyr::mutate(value = stringi::stri_trim_both(.data$value)) %>%
    tidyr::drop_na(value) %>%
    dplyr::filter(value != "")
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
        dplyr::filter(!(item1 == ""), !(item2 == "")) %>%
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
      ggplot2::aes(
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
interactive_heatmap <- function(data, group_type = c("count", "correlation"), source = "A") {
  # check argument is valid and choose the correct logical predicate
  group_type <- match.arg(group_type)
  stopifnot("group type must be one of 'count', or 'correlation'" = length(group_type) == 1)

  p <- data %>%
    plotly::plot_ly(
      x = ~item1, y = ~item2, z = ~value,
      type = "heatmap",
      hoverinfo = "none",
      colors = colorRamp(c("#ffffff", "#0072B2")),
      # height = '500',
      source = source
    ) %>%
    plotly::layout(
      xaxis = list(
        title = "",
        tickangle = -45,
        showgrid = F,
        ticks = ""
      ),
      yaxis = list(
        title = list(text = ""),
        ticks = ""
      )
    ) %>%
    # edit hover information
    plotly::add_markers(
      inherit = F,
      x = ~item1, y = ~item2,
      data = data,
      showlegend = F,
      text = ~value,
      color = I("transparent"),
      hovertemplate = paste0(
        group_type, ": %{text}<br>",
        "%{y}<br>",
        "%{x}"
      )
    ) %>%
    plotly::config(displayModeBar = FALSE)

  # add the text value to count plot but not the correlation plot
  if (group_type == "count") {
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

#' Add NHS theme to ggplot chart
#'
#' This adds a theme to your chart by tweaking
#' [NHSRtheme](https://github.com/nhs-r-community/NHSRtheme/blob/main/R/theme_nhs.R)
#'
#' @export
add_theme_nhs <- function() {
  ggplot2::theme(
    # Set the title and subtitle to be much larger and bolder than other text
    plot.title = ggplot2::element_text(size = 18),
    plot.subtitle = ggplot2::element_text(
      size = 14,
      margin = ggplot2::margin(9, 0, 9, 0)
    ),

    # For facets, set the title to be larger than other text and left-justified
    # and the panel background to white
    strip.text = ggplot2::element_text(size = 16),
    strip.background = ggplot2::element_rect(fill = "white"),

    # Set the legend to be aligned at the bottom with no title and background
    # Note: the legend may need manual tweaking based on plot coordinates
    # legend.title = ggplot2::element_blank(),
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.position = "bottom",


    # Remove all minor gridlines and add major y gridlines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#E8EDEE"),
    panel.grid.major.x = ggplot2::element_blank(),

    # Set the panel background to be blank
    panel.background = ggplot2::element_blank(),


    # Set the axis to have to have no lines or ticks with a small
    # margin on the x axis
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 0,
      margin = ggplot2::margin(5, 0, 10, 0)
    )
  )
}

#' Draw upset plot
#'
#' @param upset_data  a dataframe with each column representing a membership in the class. values are
#'                    1 - if the row is a member of the class or 0 if otherwise
#' @param intersect columns containing the classes i.e. names of the categories to show (Ex: c("staff manner", "staff attitude"))
#' @param min_size minimal number of observations in an intersection for it to be included
#' @param title title of the plot
#' @param ...
#'
#' @return a plot of upset object (not a ggplot object)
#' @noRd
upset_plot <- function(upset_data, intersect, min_size = 1, title = "", ...) {
  label_size <- 8
  group_text_size <- 7

  ComplexUpset::upset(upset_data, intersect,
    min_size = min_size, # minimum number of observation in a group for the group to be shown (intersection size)
    width_ratio = 0.1,
    height_ratio = 1.6, # height of intersection matrix to the intersection plot
    min_degree = 2, # show intersections with at least 2 groups
    # n_intersections = 40, # the max number of the intersections (bars) to be displayed;
    name = "Groups",

    # Manipulate the set size plot

    set_sizes = (
      ComplexUpset::upset_set_size(
        geom = ggplot2::geom_bar(fill = "#005EB8"),
        position = "right",
        filter_intersections = TRUE
      ) +
        # display the count text
        # ggplot2::geom_text(ggplot2::aes(label = ggplot2::after_stat(count)),
        #                    hjust = -0.2, stat = "count"
        # ) +
        ggplot2::ylab("No. of comments") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 0),
          text = ggplot2::element_text(size = label_size)
        )
    ),

    # set_sizes=FALSE,    # or hide the set size

    # Manipulate the intersection matrix color

    stripes = ComplexUpset::upset_stripes(
      geom = ggplot2::geom_segment(linewidth = 1.8),
      # color = "white"
    ),
    encode_sets = FALSE, # for annotate() to select the set by name disable encoding
    #
    matrix = (
      ComplexUpset::intersection_matrix(
        geom = ggplot2::geom_point(size = 1.6),
        outline_color = list(active = "#000000", inactive = "grey70")
      )
    ),

    # stripes='white',

    # Add plots (panels) to the upper part of the intersection matrix

    base_annotations = list(

      # Manipulate the set size plot
      "Intersection size" = ComplexUpset::intersection_size(
        # Any parameter supported by geom_text can be passed in text list
        text = list(vjust = -0.3, size = 2.3),
        fill = "#005EB8"
        # counts=FALSE, # uncheck to remove the count text
      ) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          text = ggplot2::element_text(size = label_size)
        ) +
        ggplot2::ylab("No. of comments")
    ),
    themes = ComplexUpset::upset_modify_themes(
      list(
        "intersections_matrix" = ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = group_text_size)
        )
      )
    ),
    wrap = TRUE # add it so the title applies to the entire plot instead of the intersection matrix only
  ) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(text = ggplot2::element_text(size = 7))
}


#' one hot encode a single column while still keeping other columns
#'
#' @param df dataframe to encode.
#' @param column column of interests. Other columns combined must uniquely identify each row (best is unique row_id is present).
#'
#' @return dataframe
#' @noRd
#'
#' @examples
#' data.frame(
#'   group = c("A", "A", "A", "A", "A", "B", "C"),
#'   student = c("01", "01", "01", "02", "02", "01", "02"),
#'   exam_pass = c("Y", "N", "Y", "N", "Y", "Y", "N"),
#'   subject = c("Math", "Science", "Japanese", "Math", "Science", "Japanese", "Math")
#' ) |> one_hot_labels("subject")
one_hot_labels <- function(df, column) {
  df |>
    dplyr::mutate(input = 1) |>
    tidyr::pivot_wider(
      names_from = dplyr::any_of(column),
      values_from = input, values_fill = 0
    )
}


#' Internal function for the comment datatable in format required by `single_to_multi_label()` function
#'
#' @param comment_data a dataframe
#' @param tidy_format boolean if the data was in single labeled or multilabeled format
#' @return a formatted datatable
#' @noRd
prep_data_for_comment_table <- function(comment_data, tidy_format=TRUE) {
  
  data <- comment_data 
  
  if(tidy_format){
    data <- data %>%
      single_to_multi_label() 
  }
  
  data <- data %>% 
    dplyr::select(date, comment_type, fft, comment_txt, category, super_category) %>%
    dplyr::mutate( 
      across(c(category, super_category), ~ sapply(.x, paste0, simplify = TRUE, USE.NAMES = F))
    ) %>% 
    dplyr::mutate(
      comment_type = stringr::str_replace_all(comment_type, "comment_1", get_golem_config("comment_1"))
    ) %>%
    dplyr::arrange(date)
  
  if (isTruthy(get_golem_config("comment_2"))) {
    data <- data %>%
      dplyr::mutate(
        comment_type = stringr::str_replace_all(comment_type, "comment_2", get_golem_config("comment_2"))
      )
  }
  
  colnames(data) <- c(
    "Date", "FFT Question", "FFT Score",
    "FFT Answer", "Sub-Category", "Category"
  )
  
  print(nrow(data)) # for debugging
  
  return(data)
}

#' Internal function for the comment datatable in format required by `single_to_multi_label()` function
#'
#' @param data a dataframe
#' @return a formatted datatable
#'
#' @noRd
comment_table <- function(data) {

  # add NHS blue color to the table header
  initComplete <- DT::JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#005EB8', 'color': '#fff'});",
    "}"
  )

  return(
    DT::datatable(
      data,
      extensions = "Buttons", # required to show the download buttons and groups
      options = list(
        dom = "ipt",
        buttons = c("csv", "excel", "pdf"),
        # autoWidth = TRUE,            # required for width option for columns to  work
        # columnDefs = list(list(width = '500px', targets = c(3))),
        initComplete = initComplete,
        pageLength = 50,
        scrollX = TRUE,
        selection = "single"
      ),
      filter = "top",
      rownames = FALSE,
      class = "display cell-border compact stripe",
    )
  )
}

#' Find high level categories of sub categories
#'
#' @description
#' Internal function to find the high level category to a list of sub categories as defined in the QDC framework
#'
#' @param sub_cats list of subcategories
#'
#' @return list of same length as sub_cats
#' @noRd
assign_highlevel_categories <- function(sub_cats) {
  # framework <- readxl::read_excel(here::here(app_sys(), "app/www", "FFT-QDC Framework v5 - 20230428.xlsx"), sheet = 2) %>%
  #   dplyr::select(Category, `Sub-category`)

  sapply(sub_cats, function(v) {
    hl_cat <- framework %>%
      dplyr::filter(`Sub-category` == v) %>% # View()
      dplyr::pull(Category)
    return(if (length(hl_cat) != 0) hl_cat else "Other Category")
  },
  simplify = TRUE, USE.NAMES = F
  )
}
