#' Convert an vector to a Character String
#' @param x a vector e.g. c("a", "b", "aaa")
#' @return A character vector of length 1
#' @noRd
to_string <- function(x) {
  (\(.x) paste0('"', .x, '"', collapse = ", "))(x)
}

#' multilabeled_to_singlelabel
#'
#' @description internal function to Convert a single-label data
#' into a multi-labeled format with a unique comment per row
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
    dplyr::summarise(
      across(c(date, comment_txt, fft), unique),
      across(c(category, super_category), \(x) list(unique(x))), # remove duplicated
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(across(c(category, super_category), ~ lapply(.x, to_string))) # for user friendly print

  stopifnot("values in 'comment ID' should be unique" = mt_data$comment_id %>% duplicated() %>% sum() == 0)

  return(mt_data)
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
prep_data_for_comment_table <- function(comment_data, tidy_format = TRUE) {
  data <- comment_data

  if (tidy_format) {
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
