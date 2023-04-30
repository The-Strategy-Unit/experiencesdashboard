#' trend_overlap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_trend_overlap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        uiOutput(ns("trendUI"))
      ),
      tabsetPanel(
        id = ns("tabset_trend"),
        type = "tabs",

        # A Sub-tab

        tabPanel("Trend",
          value = "trend",
          br(),
          plotly::plotlyOutput(ns("category_trend_plot")) %>%
            shinycssloaders::withSpinner()
        ),

        # A Sub-tab

        tabPanel("Theme Overlap",
          value = "overlap",
          br(),
          fluidRow(
            column(
              12,
              # plotly::plotlyOutput(ns("category_overlap_plot")) %>%
              #   shinycssloaders::withSpinner(),
              plotOutput(ns("category_upset")) %>%
                shinycssloaders::withSpinner(),
              hr(),
              uiOutput(ns("trendUI_2")),
              uiOutput(ns("dynamic_overlap_text"))
            )
          )
        )
      )
    )
  )
}

#' trend_overlap Server Functions
#'
#' @noRd
mod_trend_overlap_server <- function(id, filter_data,
                                     overlap_plot_type = c("count", "correlation")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    global <- reactiveValues(selected_cat1 = NULL, selected_cat2 = NULL, selected_cats = NULL)

    # an internal function to check user selected categories

    check_cat_selection <- function(selected_list) {
      selected_list <- selected_list[selected_list != ""]
      selected_list <- unique(selected_list)

      return(length(selected_list) > 1)
    }

    output$trendUI <- renderUI({
      choices <- na.omit(unique(filter_data()$filter_data$category))

      # select the first 5 category for the trend tab

      req(input$tabset_trend == "trend")

      selectInput(
        session$ns("select_super_category"),
        label = h5(strong("Select categories (defaults to all):")),
        choices = choices,
        multiple = TRUE,
        selected = choices[1:5]
      )
    })

    output$trendUI_2 <- renderUI({
      req(input$tabset_trend == "overlap")

      choices <- c("", na.omit(unique(filter_data()$filter_data$category)))

      fluidRow(
        column(4, selectInput(
          session$ns("select_category1"),
          label = h5(strong("Select category")),
          choices = choices,
        )),
        column(
          4,
          selectInput(
            session$ns("select_category2"),
            label = h5(strong("Select category")),
            choices = choices,
          )
        ),
        column(
          4,
          selectInput(
            session$ns("select_category3"),
            label = h5(strong("Select category")),
            choices = choices,
          )
        ),
      )
    })

    # overlap tab UI ----

    output$dynamic_overlap_text <- renderUI({
      validate(
        need(
          check_cat_selection(c(input$select_category1, input$select_category2, input$select_category3)),
          # length(global$selected_cats) > 1,
          "Please select at least two distinct categories to view"
        )
      )

      selected_cat1 <- input$select_category1
      selected_cat2 <- input$select_category2
      selected_cat3 <- input$select_category3

      selected_cats <- unique(c(selected_cat1, selected_cat2, selected_cat3))

      global$selected_cats <- selected_cats[selected_cats != ""]
      
      if (length(global$selected_cats) > 1) {
        
        tagList(
          paste(toupper(c(global$selected_cats)), collapse=" | "),
          hr(),
          htmlOutput(ns("overlap_text"))
        )
      }
    })

    # Verbatim output

    # output$dynamic_overlap_text <- renderUI({
    #
    #   validate(
    #     need(plotly::event_data("plotly_click", source = 'overlap_plot',
    #                             priority = 'event'),
    #          "Please select the categories to view")
    #   )
    #
    #   tagList(
    #     paste(toupper(global$selected_cat2),'<==>',toupper(global$selected_cat1)),
    #     hr(),
    #     htmlOutput(ns("overlap_text"))
    #   )
    #
    # })

    # Create category trend

    trend_data <- reactive({
      return_data <- filter_data()$filter_data %>%
        dplyr::mutate(
          comment_type = factor(comment_type,
            levels = unique(comment_type),
            labels = c(
              get_golem_config("comment_1"),
              get_golem_config("comment_2")
            )
          )
        ) %>%
        dplyr::mutate(date = as.Date(cut(date, "month"))) %>%
        dplyr::group_by(comment_type, date, category) %>%
        dplyr::summarize(cat = dplyr::n()) %>%
        dplyr::group_by(comment_type, date) %>%
        dplyr::mutate(prop = round(cat / sum(cat), 2)) %>%
        dplyr::filter(!is.na(category))

      if (isTruthy(input$select_super_category)) {
        return_data <- return_data %>%
          dplyr::filter(category %in% input$select_super_category)
      }

      return_data
    })

    # trend plot

    output$category_trend_plot <- plotly::renderPlotly({
      p <- trend_data() %>%
        ggplot2::ggplot(ggplot2::aes(
          x = date, y = prop,
          color = category, group = category
        )) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::scale_x_date(date_breaks = "4 months", date_labels = "%b %y") +
        ggplot2::scale_y_continuous(label = scales::label_percent(accuracy = 1)) +
        NHSRtheme::scale_colour_nhs() +
        ggplot2::facet_grid(. ~ comment_type) +
        ggplot2::labs(x = NULL, y = "% contribution", color = "Category") +
        add_theme_nhs()

      return(
        p %>%
          plotly::ggplotly() %>%
          plotly::config(displayModeBar = FALSE) %>%
          plotly::layout(legend = list(
            title = "",
            orientation = "h",
            y = -0.2,
            xanchor = "center",
            x = 0.5
          ))
      )
    })

    # Create tidy data for the overlapping plot / upset plot

    tidy_data <- reactive({
      filter_data()$filter_data %>%
        make_sample_multilabeled_data() %>%
        multi_to_single_label(column_name = "labels")
    })


    output$category_upset <- renderPlot({
      upset_data <- tidy_data() %>%
        dplyr::select(-name) %>% # to ensure  the rows are well mapped
        one_hot_labels(column = "value") # apply one hot encoding to the single label column

      # upset_plot(upset_data, intersect = unique(tidy_data$value),
      #            title = "Without empty groups (Short dropped)")

      upset_plot(upset_data,
        intersect = unique(tidy_data()$value),
        show_all = TRUE, title = "Upset plot showing relationship across categories"
      )
    })

    # Verbatim text ----

    output$overlap_text <- renderText({
      # only run when at least 2 categories are selected
      req(
        length(global$selected_cats) > 1
        # check_cat_selection(input$select_category1, input$select_category2, input$select_category3)
      )

      final_text <- show_multilabeled_text(tidy_data(), "value", na.omit(global$selected_cats))

      cat(paste(toupper(c(global$selected_cats)), "|"), " :: ", length(final_text), "\n")

      return(final_text)
    })



    # output$category_overlap_plot <- plotly::renderPlotly({
    #
    #   clicked_data <- tidy_data()  %>%
    #     make_overlap_theme(group_type = overlap_plot_type) %>%
    #     reshape_overlapping_theme()
    #
    #   clicked_data %>%
    #     interactive_heatmap(overlap_plot_type, source = 'overlap_plot')
    # })
    #
    #
    # output$overlap_text <- renderText({
    #
    #   req(plotly::event_data("plotly_click", source = 'overlap_plot',
    #                          priority = 'event'))
    #
    #   clickData <- plotly::event_data("plotly_click", source = 'overlap_plot',
    #                                   priority = 'event')
    #
    #   global$selected_cat1 <- clickData$x
    #   global$selected_cat2 <- clickData$y
    #
    #   final_text <- show_multilabeled_text(tidy_data(), 'value',
    #                                        c(global$selected_cat1,
    #                                          global$selected_cat2))
    #
    #   cat(paste(global$selected_cat1),'<==>',
    #       toupper(global$selected_cat2), ' :: ',
    #       length(final_text), ' \n')
    #
    #   return(final_text)
    #
    # })
  })
}
