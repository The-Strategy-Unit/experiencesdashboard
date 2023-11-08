#' trend UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_trend_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    uiOutput(ns("dynamic_trendUI"))
  )
}

#' trend Server Functions
#'
#' @noRd
mod_trend_server <- function(id, filter_data, data_exists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Super UI ----
    output$dynamic_trendUI <- renderUI({
      # Only show module contents if the data from the database is not empty
      validate(
        need(data_exists, "Category Trends plots will appear here")
      )

      fluidPage(
        p(HTML(paste0("
        This page provides a timeline visual to show when comments have been 
        received for the different categories and sub-categories, it is intended
        to help with identifying patterns within this, before you drill down 
        into the underling comments. To get further detail about how to use the
        timeline visual, please see the ",
        enurl("https://cdu-data-science-team.github.io/PatientExperience-QDC/dashboard/#distribution-of-comments-over-time",
              "Patient Experience – QDC documentation page.")
        ))),
        strong("Click on a box to see the comments for that month on the table below."),
        fluidRow(
          uiOutput(ns("category_selectionUI"))
        ),
        br(),
        tabsetPanel(
          id = ns("tabset_trend"),
          type = "tabs",

          # A Sub-tab
          tabPanel("High-Level Categories Trend",
            value = "super_cat",
            plotly::plotlyOutput(ns("super_category_trend_plot")) %>%
              shinycssloaders::withSpinner(),
            hr(),
            # downloadButton(ns("super_category_download_data"), "Download data",
            #   icon = icon("download")
            # ),
            uiOutput(ns("dynamic_super_category_table"))
          ),

          # A Sub-tab
          tabPanel("Sub-Categories Trend",
            value = "sub_cat",
            fluidRow(
              column(
                12,
                plotly::plotlyOutput(ns("sub_category_trend_plot")) %>%
                  shinycssloaders::withSpinner(),
                hr(),
                # downloadButton(ns("sub_category_download_data"), "Download data",
                #   icon = icon("download")
                # ),
                uiOutput(ns("dynamic_sub_category_table"))
              )
            )
          )
        )
        # hr(),
        # DT::DTOutput(ns("dynamic_super_category_table"))
      )
    })

    # dynamic ui part ----
    output$category_selectionUI <- renderUI({
      choices <- filter_data()$single_labeled_filter_data$super_category %>%
        unique() %>%
        na.omit() %>%
        sort()

      if (input$tabset_trend == "sub_cat") {
        column(
          12,
          selectInput(
            session$ns("select_super_category"),
            label = h5(strong("Select a Category to see its Sub-categories Trend (defaults to first category):")),
            choices = choices,
            selected = choices[1]
          )
        )
      }
    })

    # server code ----
    ## trend plot for the super-category ----
    super_plot_source <- ns("event_id-1") # to get user data from the super category plot
    output$super_category_trend_plot <- plotly::renderPlotly({
      filter_data()$single_labeled_filter_data %>%
        make_trend_data() %>%
        plot_trend("super_category", source = super_plot_source) %>%
        plotly::event_register("plotly_click")
    })

    ## the comments tables - super category ----
    return_data <- reactive({
      data <- filter_data()$single_labeled_filter_data

      if (isTruthy(plotly::event_data("plotly_click", source = super_plot_source, priority = "input"))) {
        d <- plotly::event_data("plotly_click", source = super_plot_source, priority = "input")

        super_category_selected <- d$y
        selected_date <- d$x

        print(super_category_selected)
        print(selected_date)

        data <- filter_data()$single_labeled_filter_data %>%
          dplyr::filter(
            super_category == super_category_selected,
            format(as.Date(date), "%Y-%m") == format(as.Date(selected_date), "%Y-%m")
          )
      }
      return(prep_data_for_comment_table(data))
    })

    output$dynamic_super_category_table <- renderUI({
      mod_comment_download_server(ns("comment_download_1"), return_data(), filepath = "super_category-trend-")
    })
    
    ## trend plot for the sub-category ----
    sub_plot_source <- ns("event_id-1") # to get user data from the sub category plot
    output$sub_category_trend_plot <- plotly::renderPlotly({
      req(!is.null(input$select_super_category))
      
      super_category <- input$select_super_category
      filter_data()$single_labeled_filter_data %>%
        make_trend_data(selected_super_category = super_category) %>%
        plot_trend("category", source = sub_plot_source, super_category = super_category) %>%
        plotly::event_register("plotly_click")
    })

    ## the comments tables - sub category ----
    return_data2 <- reactive({
      req(!is.null(input$select_super_category))
      
      data <- filter_data()$single_labeled_filter_data %>% 
        dplyr::filter(super_category == input$select_super_category)

      if (isTruthy(plotly::event_data("plotly_click", source = sub_plot_source, priority = "input"))) {
        sub_d <- plotly::event_data("plotly_click", source = sub_plot_source, priority = "input")

        sub_category_selected <- sub_d$y
        selected_date <- sub_d$x

        print(sub_category_selected)
        print(selected_date)

        return_data <- data %>%
          dplyr::filter(
            category == sub_category_selected,
            format(as.Date(date), "%Y-%m") == format(as.Date(selected_date), "%Y-%m")
          )
        
        if (nrow(return_data) > 0) data <- return_data
      }
      
      return(prep_data_for_comment_table(data))
    })

    output$dynamic_sub_category_table <- renderUI({
      mod_comment_download_server(ns("comment_download_2"), return_data2(), filepath = "sub_category-trend-")
    })
  })
}
