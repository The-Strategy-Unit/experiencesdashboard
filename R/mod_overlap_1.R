#' overlap_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overlap_1_ui <- function(id) {
  ns <- NS(id)
}

#' overlap_1 Server Functions
#'
#' @noRd
mod_overlap_1_server <- function(id, filter_data, input_select_super_category, input_min_size) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    global <- reactiveValues(
      selected_cat1 = NULL,
      selected_cat2 = NULL, selected_cats = NULL
    )

    # an internal function to check user selected categories
    check_cat_selection <- function(selected_list) {
      selected_list <- selected_list[selected_list != ""]
      selected_list <- unique(selected_list)

      return(length(selected_list) > 1)
    }

    output$trendUI_2 <- renderUI({
      choices <- c(
        "",
        get_unique_value(filter_data()$single_labeled_filter_data, "category")
      )

      fluidRow(
        column(4, selectInput(
          session$ns("select_category1"),
          label = h5(strong("Select Sub-category")),
          choices = choices,
        )),
        column(
          4,
          selectInput(
            session$ns("select_category2"),
            label = h5(strong("Select Sub-category")),
            choices = choices,
          )
        ),
        column(
          4,
          selectInput(
            session$ns("select_category3"),
            label = h5(strong("Select Sub-category")),
            choices = choices,
          )
        ),
      )
    })

    # overlap tab UI ----
    output$dynamic_select_category_ui <- renderUI({
      validate(
        need(
          check_cat_selection(
            c(
              input$select_category1,
              input$select_category2,
              input$select_category3
            )
          ),
          "Please select at least two distinct sub-categories to view related comments"
        )
      )

      selected_cat1 <- input$select_category1
      selected_cat2 <- input$select_category2
      selected_cat3 <- input$select_category3

      selected_cats <- unique(c(selected_cat1, selected_cat2, selected_cat3))

      global$selected_cats <- selected_cats[selected_cats != ""]

      if (length(global$selected_cats) > 1) {
        tagList(
          paste(stringr::str_to_title(c(global$selected_cats)), collapse = "  ||  "),
          hr()
        )
      }
    })

    # server codes - the overlapping plot / upset plot ----
    upset_data <- reactive({
      filter_data()$single_labeled_filter_data %>%
        one_hot_labels(column = "category") # apply one hot encoding to the single label column
    })


    all_categories <- reactive(
      filter_data()$single_labeled_filter_data %>%
        get_unique_value("category")
    )

    filtered_categories <- reactive({
      req(!is.null(input_select_super_category))

      filter_data()$single_labeled_filter_data %>%
        dplyr::filter(super_category == input_select_super_category) %>%
        get_unique_value("category")
    })

    ## the upset plot ----
    # create a session-level cacheable version of upset_plot()
    memoised_upset_plot <- memoise::memoise(upset_plot, cache = session$cache)
    output$category_upset <- renderPlot(
      {
        if (!is.null(input_select_super_category)) {
          filtered_categories <- filtered_categories()
          title <- paste0(
            'Upset plot showing relationship between sub-categories in "',
            input_select_super_category, '" category'
          )
        } else {
          filtered_categories <- all_categories()
          title <- "Upset plot showing relationship between sub-categories across all Categories"
        }

        min_size <- if (is.numeric(input_min_size)) as.integer(input_min_size) else 1

        tryCatch(
          {
            tryCatch(
              {
                memoised_upset_plot(upset_data(),
                  intersect = filtered_categories,
                  min_size = min_size,
                  title = title
                )
              },
              error = function(e) {
                print("layer 1 error:")
                print(e)
                memoised_upset_plot(upset_data(),
                  intersect = all_categories(),
                  min_size = 2,
                  title = "Upset plot showing relationship between sub-categories across all Categories"
                ) %>%
                  print()

                showModal(modalDialog(
                  title = "Error!",
                  HTML(paste0(
                    p(strong("There is no relationship in this selection")),
                    strong("Plot has default to  show all sub-categories
                           with '2' minimum number of comments")
                  )),
                  easyClose = TRUE
                ))
              }
            )
          },
          error = function(e) {
            print("layer 2 error:")
            print(e)
            showModal(modalDialog(
              title = "Error!",
              HTML(paste0(
                strong("Sorry, there is not enough data to draw the upset plot. Please expand your selection")
              )),
              easyClose = TRUE
            ))
          }
        )
      },
      height = function() {
        # get the plot specific info.
        session$clientData[[paste0("output_", get_module_id(id, session), "category_upset_height")]] * session$clientData$pixelratio * 1.01
      },
      res = 100
    )

    ## Verbatim text table ----
    return_data <- reactive({
      # only run when at least 2 categories are selected
      req(length(global$selected_cats) > 1)

      data <- filter_data()$single_labeled_filter_data %>%
        relationship_table("category", na.omit(global$selected_cats))

      return(prep_data_for_comment_table(data))
    })

    ## the comments tables ----
    output$overlap_table <- renderUI({
      # only run when at least 2 categories are selected
      req(
        check_cat_selection(
          c(
            input$select_category1,
            input$select_category2,
            input$select_category3
          )
        )
      )
      req(length(global$selected_cats) > 1)

      mod_comment_download_server(ns("comment_download_1"), return_data(), filepath = "sub-category relatioship-")
    })

    return(
      tagList(
        plotOutput(ns("category_upset")) %>%
          shinycssloaders::withSpinner(),

        # comment table ui ----
        br(),
        fluidRow(
          column(
            12,
            hr(),
            uiOutput(ns("trendUI_2")),
            uiOutput(ns("dynamic_select_category_ui")),
            uiOutput(ns("overlap_table"))
          )
        )
      )
    )
  })
}
