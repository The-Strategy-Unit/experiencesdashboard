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
    uiOutput(ns("dynamic_trend_overlap"))
  )
}

#' trend_overlap Server Functions
#'
#' @noRd
mod_trend_overlap_server <- function(id, filter_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    global <- reactiveValues(selected_cat1 = NULL, selected_cat2 = NULL, selected_cats = NULL)

    # an internal function to check user selected categories

    check_cat_selection <- function(selected_list) {
      selected_list <- selected_list[selected_list != ""]
      selected_list <- unique(selected_list)

      return(length(selected_list) > 1)
    }

    # Super UI ----
    output$dynamic_trend_overlap <- renderUI({
      validate(
        need(
          filter_data()$filter_data %>%
            dplyr::tally() %>%
            dplyr::pull(n) > 0,
          "Sub-category inter-relationship plots will appear here"
        )
      )

      fluidPage(
        fluidRow(
          uiOutput(ns("trendUI")),
          br()
        ),
        tabsetPanel(
          id = ns("tabset_overlap"),
          type = "tabs",

          # A upset plot tab UI ----

          tabPanel("Visualise relationship between Sub-Categories",
            value = "overlap_plot",
            p("This plot is meant to aid users to explore the relationships between the sub-categories.
              Please go to the 'Overlapping comments' tab to see the actual comments"),
            br(),
            plotOutput(ns("category_upset")) %>%
              shinycssloaders::withSpinner()
          ),

          # A comment table ui ----

          tabPanel("Overlapping comments",
            value = "overlap_comments",
            br(),
            fluidRow(
              column(
                12,
                hr(),
                uiOutput(ns("trendUI_2")),
                hr(),
                uiOutput(ns("dynamic_overlap_table"))
              )
            )
          )
        )
      )
    })

    # dynamic ui part ----
    output$trendUI <- renderUI({
      choices <- filter_data()$single_labeled_filter_data$super_category %>%
        unique() %>%
        na.omit()

      if (input$tabset_overlap == "overlap_plot") {
        fluidRow(
          column(
            6,
            selectInput(
              session$ns("select_super_category"),
              label = h5(strong("Select a Category to see its Sub-categories relationships (defaults to first category):")),
              choices = choices,
              selected = choices[1]
            )
          ),
          column(
            6,
            numericInput(
              ns("min_size"),
              label = h5(strong("Select Minimum Number of comments in Groups (defaults to 2):")),
              value = 2,
              min = 1,
              max = 3000
            )
          )
        )
      }
    }) %>% 
      bindCache(filter_data()$single_labeled_filter_data$super_category)

    output$trendUI_2 <- renderUI({
      req(input$tabset_overlap == "overlap_comments")

      choices <- c("", filter_data()$single_labeled_filter_data$category %>% unique() %>% na.omit() %>% sort())

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
    output$dynamic_overlap_table <- renderUI({
      validate(
        need(
          check_cat_selection(c(input$select_category1, input$select_category2, input$select_category3)),
          "Please select at least two distinct sub-categories to view comments"
        )
      )

      selected_cat1 <- input$select_category1
      selected_cat2 <- input$select_category2
      selected_cat3 <- input$select_category3

      selected_cats <- unique(c(selected_cat1, selected_cat2, selected_cat3))

      global$selected_cats <- selected_cats[selected_cats != ""]

      if (length(global$selected_cats) > 1) {
        tagList(
          paste(toupper(c(global$selected_cats)), collapse = " | "),
          hr(),
          DT::DTOutput(ns("overlap_table"))
        )
      }
    })


    # tab 2 server codes - the overlapping plot / upset plot ----
    upset_data <- reactive({
      filter_data()$single_labeled_filter_data %>%
        dplyr::rename(value = category) %>%
        one_hot_labels(column = "value") # apply one hot encoding to the single label column
    }) %>%
      bindCache(filter_data()$single_labeled_filter_data)

    ## the upset plot ----
    memoised_upset_plot <- memoise::memoise(upset_plot, cache = session$cache) # create a session-level cacheable version of upset_plot()
    output$category_upset <- renderImage({
      
        req(!is.null(input$select_super_category))
        
        pixelratio <- session$clientData$pixelratio
        width <- session$clientData$`output_trend_overlap_ui-category_upset_width`
        height <- session$clientData$`output_trend_overlap_ui-category_upset_height`
        
        all_categories <- filter_data()$single_labeled_filter_data %>% 
          dplyr::pull(category) %>% unique() %>% na.omit() %>% sort()

        filtered_categories <- filter_data()$single_labeled_filter_data %>% 
          dplyr::filter(super_category == input$select_super_category) %>% 
          dplyr::pull(category) %>% unique() %>% na.omit() %>% sort()
        
        
        min_size <- if (is.numeric(input$min_size)) input$min_size else 1

        # A temp file to save the output. This file will be removed later by renderImage
        # outfile <- tempfile(tmpdir = here::here(app_sys(), "app/www"), fileext='.png')
        outfile <- tempfile(fileext = ".png")

        png(outfile,
          width = width * pixelratio, height = height * pixelratio * 1.3,
          res = 120 * pixelratio
        )

        tryCatch({
          tryCatch({
              memoised_upset_plot(upset_data(),
                intersect = filtered_categories,
                min_size = as.integer(min_size),
                title = paste("Upset plot showing relationship between", 
                              input$select_super_category, " - Sub categories")
              ) %>%
                print()
            },
            error = function(e) {
              print(e)
              memoised_upset_plot(upset_data(),
                  intersect = all_categories,
                  min_size = 2,
                  title = "Upset plot showing relationship between All Sub-categories") %>%
                print()
              
              showModal(modalDialog(
                title = "Error!",
                HTML(paste0(p('There is no relationship in this selection'),
                            strong("Default back to  All sub-categories with '2' Minimum Number of comments"))
                ),
                easyClose = TRUE
              ))
            })
        },
          error = function(e) {
            print(e)
            showModal(modalDialog(
              title = "Error!",
              HTML(paste0(p("Sorry, this plot can't be plotted"))
              ),
              easyClose = TRUE
            ))
          }
        )
        dev.off()
        # Return a list containing the filename
        list(
          src = outfile,
          contentType = "image/png",
          alt = "Upset plot showing relationship between sub-categories" # This is alternate text
        )
      },
      deleteFile = TRUE
    )

    ## Verbatim text table ----
    
    memoised_comment_table <- memoise::memoise(comment_table, cache = session$cache) # create a session-level cacheable version of comment_table()
    output$overlap_table <- DT::renderDT({
      # only run when at least 2 categories are selected
      req(length(global$selected_cats) > 1)
      
      filter_data()$single_labeled_filter_data %>% 
        relationship_table("category", na.omit(global$selected_cats)) %>% 
        memoised_comment_table()
    })
  })
}
