#' complex_comments UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_complex_comments_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("dynamic_complex_tableUI"))|>
    shinycssloaders::withSpinner()
}

#' complex_comments Server Functions
#'
#' @noRd
mod_complex_comments_server <- function(id, filter_data, data_exists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # get the complex comments
    complex_comments <- reactive({
      # data <- prepare_data_management_data(
      #   filter_data()$filter_data,
      #   id , session,
      #   column_names = names(dt_out$display_column_name),
      #   comment_column = "comment_txt",
      #   comment_1 = get_golem_config("comment_1"),
      #   comment_2 = get_golem_config("comment_2")
      # )
      
      data <- filter_data()$filter_data

      return(
        get_complex_comments(data, multilabel_column = "category")
      )
    })


    output$dynamic_complex_tableUI <- renderUI({
      validate(
        need(data_exists, "Complex comment table will appear here")
      )

      tagList(
        colored_h4(strong("Criterial for selecting complex comments")),
        p("Any comment with over 50 words or assigned more than 5 labels by the
          machine learning model is considered complex."),
        hr(),
        uiOutput(ns("dynamic_complex_ui"))|>
          shinycssloaders::withSpinner()
      )
    })

    ## the comments tables ----
    output$comment_table <- renderUI({
    })

    # complex comments ----

    output$dynamic_complex_ui <- renderUI({
      if (nrow(complex_comments()) > 0) {
        n_complex_comments <- complex_comments() |>
          dplyr::pull(comment_txt) |>
          length()


        return_data <- prep_data_for_comment_table(complex_comments(), in_tidy_format = FALSE)

        tagList(
          HTML(paste(n_complex_comments, "complex comments identified") |>
            strong() |> h4() |> paste()),
          mod_comment_download_server(ns("comment_download_1"), return_data, filepath = "complex-data-")
        )
      } else {
        pre(strong("No complex comment identified"))
      }
    })
  })
}