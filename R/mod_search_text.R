#' search_text UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_search_text_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      colored_h4(strong("Appropriate use of the comment search")),
      p("
      Before searching for comments on a specific topic, first identify whether 
      a relevant sub-category already exists in the categorisation framework. 
      If it does you should look at the comments categorised there without need 
      to use the comment search. Word searches are a simplistic approach which 
      can miss a lot of relevant data, whereas the sophisticated categorisation 
      approach used within this tool has much better results. The comment search
      is best utilised when you have a very narrow focus of interest and there 
      is a small consistent vocabulary around the topic.
      "),
      colored_h4(strong("How search is done")),
      p('The search uses Boolean techniques to find all provided words, 
      including their singular and plural forms. For instance, if you input 
      "staff, doctors", it will also search for "staffs, doctor" and so on.".
        '),
      textInput(ns("text_search"), "Search term(s) - Add multiple search terms with comma",
        placeholder = "e.g. staff, doctor, nurse"
      ),
      hr(),
      uiOutput(ns("dynamic_comment_ui"))
    )
  )
}

#' search_text Server Functions
#'
#' @noRd
mod_search_text_server <- function(id, filter_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$dynamic_comment_ui <- renderUI({
      validate(
        need(text_search(), "Please enter a search term")
      )
      req(return_data())

      tagList(
        uiOutput(ns("comment_output")) |>
          shinycssloaders::withSpinner()
      )
    })

    text_search <- reactive({
      if (is.null(input$text_search)) {
        NULL
      } else {
        input$text_search
      }
    }) |> debounce(1000)

    return_data <- reactive({
      req(text_search())

      return_search_text(
        text_data = filter_data()$filter_data,
        filter_text = text_search(),
        comment_type_filter = NULL, search_type = "and"
      ) |>
        dplyr::mutate(across(c(category, super_category), ~ purrr::map(.x, jsonlite::fromJSON)),
          super_category = lapply(super_category, unique), # to remove the duplicate values from each super category row
          across(c(category, super_category), ~ purrr::map(.x, to_string))
        ) |>
        prep_data_for_comment_table(in_tidy_format = FALSE)
    })

    ## the comments tables ----
    output$comment_output <- renderUI({
      mod_comment_download_server(
        ns("comment_download_1"),
        return_data(),
        filepath = input_sanitizer(text_search()) |>
          paste(collapse = "_") |>
          paste0("-")
      )
    })
  })
}
