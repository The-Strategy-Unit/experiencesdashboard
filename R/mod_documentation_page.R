#' documentation_page UI Function#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_documentation_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    p("This dashboard utilizes the pxtextmining API, a machine learning tool, to assign one or more subcategories to free-text comments
      based on the Qualitative Data Categorization (QDC) framework. The QDC framework is an evidence-based work that has been designed
      with several categories, each with its own set of subcategories. The categories group similar topics together to make it easier 
      for users to navigate the framework, while the subcategories reflect the actual topics that better represent the underlying data."),
    p("The dashboard's visualizations and intuitive interactivity are thoughtfully created to help users effectively engage with the 
      comments and not merely quantify the data. Below is a high-level visual of the categories and subcategories:"),
    img(src = "www/framework_MVP_version.jpeg", width = "100%"),
    hr(),
    p("To see detailed description of the sub-categories, kindly click on the category to expand it."),
    DT::DTOutput(ns("framework_table")),
    hr(),
    tagList(
      p("To get further detail about the data categorisation framework and the dashboard
        including some illustrative examples for each of the sub-categories.
        Please see the"),
      a(p("Patient Experience - QDC documentation Page"),
        href = "https://cdu-data-science-team.github.io/PatientExperience-QDC/framework/framework3.html",
        target = "_blank"
      )
    ),
  )
}

#' documentation_page Server Functions
#' @noRd
mod_documentation_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # table
    output$framework_table <- DT::renderDT({

      # JaveScript code to collapse the table
      callback_js <- DT::JS(
        "table.on('click', 'tr.dtrg-group', function () {",
        "  var rowsCollapse = $(this).nextUntil('.dtrg-group');",
        "  $(rowsCollapse).toggleClass('hidden');",
        "});",
        "table.one('init', () => $('.dtrg-group').trigger('click'))"
      )

      # add NHS blue color to the table header
      initComplete <- DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#005EB8', 'color': '#fff'});",
        "}"
      )

      DT::datatable(
        framework,
        extensions = c("RowGroup", "Buttons"), # required to show the download buttons and groups
        options = list(
          rowGroup = list(dataSrc = 1),
          dom = "Bt",
          buttons = c("csv", "excel", "pdf"),
          initComplete = initComplete,
          pageLength = 50
        ),
        callback = callback_js,
        class = "display cell-border",
      )
    })
  })
}
