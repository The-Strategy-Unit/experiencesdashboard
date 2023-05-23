#' decumentation_page UI Function#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_decumentation_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    img(src='framework_v5.png', width="675px", height="457px"),
    hr(),
    DT::DTOutput(ns("framework_table"))
   )
}
    
#' decumentation_page Server Functions
#' @noRd 
mod_decumentation_page_server <- function(id){
  moduleServer( id, function(input, output, session){    
    ns <- session$ns
    
    framework <- readxl::read_excel(here::here(app_sys(), 
                                               "app/www", "FFT-QDC Framework v5 - 20230428.xlsx"),
                                    # "inst/app/www/FFT-QDC Framework v5 - 20230428.xlsx", 
                                    sheet=2) %>% 
      dplyr::arrange(Category, `Sub-category`) %>% 
      dplyr::select(-Examples)
    
    # JaveScript code to collapse the table
    callback_js <-  DT::JS(
      "table.on('click', 'tr.dtrg-group', function () {",
      "  var rowsCollapse = $(this).nextUntil('.dtrg-group');",
      "  $(rowsCollapse).toggleClass('hidden');",
      "});",
      "table.one('init', () => $('.dtrg-group').trigger('click'))"
    )

    # add NHS blue color to the table header
    initComplete <-  DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#005EB8', 'color': '#fff'});",
      "}")
    
    output$framework_table <- DT::renderDT({
      DT::datatable(
        framework,
        extensions = c('RowGroup', 'Buttons'), # required to show the download buttons and groups
        options = list(
          rowGroup = list(dataSrc = 1),
          dom = 'Bt', 
          buttons = c('csv', 'excel', 'pdf'),
          initComplete = initComplete,
          pageLength = 50),
        callback = callback_js,
        class = "display cell-border",
      )
    })
 
  })
}    
