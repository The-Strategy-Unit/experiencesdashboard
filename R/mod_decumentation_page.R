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
    strong("The Qualitative Data Categorisation (QDC) - framework has multiple categories, each with its own set of sub-categories. 
    The category groups similar topics together in a meaningful way to help users navigate the framework  more easily. 
    The sub-categories are the actual topics that better reflect the underlying data.
    A high-level visual of the categories and sub-categories is displayed below:"),
    
    img(src='www/framework_v5.png', width="100%"),
    hr(),
    strong('To see detailed description of the sub-categories, please expand the categories below'),
    
    DT::DTOutput(ns("framework_table")),
    hr(),
    
    tagList(strong("To get further detail about the data categorisation framework and the dashboard 
        including some illustrative examples for each of the sub-categories. 
        Please see the"),  
      a(strong("Patient Experience - QDC documentation Page"), 
        href="https://cdu-data-science-team.github.io/PatientExperience-QDC/framework/framework3.html",
        target="_blank")
    ),
   )
}
    
#' decumentation_page Server Functions
#' @noRd 
mod_decumentation_page_server <- function(id){
  moduleServer( id, function(input, output, session){    
    ns <- session$ns

    # table    
    output$framework_table <- DT::renderDT({
      
      framework <- readxl::read_excel(here::here(app_sys(), "app/www", "FFT-QDC Framework v5 - 20230428.xlsx"),
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
