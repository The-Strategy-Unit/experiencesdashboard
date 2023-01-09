library(shiny)
library(tidyverse, warn.conflicts=F)
library(DT)
library(shinydashboard)

# UI
ui <- function(){
  tagList(
    dashboardPage(
      # header (title)
      dashboardHeader(title = "PAT table",
                      titleWidth = 300),

      # sidebar
      shinydashboard::dashboardSidebar(
        shinyFeedback::useShinyFeedback(),
        shinyjs::useShinyjs(),
        sidebarMenu(


          menuItem('maininput',
                   tabName = 'tabs',
                   icon = shiny::icon("comment"),
                   selected = TRUE,
                   badgeLabel = "dev", badgeColor = "blue")
        )

      ),

      # body
      shinydashboard::dashboardBody(
        tabsetPanel(
          # id='tabs',
          tabPanel('tab1',
                   tagList(
                     tags$br(),
                     # add click for adding table
                     fluidRow(
                       column(
                         width = 2,
                         actionButton(
                           "upload_data",
                           "Upload data",
                           class = "btn-success",
                           style = "color: #fff;",
                           icon = icon('person-circle-plus'),
                           width = '100%'
                         ),
                         tags$br(),
                         tags$br()
                       ),

                       column(
                         width = 2,
                         actionButton(
                           "del_pat",
                           "Delete",
                           class = "btn-success",
                           style = "color: #fff;",
                           icon = icon('trash-can'),
                           width = '100%'
                         ),
                         tags$br(),
                         tags$br()
                       ),

                       column(
                         width = 2,
                         actionButton(
                           "save_to_db",
                           "Save to database",
                           class = "btn-success",
                           style = "color: #fff;",
                           icon = icon('save'),
                           width = '100%'
                         ),
                         tags$hr(),
                         tags$br()
                       )
                     ),
                     # display the table
                     fluidRow(
                       column(
                         width = 12,
                         title = "Motor Trend Car Road Tests",
                         DTOutput('pat_table') %>%
                           shinycssloaders::withSpinner(),
                         tags$br(),
                         tags$br()
                       )
                     ),
                   )

          ),
          tabPanel('tab2',
                   DTOutput('uploaded_dt') %>%
                     shinycssloaders::withSpinner(7)    # show loading sign until data is correctly uploaded
          ),
        )
      ),
      skin = 'red'
    )
  )
}

# SERVER
server <- function(
    input,
    output,
    session
){
  # Read data from source e.g. database ####
  dt_out <- reactiveValues(data = read.csv(here::here('data/p_data.csv')), noedit=0)
  proxy <-  dataTableProxy("pat_table")

  # isolate(
    # req(dt_out$data)
    # rownames(dt_out$data) <-  uuid::UUIDgenerate(n=nrow(dt_out$data)) # add unique identifier that can be used to identify each row.
     # )

  reactive (str(dt_out$data))
  output$pat_table <- renderDT(
    dt_out$data, #%>% select(-uid),
    selection = 'multiple',
    # rownames = F,
    # editable = 'row',
    editable = list('target' = 'row', disable = list(columns = c(0))), # prevent editing of the first n second col
    extensions = 'Buttons',
    options = list(
      pageLength = 10, lengthMenu = c(10, 15, 20, 50),
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )

  # Edit a row and effect it in the UI view ####
  observeEvent(input$pat_table_cell_edit, {

    # for debugging and logging
    # print(names(dt_out$data))
    info = input$pat_table_cell_edit
    print(info)
    i = info$row
    j = info$col
    k = info$value

    t = dt_out$data[unique(i),] %>%  unlist(use.name=F) %>% replace_na('')
    t2 = unlist(k, use.name=F)[-1]

    # track the numbers of rows that has been edited
    if (sum(t2 == t)!=length(colnames(dt_out$data))){
      dt_out$noedit = dt_out$noedit + 1
    }

    isolate(
      dt_out$data <- editData(dt_out$data, info, 'pat_table', resetPaging=T)
    )
    })


  # delete data ####
  deleteData <- reactive({

    # isolate({
      # row_selection <- dt_out$data[input$pat_table_rows_selected, "uid"]
      # dt_out$data <- dt_out$data %>% filter(!uid %in% row_selection) # update the data
    # })

    print(input$pat_table_rows_selected)
    isolate(
      dt_out$data <<- dt_out$data %>% filter(!rownames(.) %in% input$pat_table_rows_selected)
    )
    replaceData(proxy, dt_out$data, resetPaging = F)  # update the data on the UI
   })

  observeEvent(input$del_pat, priority = 20,{
    showModal(
      if(length(input$pat_table_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      } else{
        modalDialog(
          paste('Are you sure you want to delete', length(input$pat_table_rows_selected), 'rows?'),
          easyClose = F,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("delete_row", "Delete", icon = icon('trash')),
          )
        )
      })
    observeEvent(input$delete_row, {
      deleteData()
      removeModal()
    })
  })

  # SAVE (write edited data to source) ####
  observeEvent(input$save_to_db, {

    req(input$save_to_db)

    if((dt_out$noedit < 1)){
     showModal(modalDialog(
        title = "Error!",
        "There is not changes made to be save to the database",
        easyClose = TRUE
      ))

    } else {

      showModal(
        modalDialog(
          title = "Save data to database!",
          HTML("Are you sure you want to overwrite existing data with this newly created data?"),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("update_source", "Update", icon = icon('database')),
          )
        )
      )
      observeEvent(input$update_source, {
        # success <- DBI::dbWriteTable(conn, trust_id,
        #                              final_df, append = TRUE)
        write.csv(dt_out$data, here::here('data/data_output.csv'), row.names = F)  #*
      removeModal()
      dt_out$noedit = 0
      })
    }
  })

  # upload data - a stand alone functionality ####


  observeEvent(input$upload_data, {
    # create an upload interface
    datamods::import_modal(
      id = "myid",
      from = "file",
      title = "Import data to be used in application"
    )

    # gets the data uploaded
    import_dt <- datamods::import_server("myid", return_class = "tbl_df")


    # run this only if data has been uploaded
    observe({
      req(import_dt$data())

      raw_df <- import_dt$data()%>%
        dplyr::mutate(uid = uuid::UUIDgenerate(nrow(import_dt$data())))

      # do something to the data (e.g. print the data to dashboard)
      output$uploaded_dt <- renderDT({
        raw_df

        # if(success){
        #
        #   showModal(modalDialog(
        #     title = "Success!",
        #     paste0(nrow(raw_df), " records successfully imported. Please refresh
        #        your browser to access the new data"),
        #     easyClose = TRUE
        #   ))
        # } else {
        #
        #   showModal(modalDialog(
        #     title = "Error!",
        #     "There was a problem importing your data. Try reuploading and check
        #   in the 'View' section to ensure that the data is well formatted",
        #     easyClose = TRUE
        #   ))
        # }
      })
    })
  })
}
shinyApp(ui, server)



# USEFUL

# df = read.csv(here::here('data/p_data.csv'))
# options(DT.options = list(pageLength = 5))
# addRow(proxy, data, resetPaging = TRUE)
