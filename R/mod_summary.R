#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      
      h1("Overview"),
      
      uiOutput(ns("summary_text")),
      
      fluidRow(
        column(width = 1,
        actionButton(ns("launch_modal"), "Upload new data", 
                     icon = icon('person-circle-plus'))
        )
      ),
      tags$br(),
      tags$hr(),
      
      # add button for editing the table
      fluidRow(
        column(
          width = 1,
          actionButton(ns("del_pat"), "Delete",
                       class = "btn-success",
                       # style = "color: #fff;",
                       icon = icon('trash-can')
          ),
        ),
        column(
          width = 1,
          actionButton(ns("save_to_db"), "Save edit",
                       class = "btn-success",
                       icon = icon('save'),
          ),
        )
      ),
      
      tags$br(),
      p("Double click a row to edit its value and press CTRL+ENTER to confirm"),
      # display the table
      fluidRow(
        column(width = 12,
               title = "Patient experience table",
               DT::DTOutput(ns('pat_table')) %>% shinycssloaders::withSpinner()
        )
      )
    )
  )
}

#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id, db_conn, db_data, filter_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # summary
    
    output$summary_text <- renderUI({
      
      n_responses <- db_data %>% 
        dplyr::filter(!is.na(comment_txt)) %>% 
        dplyr::tally() %>% 
        dplyr::pull(n)
      
      n_individuals <- db_data %>% 
        dplyr::distinct(pt_id) %>% 
        dplyr::tally() %>% 
        dplyr::pull(n)
      
      current_responses <- filter_data()$filter_data %>% 
        dplyr::filter(!is.na(comment_txt)) %>% 
        dplyr::tally() %>% 
        dplyr::pull(n)
      
      current_individuals <- filter_data()$filter_data %>% 
        dplyr::distinct(pt_id) %>% 
        dplyr::tally() %>% 
        dplyr::pull(n)
      
      tagList(
        p(glue::glue("There are {n_responses} comments in the database from 
                 {n_individuals} individuals.")),
        
        p(glue::glue("The current selected data comprises {current_responses} 
                   comments in the database from {current_individuals} 
                   individuals."))
      )
    })
    
    # Read data from source e.g. database ####
    dt_out <- reactiveValues(data = db_data  %>% 
                               dplyr::filter(hidden==0) %>% 
                               dplyr::select(-hidden) %>% 
                               dplyr::select(row_id, everything()) %>% 
                               dplyr::as_tibble(),
                             
                             index=list()
                             )
    
    proxy <-  DT::dataTableProxy(ns("pat_table"))
    
    output$pat_table <- DT::renderDT(
      dt_out$data,
      selection = 'multiple',
      rownames = F,
      editable = list('target' = 'row', disable = list(columns = c(0))), # prevent editing of the first n second col
      extensions = 'Buttons',
      options = list(
        pageLength = 10, lengthMenu = c(10, 15, 20, 50),
        dom = 'Blfrtip',
        search = list(caseInsensitive = F),
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = TRUE
      )
    )
    
    # Edit a row and effect it in the UI view ####
    
    observeEvent(input$pat_table_cell_edit, {
      
      info <- input$pat_table_cell_edit
      
      info$value <- sapply(info$value, html_decoder, USE.NAMES = F)
            
      old_dt <- dt_out$data
      tryCatch({
        dt_out$data <- DT::editData(dt_out$data, info, rownames = F)
        
        # Check if any changes was made
        if (!identical(old_dt, dt_out$data)){
          
          # update the UI
          DT::replaceData(proxy, dt_out$data, rownames = F, resetPaging = F)
          
          # track edits
          dt_out$index <- unique(dt_out$index %>% append((info$value[1])))# track row ids that has been edited
          
          cat('Edited rows: ', unlist(dt_out$index), ' \n') # for debugging
          }
        },
        error = function(e) {
          showModal(modalDialog(
            title = "Error!",
            paste(e),
            easyClose = TRUE
          ))
        }
      )
    })

    # Delete data ####
    
    deleteData <- reactive({      
      
      # print(input$pat_table_rows_selected) # for debugging and logging
        
      rowselected <- dt_out$data[input$pat_table_rows_selected, "row_id"] %>%  unlist(use.name=F)
      
      # update datababse
      query <- glue::glue_sql("UPDATE {`get_golem_config('trust_name')`} SET hidden = 1 WHERE row_id IN ({ids*})", 
                              ids = rowselected, .con = db_conn)
      DBI::dbExecute(db_conn, query)
      
      # update UI
      dt_out$data <- dt_out$data %>% dplyr::filter(!row_id %in% rowselected)
      DT::replaceData(proxy, dt_out$data, resetPaging = F)  # update the data on the UI
      
      cat("Deleted Rows: ", rowselected, " \n") # for debugging and logging 
      
      dt_out$index <- setdiff(dt_out$index,rowselected) # remove deleted rows from tracked edited rows
    })

    observeEvent(input$del_pat,{
      
      no_rows <- length(input$pat_table_rows_selected)
      if(no_rows >= 1){
          showModal(modalDialog(
            paste('Are you sure you want to delete these', no_rows, 'rows?'),
            easyClose = T,
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("delete_row"), "Delete", icon = icon('trash')),
            )
          ))
          # input$pat_table_rows_selected = NULL
        } else{
          showModal(
          modalDialog(
            title = "Warning",
            paste("Please select row(s)." ),easyClose = TRUE
          ))
        }
      })
      
      observeEvent(input$delete_row, {
        
        tryCatch({
          deleteData()
          removeModal()
        },
        error = function(e) {
          showModal(modalDialog(
            title = "Error!",
            paste('error in database, please try again and if it persist contact project administrator'),
            easyClose = TRUE
          ))
          print(e)
        })
      })

    # Save (write edited data to source) ####
    
    observeEvent(input$save_to_db, {

      if(length(dt_out$index) < 1){
        
        showModal(modalDialog(
          title = "Error!",
          "There is not edited changes to be save",
          easyClose = TRUE
        ))
      } else {

        showModal(
          modalDialog(
            title = "Save data to database!",
            HTML(paste("Are you sure you want to update", length(dt_out$index), "record(s) of data?")),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("update_source"), "Update", icon = icon('database')),
            )
          )
        )
      }
    })
        
    observeEvent(input$update_source, {
      
      tryCatch({
        #update the database  
        trust_db <- dplyr::tbl(db_conn, get_golem_config('trust_name'))
        dplyr::rows_update(trust_db, dt_out$data %>% dplyr::filter(row_id %in% unlist(dt_out$index)), 
                           by = 'row_id', copy = TRUE, unmatched = 'ignore', in_place = TRUE)
        
        showModal(modalDialog(
              title = "Success!",
              p(paste("Record of", length(dt_out$index), "Patient(s) have been successfully updated.")),
              em("Please refresh your browser to visualise the update"),
              easyClose = TRUE
            ))
        
        dt_out$index=list()
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Error!",
          paste("There was a problem accessing the database. Please try again"),
          easyClose = TRUE
          ))
        print(e)
        }
      )
    })
    
    # data module

    observeEvent(input$launch_modal, {
      
      datamods::import_modal(
        id = session$ns("myid"),
        from = "file",
        title = "Import data to be used in application"
      )
    })

    imported <- datamods::import_server("myid", return_class = "tbl_df")

    observe({
      
      req(imported$data())

      raw_df <- imported$data() %>%
        dplyr::mutate(pt_id = dplyr::row_number())

      withProgress(message = 'Processing data. This may take a while.
                   Please wait...', value = 0, {

                     success <- T # upload_data(data = raw_df, conn = db_conn, trust_id = get_golem_config("trust_name"))

                     incProgress(1)
                   })

      if(success){

        showModal(modalDialog(
          title = "Success!",
          paste0(nrow(raw_df), " records successfully imported. Please refresh
               your browser to access the new data"),
          easyClose = TRUE
        ))
      } else {

        showModal(modalDialog(
          title = "Error!",
          "There was a problem importing your data. Try reuploading and check
          in the 'View' section to ensure that the data is well formatted",
          easyClose = TRUE
        ))
      }
    })
  })
}
