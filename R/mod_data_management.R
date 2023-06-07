#' data_management UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_management_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$br(),
      fluidRow(
        column(
          width = 1,
          actionButton(ns("upload_new_data"), "Upload new data",
            icon = icon("person-circle-plus")
          )
        )
      ),
      tags$hr(),
      uiOutput(ns("data_management_UI"))
    )
  )
}

#' data_management Server Functions
#'
#' @noRd
mod_data_management_server <- function(id, db_conn, filter_data, data_exists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create global variable ####
    dt_out <- reactiveValues(
      data = data.frame(),
      index = list(),
      column_names = c(
        "comment_id", "date", "location_1", "location_2", "location_3",
        "comment_type", "comment_txt", "category", "super_category", "fft",
        "gender", "age", "ethnicity", "sexuality", "disability", "religion",
        "extra_variable_1", "extra_variable_2", "extra_variable_3",
        "pt_id"
      ),
      complex_comments = data.frame(),
      display_column_name = list(
        "comment_id" = "Comment ID",
        "date" = "Date",
        "location_1" = get_golem_config("location_1"),
        "location_2" = get_golem_config("location_2"),
        "location_3" = get_golem_config("location_3"),
        "comment_type" = "Question Type",
        "comment_txt" = "Comment",
        "category" = "Sub-Category",
        "super_category" = "Category",
        "fft" = "FFT Score",
        "gender" = "Gender",
        "age" = "Age Group",
        "ethnicity" = "Ethnicity",
        "sexuality" = "Sexuality",
        "disability" = "Disability",
        "religion" = "Religion",
        "extra_variable_1" = get_golem_config("extra_variable_1"),
        "extra_variable_2" = get_golem_config("extra_variable_2"),
        "extra_variable_3" = get_golem_config("extra_variable_3"),
        "pt_id" = "Patient ID"
      )
    )

    # dynamic UI ----

    output$data_management_UI <- renderUI({
      
      validate(
        need(data_exists, "Data Table will appear here")
      )
      
      isolate({
        dt_out$data <- dm_data(
          filter_data()$filter_data,
          column_names = dt_out$column_names,
          comment_1 = get_golem_config("comment_1"),
          comment_2 = get_golem_config("comment_2")
        )
      })

      # UI ----
      tagList(
        # add button for editing the table
        fluidRow(
          column(
            width = 1,
            actionButton(ns("del_pat"), "Delete",
              icon = icon("trash-can")
            ),
          ),
          # column(
          #   width = 1,
          #   actionButton(ns("save_to_db"), "Save edit",
          #     icon = icon("save"),
          #   ),
          # ),
          column(
            width = 1,
            downloadButton(ns("download1"), "Download data",
              icon = icon("download")
            )
          )
        ),
        tags$br(),

        # UI complex comment

        fluidRow(
          column(12, uiOutput(ns("dynamic_complex_ui")))
        ),
        # p(strong("To edit any row:"), "Double click the row, edit its value and press CTRL+ENTER to confirm"),

        # display the table

        fluidRow(
          column(
            width = 12,
            title = "Patient experience table",
            DT::DTOutput(ns("pat_table")) %>% 
              shinycssloaders::withSpinner()
          )
        )
      )
    })
    
    # render the data table ####
    output$pat_table <- DT::renderDT({
      
      DT::datatable(
        dt_out$data,
        selection = "multiple",
        rownames = FALSE,
        # editable = list(
        #   "target" = "row",
        #   disable = list(columns = c(0,5,length(names(dt_out$data))-1)) # disable editing of comment_id (0), comment_type(5), n pat_id (last column) cols
        #   ),
        filter = "top",
        class = "display cell-border compact",
        colnames = unlist(dt_out$display_column_name[names(dt_out$data)], use.name = FALSE),
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 30, 50),
          dom = "lrtip",
          search = list(caseInsensitive = FALSE),
          scrollX = TRUE
        )
      )
    })
    
    # create a proxy data to track the UI version of the table when edited
    proxy <- DT::dataTableProxy(ns("pat_table"))

    # Edit a row and effect it in the UI view ####
    observeEvent(input$pat_table_cell_edit, {
      info <- input$pat_table_cell_edit # get the edited row information

      info$value <- sapply(info$value, html_decoder, USE.NAMES = FALSE) # decode any html character introduced to the values

      old_dt <- dt_out$data # Track the initial state of the data before recording user changes

      tryCatch(
        {
          dt_out$data <- DT::editData(dt_out$data, info, rownames = FALSE)

          # Data Validation

          check_list <- list()
          #  column index of columns to check (all columns aside "comment_id" = 1,
          # "date" = 2, "comment_type" = 6, "comment_txt" = 7, "pt_id" = last column index)
          # column mapping can be gotten from global variable {dt_out$column_names}
          len_col <- 1:(length(names(dt_out$data)) - 1)
          column_to_check <- setdiff(len_col, c(1, 2, 6, 7))

          # check if the value entered for each column in column_to_check is part of the existing unique values
          # of that column  or empty string
          for (i in column_to_check) {
            check <- info$value[i] %in% c(unique(old_dt[[dt_out$column_names[i]]]), "")
            check_list <- check_list %>% append(check)
          }

          check_list <- unlist(check_list)
          error_columns <- dt_out$column_names[column_to_check[!check_list]]
          # cat('columns to check: ', dt_out$column_names[column_to_check], ' \n') # for debugging

          # Ignore changes if enter value in some columns are not part of the  existing unique values in its column

          if (!all(check_list)) {
            cat("columns with error in edited row: ", error_columns, " \n") # for debugging

            dt_out$data <- old_dt

            showModal(modalDialog(
              title = "Error!",
              paste("Value(s) entered in", paste(error_columns, collapse = " and "), "column(s) is not part of existing values in that column(s)"),
              easyClose = TRUE
            ))
          }

          # if any allowed changes was made to the data then update the UI data
          cat("is UI and server data identical?", identical(old_dt, dt_out$data), "\n")

          if (!identical(old_dt, dt_out$data)) {
            DT::replaceData(proxy, dt_out$data, rownames = FALSE, resetPaging = FALSE)

            # track edits
            dt_out$index <- unique(dt_out$index %>% append((info$value[1]))) # track row ids that has been edited

            cat("Edited rows: ", unlist(dt_out$index), " \n") # for debugging
            print(dt_out$data %>% dplyr::filter(comment_id == dt_out$index))
          }
        },
        error = function(e) {
          showModal(modalDialog(
            title = "Error!",
            paste(e, "\n\nPlease correct your changes"),
            easyClose = TRUE
          ))
        }
      )
    })

    # Delete data ####

    deleteData <- reactive({
      # print(input$pat_table_rows_selected) # for debugging and logging

      rowselected <- dt_out$data[input$pat_table_rows_selected, "comment_id"] %>% unlist(use.name = FALSE)

      # update database
      query <- glue::glue_sql("UPDATE {`get_golem_config('trust_name')`} SET hidden = 1 WHERE comment_id IN ({ids*})",
        ids = rowselected, .con = db_conn
      )
      DBI::dbExecute(db_conn, query)

      # update UI
      dt_out$data <- dt_out$data %>% dplyr::filter(!comment_id %in% rowselected)
      DT::replaceData(proxy, dt_out$data, resetPaging = FALSE) # update the data on the UI

      cat("Deleted Rows: ", rowselected, " \n") # for debugging and logging

      dt_out$index <- setdiff(dt_out$index, rowselected) # remove deleted rows from tracked edited rows
    })

    observeEvent(input$del_pat, {
      no_rows <- length(input$pat_table_rows_selected)
      if (no_rows >= 1) {
        showModal(modalDialog(
          paste("Are you sure you want to delete these", no_rows, "rows?"),
          easyClose = T,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("delete_row"), "Delete", icon = icon("trash")),
          )
        ))
        # input$pat_table_rows_selected = NULL
      } else {
        showModal(
          modalDialog(
            title = "Warning",
            paste("Please select row(s)."), easyClose = TRUE
          )
        )
      }
    })

    observeEvent(input$delete_row, {
      tryCatch(
        {
          deleteData()
          removeModal()
        },
        error = function(e) {
          showModal(modalDialog(
            title = "Error!",
            paste("error in database, please try again and if it persist contact project administrator"),
            easyClose = TRUE
          ))
          print(e)
        }
      )
    })

    # Save (write edited data to source) ####

    observeEvent(input$save_to_db, {
      if (length(dt_out$index) < 1) {
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
              actionButton(ns("update_source"), "Update", icon = icon("database")),
            )
          )
        )
      }
    })

    observeEvent(input$update_source, {
      tryCatch(
        {
          # update the database
          trust_db <- dplyr::tbl(db_conn, get_golem_config("trust_name"))
          dplyr::rows_update(trust_db, dt_out$data %>% dplyr::filter(comment_id %in% unlist(dt_out$index)),
            by = "comment_id", copy = TRUE, unmatched = "ignore", in_place = TRUE
          )

          showModal(modalDialog(
            title = "Success!",
            p(paste("Record of", length(dt_out$index), "Patient(s) have been successfully updated.")),
            em("Please refresh your browser to visualise the update"),
            easyClose = TRUE
          ))

          dt_out$index <- list()
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

    # Download the data ####

    output$download1 <- downloadHandler(
      filename = paste0("pat_data-", Sys.Date(), ".xlsx"),
      content = function(file) {
        withProgress(message = "Downloading...", value = 0, {
          writexl::write_xlsx(dt_out$data, file)
          incProgress(1)
        })
      }
    )

    # complex comments ----

    output$dynamic_complex_ui <- renderUI({
      
      # complex comments ----
      dt_out$complex_comments <- get_complex_comments(dt_out$data, multilabel_column = "category")
      
      if ((nrow(dt_out$complex_comments) > 1)) {
        n_complex_comments <- dt_out$complex_comments |>
          dplyr::pull(comment_txt) |>
          length()

        downloadLink(
          ns("complex_com"),
          HTML(paste(n_complex_comments, "complex comments identified. click here to download them") %>%
            strong() %>% h4() %>% paste())
        )
      }
    })

    output$complex_com <- downloadHandler(
      filename = paste0("complex_comments-", Sys.Date(), ".xlsx"),
      content = function(file) {
        withProgress(message = "Downloading...", value = 0, {
          writexl::write_xlsx(dt_out$complex_comments, file)
          incProgress(1)
        })
      }
    )

    # data upload module ----

    observeEvent(input$upload_new_data, {
      # create an upload interface

      datamods::import_modal(
        id = session$ns("myid"),
        from = "file",
        title = "Import data to be used in Dashboard"
      )
    })

    # Get the imported data as tibble

    tryCatch(
      import_dt <- datamods::import_server("myid", return_class = "tbl_df"),
      error = function(e) {
        print(e)

        showModal(modalDialog(
          title = "Error!",
          "There was a problem importing your data. Try reuploading and check
          in the 'View' section to ensure that the data is well formatted",
          easyClose = TRUE
        ))
      }
    )

    observe({
      req(import_dt$data())

      raw_df <- import_dt$data()
      # print(str(raw_df))

      compulsory_cols <- c("date", "location_1", "question_1", "fft_score")

      tryCatch(
        {
          if (!all(compulsory_cols %in% names(raw_df))) {
            stop("the following columns are required [date, location_1, question_1, fft_score]", call. = FALSE)
          }

          withProgress(message = "Processing data. This may take a while.
                     Please wait...", value = 0, {
            upload_data(data = raw_df, conn = db_conn, trust_id = get_golem_config("trust_name"))
            incProgress(1)
          })


          showModal(modalDialog(
            title = "Success!",
            paste0(nrow(raw_df), " records successfully imported. Please refresh
             your browser to access the new data"),
            easyClose = TRUE
          ))
        },
        error = function(e) {
          print(e$message) # for logging error

          # try to guess the error type to improve user experience

          col_error <- stringr::str_detect(e$message, "the following columns are required")
          api_error <- stringr::str_detect(e$message, "Connection refused")
          db_error <- stringr::str_detect(e$message, "dbWriteTable|nanodbc/nanodbc")

          if (db_error) {
            showModal(modalDialog(
              title = "Database Error!",
              "Please try again or contact project admin if error persist",
              easyClose = TRUE
            ))
          } else if (col_error) {
            showModal(modalDialog(
              title = "Data Column Error!",
              paste(e),
              easyClose = TRUE
            ))
          } else if (api_error) {
            showModal(modalDialog(
              title = "API Error!",
              "Please try again or contact project admin if error persist",
              easyClose = TRUE
            ))
          } else {
            showModal(modalDialog(
              title = "Data Error!",
              "There was a problem importing your data. Try reuploading and check
            in the 'View' section to ensure that the data is well formatted",
              easyClose = TRUE
            ))
          }
        }
      )
    })
  })
}
