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
        strong("
        This page is only for users who wants to upload new data or amend the
        existing data in the dashboard.
          ") |> p(),
        column(
          width = 1,
          actionButton(ns("upload_new_data-disabled"), "Upload new data",
            icon = icon("person-circle-plus")
          )
        )
      ),
      tags$hr(),
      uiOutput(ns("data_management_UI")) |>
        shinycssloaders::withSpinner()
    )
  )
}

#' data_management Server Functions
#'
#' @noRd
mod_data_management_server <- function(id, db_conn, filter_data, data_exists, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create global variable ####
    dt_out <- reactiveValues(
      data = data.frame(),
      index = list(),
      display_column_name = list(
        "checks" = "Flag row",
        "comment_id" = "Comment ID",
        "date" = "Date",
        "location_1" = get_golem_config("location_1"),
        "location_2" = get_golem_config("location_2"),
        "location_3" = get_golem_config("location_3"),
        "comment_type" = "Question Type",
        "comment_txt" = "Comment",
        "fft" = "FFT Score",
        "category" = "Sub-Category",
        "super_category" = "Category",
        "sentiment" = "Comment Sentiment",
        "sex" = "Sex",
        "gender" = "Gender",
        "age" = "Age Group",
        "ethnicity" = "Ethnicity",
        "sexuality" = "Sexuality",
        "disability" = "Disability",
        "religion" = "Religion",
        "extra_variable_1" = get_golem_config("extra_variable_1"),
        "extra_variable_2" = get_golem_config("extra_variable_2"),
        "extra_variable_3" = get_golem_config("extra_variable_3"),
        "pt_id" = "Responder ID",
        "flagged" = ""
      )
    )

    # dynamic UI ----

    output$data_management_UI <- renderUI({
      validate(
        need(data_exists, "Data Table will appear here")
      )

      # the data ----
      isolate({
        dt_out$data <- prepare_data_management_data(
          filter_data()$filter_data,
          id, session,
          column_names = names(dt_out$display_column_name),
          comment_column = "comment_txt",
          comment_1 = get_golem_config("comment_1"),
          comment_2 = get_golem_config("comment_2")
        )
      })

      # UI ----
      tagList(
        # download UIs
        fluidRow(
          column(12, uiOutput(ns("dynamic_flagged_ui")))
        ),
        hr(),

        # add button for deleting and downloading (all) the data the table
        fluidRow(
          column(
            width = 1,
            actionButton(ns("del_pat-disabled"), "Delete",
              icon = icon("trash-can")
            ),
          ),
          column(
            width = 1,
            downloadButton(ns("download1"), "Download data",
              icon = icon("download")
            )
          )
        ),

        # hint UI
        p(HTML(paste0(
          with_red_stars(strong("To delete row(s): ")), "  Select the row(s)
          and click the delete button.",
          strong(em(" When you are done editing, you will need to refresh your
          browser to pull the edited data into other tabs of the dashboard."))
        ))),
        sprintf(
          'Use  %s  checkbox to flag a row as "interesting" and
            %s to flag it as "wrongly categorised".',
          icon("flag", style = "color:green"),
          icon("circle-xmark", style = "color:red")
        ) |> HTML() |> strong() |>
          paste0("  All 'interesting' rows will be available to download if you
                 refresh your browser") |>
          HTML(),
        hr(),
        # display the table
        fluidRow(
          column(
            width = 12,
            title = "Patient experience table",
            DT::DTOutput(ns("pat_table")) |>
              shinycssloaders::withSpinner()
          )
        )
      )
    })

    # render the data table ####
    output$pat_table <- DT::renderDT({
      columns_to_show <- setdiff(names(dt_out$data), "flagged") # remove the flagged column

      colnames <- unlist(dt_out$display_column_name[columns_to_show], use.names = FALSE)
      stopifnot("lenght of display column name is not the same as number of
                columns in the data" = length(columns_to_show) == length(colnames))

      DT::datatable(
        dplyr::select(dt_out$data, -flagged),
        selection = "multiple",
        rownames = FALSE,
        filter = "top",
        class = "display cell-border compact",
        colnames = colnames,
        escape = FALSE, # ensures HTML entities in the table are properly rendered
        options = list(
          columnDefs = list(
            list("searchable" = FALSE, targets = 0)
          ),
          pageLength = 10,
          lengthMenu = c(10, 30, 50),
          dom = "lrtip",
          search = list(caseInsensitive = FALSE),
          scrollX = TRUE,
          #  to show processing indicator when the DataTable is busy doing some operation that would take some time
          processing = TRUE
        )
      )
    })

    # create a proxy data to track the UI version of the table when edited
    proxy <- DT::dataTableProxy("pat_table")

    # flagged comments ----

    # `input$current_check_info` is from the
    # JavaScript function `get_check_info` (see js_script.js)
    # used in the `add_checkbox_buttons()` function

    ## flag row ----
    observeEvent(input$current_check_info, {
      # only run when the id isn't null and one of the flagged box is clicked
      req(!is.null(input$current_check_info) & stringr::str_detect(input$current_check_info, pattern = "flag"))

      # extracted the comment_id and TRUE/FALSE value from the checkbox
      row <- sub("flag_", "", input$current_check_info["id"])
      check_value <- ifelse(input$current_check_info["value"], 1, 0)

      # for logging
      if (check_value) {
        cat("Comment '", row, "' flagged as interesting \n")
      } else {
        cat("Comment '", row, "' unflagged as interesting \n")
      }

      # # Update the serve data
      # dt_out$data[row,"flagged"] <- check_value

      # Update the database
      query <- glue::glue_sql(
        "UPDATE {`get_golem_config('trust_name')`} SET flagged = {v*} WHERE comment_id IN ({ids*})",
        v = check_value, ids = row, .con = db_conn
      )
      DBI::dbExecute(db_conn, query)
    })

    ## bad row ----
    observeEvent(input$current_check_info, {
      # only run when the id isn't null and one of the bad box is clicked
      req(!is.null(input$current_check_info) & stringr::str_detect(input$current_check_info, pattern = "bad"))

      # extracted the comment_id and TRUE/FALSE value from the checkbox
      row <- sub("bad_", "", input$current_check_info["id"])
      check_value <- ifelse(input$current_check_info["value"], 1, 0)

      # for logging
      if (check_value) {
        cat("Comment '", row, "' flagged as badly coded \n")
      } else {
        cat("Comment '", row, "' unflagged as badly coded \n")
      }

      # Update the database
      query <- glue::glue_sql(
        "UPDATE {`get_golem_config('trust_name')`} SET bad_code = {v*} WHERE comment_id IN ({ids*})",
        v = check_value, ids = row, .con = db_conn
      )
      DBI::dbExecute(db_conn, query)
    })

    # Delete data ####

    deleteData <- reactive({
      # print(input$pat_table_rows_selected) # for debugging and logging

      rowselected <- dt_out$data[input$pat_table_rows_selected, "comment_id"] |> unlist(use.name = FALSE)

      # Instead of actually deleting the rows from the database, we Set the hidden flag to 1 (for all the deleted rows).
      # Only rows with hidden == 0 are loaded into the dashboard. By doing this the data can be recovered if needed
      query <- glue::glue_sql(
        "UPDATE {`get_golem_config('trust_name')`} SET hidden = 1 WHERE comment_id IN ({ids*})",
        ids = rowselected, .con = db_conn
      )
      DBI::dbExecute(db_conn, query)

      # Update the edit date for the deleted rows
      query <- glue::glue_sql("UPDATE {`get_golem_config('trust_name')`} SET last_edit_date = {as.POSIXlt(Sys.time(), tz = 'UTC')} WHERE comment_id IN ({ids*})",
        ids = rowselected, .con = db_conn
      )
      DBI::dbExecute(db_conn, query)

      # update UI
      dt_out$data <- dt_out$data |> dplyr::filter(!comment_id %in% rowselected)
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

    # Download ALL the data ----

    output$download1 <- downloadHandler(
      filename = paste0("pat_data-", Sys.Date(), ".xlsx"),
      content = function(file) {
        withProgress(message = "Downloading...", value = 0, {
          writexl::write_xlsx(prepare_data_for_download(dt_out$data), file)
          incProgress(1)
        })
      }
    )

    # Download rows flagged as interesting ----

    output$dynamic_flagged_ui <- renderUI({
      dt_out$flagged_comments <- dt_out$data |>
        dplyr::filter(flagged == 1) |>
        prepare_data_for_download()

      if (nrow(dt_out$flagged_comments) > 0) {
        n_flagged_comments <- dt_out$flagged_comments |>
          dplyr::pull(comment_txt) |>
          length()

        downloadLink(
          ns("flagged_com"),
          HTML(sprintf('Click here to download the %s comments "flagged as interesting"', n_flagged_comments) |>
            strong() |>
            h4() |>
            paste())
        )
      } else {
        p('
        No comment has been flagged as interesting. If any are flagged, 
        they will be downloadable here.
        ')
      }
    })

    output$flagged_com <- downloadHandler(
      filename = paste0("flagged_comments-", Sys.Date(), ".xlsx"),
      content = function(file) {
        withProgress(message = "Downloading...", value = 0, {
          writexl::write_xlsx(dt_out$flagged_comments, file)
          incProgress(1)
        })
      }
    )

    # data upload module ----

    observe({
      # guess the wait time for sentiment prediction
      api_jobs <- check_api_job(db_conn)
      latest_time <- api_jobs$latest_time
      wait_time <- api_jobs$estimated_wait_time

      if (!is.null(latest_time)) {
        showModal(
          modalDialog(
            title = "Existing Upload!",
            HTML(paste(
              "Sorry, a data upload started at", latest_time, "(GMT) is still uploading. Please allow it to finish uploading (in about",
              wait_time, "mins time) before starting a new upload"
            ))
          )
        )
      } else {
        # create an upload interface
        datamods::import_modal(
          id = ns("myid"),
          from = "file",
          title = "Import data to be used in Dashboard"
        )
      }
    }) |>
      bindEvent(input$upload_new_data)

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
            upload_data(data = raw_df, conn = db_conn, trust_id = get_golem_config("trust_name"), user = user)
            incProgress(1)
          })

          # guess the wait time for sentiment prediction
          api_jobs <- check_api_job(db_conn)
          wait_time <- api_jobs$estimated_wait_time

          showModal(modalDialog(
            title = strong("Success!"),
            HTML(paste(
              h5(paste(nrow(raw_df), "records successfully imported. The dashboard is still processing data to predict category and sentiment")),
              h4(strong(em(paste("Please check back or refresh your browser in about", wait_time, "mins to access the new data"))))
            )),
            easyClose = FALSE
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
