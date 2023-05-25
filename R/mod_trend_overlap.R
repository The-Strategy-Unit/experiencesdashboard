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
          data_exists <- filter_data()$filter_data %>%
            dplyr::tally() %>%
            dplyr::pull(n) > 0,
          "Category Trends and inter-relationship plots will appear here"
        )
      )
      
      fluidPage(
        fluidRow(
          uiOutput(ns("trendUI"))
        ),
        tabsetPanel(
          id = ns("tabset_overlap"),
          type = "tabs",
          
          # A Sub-tab
          
          tabPanel("Trend",
                   value = "trend",
                   br(),
                   plotly::plotlyOutput(ns("category_trend_plot")) %>%
                     shinycssloaders::withSpinner()
          ),
          
          # A Sub-tab
          
          tabPanel("Theme Overlap",
                   value = "overlap",
                   br(),
                   fluidRow(
                     column(12,
                            plotOutput(ns("category_upset")) %>%
                              shinycssloaders::withSpinner(),
                            hr(),
                            uiOutput(ns("trendUI_2")),
                            uiOutput(ns("dynamic_overlap_text"))
                     )
                   )
          )
        )
      )
    })
    
    # dynamic ui part ----
    output$trendUI <- renderUI({
      choices <- filter_data()$single_labeled_filter_data$category %>% unique() %>% na.omit()
      
      # select the first 5 category for the trend tab
      ui_list <- list(
        column(6, 
             selectInput(
               session$ns("select_super_category"),
               label = h5(strong("Select Categories (defaults to first 5):")),
               choices = choices,
               multiple = TRUE,
               selected = choices[1:5]
               )
             )
      )
      
      if (input$tabset_overlap == "overlap") {
        
        ui_list <- c(
          ui_list,
          list(
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
        )
      }
      
      return (ui_list)
    })
    
    output$trendUI_2 <- renderUI({
      req(input$tabset_overlap == "overlap")
      
      choices <- c("", filter_data()$single_labeled_filter_data$category %>% unique() %>% na.omit() %>% sort())
      
      fluidRow(
        column(4, selectInput(
          session$ns("select_category1"),
          label = h5(strong("Select category")),
          choices = choices,
        )),
        column(
          4,
          selectInput(
            session$ns("select_category2"),
            label = h5(strong("Select category")),
            choices = choices,
          )
        ),
        column(
          4,
          selectInput(
            session$ns("select_category3"),
            label = h5(strong("Select category")),
            choices = choices,
          )
        ),
      )
    })
    
    # overlap tab UI ----
    
    output$dynamic_overlap_text <- renderUI({
      validate(
        need(
          check_cat_selection(c(input$select_category1, input$select_category2, input$select_category3)),
          "Please select at least two distinct categories to view comments"
        )
      )
      
      selected_cat1 <- input$select_category1
      selected_cat2 <- input$select_category2
      selected_cat3 <- input$select_category3
      
      selected_cats <- unique(c(selected_cat1, selected_cat2, selected_cat3))
      
      global$selected_cats <- selected_cats[selected_cats != ""]
      
      if (length(global$selected_cats) > 1) {
        
        tagList(
          paste(toupper(c(global$selected_cats)), collapse=" | "),
          hr(),
          htmlOutput(ns("overlap_text"))
        )
      }
    })
    
    # tab 1 server codes - category trend ----
    
    trend_data <- reactive({
      return_data <- filter_data()$single_labeled_filter_data %>%
        dplyr::mutate(
          comment_type = factor(comment_type,
                                levels = unique(comment_type),
                                labels = c(
                                  get_golem_config("comment_1"),
                                  get_golem_config("comment_2")
                                )
          )
        ) %>%
        dplyr::mutate(date = as.Date(cut(date, "month"))) %>%
        dplyr::group_by(comment_type, date, category) %>%
        dplyr::summarize(cat = dplyr::n()) %>%
        dplyr::group_by(comment_type, date) %>%
        dplyr::mutate(prop = round(cat / sum(cat), 2)) %>%
        dplyr::filter(!is.na(category))
      
      if (isTruthy(input$select_super_category)) {
        return_data <- return_data %>%
          dplyr::filter(category %in% input$select_super_category)
      }
      
      return_data
    })  %>% 
      bindCache(filter_data()$single_labeled_filter_data, 
                get_golem_config("comment_1"),
                get_golem_config("comment_2"), 
                input$select_super_category)
    
    # trend plot
    
    output$category_trend_plot <- plotly::renderPlotly({
      p <- trend_data() %>%
        ggplot2::ggplot(ggplot2::aes(
          x = date, y = prop,
          color = category, group = category
        )) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::scale_x_date(date_breaks = "4 months", date_labels = "%b %y") +
        ggplot2::scale_y_continuous(label = scales::label_percent(accuracy = 1)) +
        NHSRtheme::scale_colour_nhs() +
        ggplot2::facet_grid(. ~ comment_type) +
        ggplot2::labs(x = NULL, y = "% contribution", color = "Category") +
        add_theme_nhs()
      
      return(
        p %>%
          plotly::ggplotly() %>%
          plotly::config(displayModeBar = FALSE) %>%
          plotly::layout(legend = list(
            title = "",
            orientation = "h",
            y = -0.2,
            xanchor = "center",
            x = 0.5
          ))
      )
    }) %>% 
      bindCache(trend_data())
    
    # tab 2 server codes - the overlapping plot / upset plot ----
    # Create reactive data to be use by the upset plot function and verbatim comment outputs
    tidy_data <- reactive({
      
      filter_data()$single_labeled_filter_data 
    }) %>% 
      bindCache(filter_data()$single_labeled_filter_data)
    
    upset_data <- reactive({
      tidy_data()  %>%
        dplyr::rename(value = category)%>%
        one_hot_labels(column = "value") # apply one hot encoding to the single label column
    }) %>% 
      bindCache(tidy_data())
    
    ## the upset plot ----
    
    output$category_upset <- renderImage({
      
      pixelratio <- session$clientData$pixelratio
      width  <- session$clientData$`output_trend_overlap_ui-category_upset_width`
      height <- session$clientData$`output_trend_overlap_ui-category_upset_height`
      
      # A temp file to save the output. This file will be removed later by renderImage
      # outfile <- tempfile(tmpdir = here::here(app_sys(), "app/www"), fileext='.png')
      outfile <- tempfile(fileext='.png')
      
      png(outfile,
          width=width*pixelratio, height=height*pixelratio*1.03,
          res=120*pixelratio)
      
      tryCatch({
        
        min_size = if (is.numeric(input$min_size)) input$min_size else 1
        intersect <- intersect(input$select_super_category, unique(tidy_data()$category))
        
        upset_plot(upset_data(),
                   intersect = if (length(intersect) > 1) intersect else unique(tidy_data()$category),
                   min_size = as.integer(min_size), 
                   title = "Upset plot showing relationship between Categories"
        ) %>%
          print()
        
      }, error = function(e) {
        print(e)
      })
      dev.off()
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           alt = "Upset plot showing relationship between categories" # This is alternate text
           )
      },
    deleteFile = TRUE)
    
    ## Verbatim text ----
    
    output$overlap_text <- renderText({
      # only run when at least 2 categories are selected
      req(
        length(global$selected_cats) > 1
      )
      
      final_text <- show_multilabeled_text(tidy_data(), "category", na.omit(global$selected_cats))
      
      cat(paste(toupper(c(global$selected_cats)), "|"), " :: ", length(final_text), "\n")
      
      return(final_text)
    }) %>% 
       bindCache(tidy_data(), global$selected_cats)
  })
}
