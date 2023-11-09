# mod_click_tables_server works correctly - user input can be accessed

    Code
      output$comment_table
    Output
      $html
      <pre>
        <strong>No data to show</strong>
      </pre>
      
      $deps
      list()
      

# mod_demographics_selection_server work correctly: CONFIG with demographic feature

    Code
      output$dynamic_demographics_selection
    Output
      $html
      <div id="proxy1-demography_1_UI" class="shiny-html-output"></div>
      <div id="proxy1-demography_2_UI" class="shiny-html-output"></div>
      <div id="proxy1-demography_3_UI" class="shiny-html-output"></div>
      
      $deps
      list()
      

# mod_demographics_selection_server work correctly: CONFIG with no demographic feature

    Code
      output$dynamic_demographics_selection
    Output
      $html
      
      
      $deps
      list()
      

# module server works well if given corrent arguements

    Code
      session$returned
    Output
      <div class="row">
        <a id="proxy1-download_comments" class="btn btn-default shiny-download-link " href="" target="_blank" download>
          <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
          Download data from table
        </a>
        <div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="proxy1-dynamic_comment_table" style="width:100%;height:auto;"></div>
      </div>

# mod_complex_comments_server well if given corrent arguements

    Code
      output$dynamic_complex_ui
    Output
      $html
      <h4>
        <strong>10 complex comments identified</strong>
      </h4>
      <div class="row">
        <a id="proxy1-proxy1-comment_download_1-download_comments" class="btn btn-default shiny-download-link " href="" target="_blank" download>
          <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
          Download data from table
        </a>
        <div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="proxy1-proxy1-comment_download_1-dynamic_comment_table" style="width:100%;height:auto;"></div>
      </div>
      
      $deps
      $deps[[1]]
      List of 9
       $ name      : chr "font-awesome"
       $ version   : chr "6.4.2"
       $ src       :List of 1
        ..$ href: chr "font-awesome-6.4.2"
       $ meta      : NULL
       $ script    : NULL
       $ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      $deps[[2]]
      List of 9
       $ name      : chr "htmlwidgets"
       $ version   : chr "1.6.2"
       $ src       :List of 1
        ..$ href: chr "htmlwidgets-1.6.2"
       $ meta      : NULL
       $ script    : chr "htmlwidgets.js"
       $ stylesheet: NULL
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      $deps[[3]]
      List of 9
       $ name      : chr "datatables-css"
       $ version   : chr "0.0.0"
       $ src       :List of 1
        ..$ href: chr "datatables-css-0.0.0"
       $ meta      : NULL
       $ script    : NULL
       $ stylesheet: chr "datatables-crosstalk.css"
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      $deps[[4]]
      List of 9
       $ name      : chr "datatables-binding"
       $ version   : chr "0.29"
       $ src       :List of 1
        ..$ href: chr "datatables-binding-0.29"
       $ meta      : NULL
       $ script    : chr "datatables.js"
       $ stylesheet: NULL
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi FALSE
       - attr(*, "class")= chr "html_dependency"
      
      $deps[[5]]
      List of 9
       $ name      : chr "jquery"
       $ version   : chr "3.5.1"
       $ src       :List of 1
        ..$ href: chr "jquery-3.5.1"
       $ meta      : NULL
       $ script    : chr "jquery.min.js"
       $ stylesheet: NULL
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      $deps[[6]]
      List of 9
       $ name      : chr "crosstalk"
       $ version   : chr "1.2.0"
       $ src       :List of 1
        ..$ href: chr "crosstalk-1.2.0"
       $ meta      : NULL
       $ script    : chr "js/crosstalk.min.js"
       $ stylesheet: chr "css/crosstalk.min.css"
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      

