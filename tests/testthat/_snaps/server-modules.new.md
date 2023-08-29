# mod_click_tables_server set's up sub-category datatable and dynamic_click_tableUI correctly

    Code
      output$dynamic_click_tableUI
    Output
      $html
      <div class="container-fluid">
        <!--SHINY.SINGLETON[3891ac171834ed25f436995ba40a47edd74e5169]-->
        <head>
          <link rel="stylesheet" href="shinycssloaders-assets/spinner.css"/>
          <script src="shinycssloaders-assets/spinner.js"></script>
        </head>
        <!--/SHINY.SINGLETON[3891ac171834ed25f436995ba40a47edd74e5169]-->
        <!--SHINY.SINGLETON[45534f2c59c7069db5da7f5118dc12c792e86de5]-->
        <head>
          <link rel="stylesheet" href="shinycssloaders-assets/css-loaders.css"/>
        </head>
        <!--/SHINY.SINGLETON[45534f2c59c7069db5da7f5118dc12c792e86de5]-->
        <head>
          <style>#spinner-9979a7e2a4bb3e14eec3c77c8031e9f0, #spinner-9979a7e2a4bb3e14eec3c77c8031e9f0:before, #spinner-9979a7e2a4bb3e14eec3c77c8031e9f0:after {   background: #0275D8; } #spinner-9979a7e2a4bb3e14eec3c77c8031e9f0 {   color: #0275D8; } #spinner-9979a7e2a4bb3e14eec3c77c8031e9f0 { font-size: 8px; }</style>
        </head>
        <div class="shiny-spinner-output-container shiny-spinner-hideui ">
          <div class="load-container shiny-spinner-hidden load1">
            <div id="spinner-9979a7e2a4bb3e14eec3c77c8031e9f0" class="loader">Loading...</div>
          </div>
          <div style="height:400px" class="shiny-spinner-placeholder"></div>
          <div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="proxy1-table" style="width:100%;height:auto;"></div>
        </div>
        <hr/>
        <h5>Please select a Sub-category from the table above in other to drill down the table below</h5>
        <a id="proxy1-click_table_download_data" class="btn btn-default shiny-download-link " href="" target="_blank" download>
          <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
          Download data
        </a>
        <div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="proxy1-comment_table" style="width:100%;height:auto;"></div>
      </div>
      
      $deps
      $deps[[1]]
      List of 9
       $ name      : chr "jquery"
       $ version   : chr "3.6.0"
       $ src       :List of 1
        ..$ href: chr "jquery-3.6.0"
       $ meta      : NULL
       $ script    : chr "jquery.min.js"
       $ stylesheet: NULL
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi FALSE
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
       $ version   : chr "0.28"
       $ src       :List of 1
        ..$ href: chr "datatables-binding-0.28"
       $ meta      : NULL
       $ script    : chr "datatables.js"
       $ stylesheet: NULL
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi FALSE
       - attr(*, "class")= chr "html_dependency"
      
      $deps[[5]]
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
      
      $deps[[6]]
      List of 9
       $ name      : chr "font-awesome"
       $ version   : chr "6.4.0"
       $ src       :List of 1
        ..$ href: chr "font-awesome-6.4.0"
       $ meta      : NULL
       $ script    : NULL
       $ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      $deps[[7]]
      List of 9
       $ name      : chr "bootstrap"
       $ version   : chr "3.4.1"
       $ src       :List of 1
        ..$ href: chr "bootstrap-3.4.1"
       $ meta      :List of 1
        ..$ viewport: chr "width=device-width, initial-scale=1"
       $ script    : chr [1:2] "js/bootstrap.min.js" "accessibility/js/bootstrap-accessibility.min.js"
       $ stylesheet: chr [1:2] "css/bootstrap.min.css" "accessibility/css/bootstrap-accessibility.min.css"
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      

# mod_click_tables_server works correctly

    Code
      output$comment_table
    Output
      {"x":{"filter":"none","vertical":false,"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>comment_id<\/th>\n      <th>comment_type<\/th>\n      <th>date<\/th>\n      <th>pt_id<\/th>\n      <th>location_1<\/th>\n      <th>location_2<\/th>\n      <th>location_3<\/th>\n      <th>comment_txt<\/th>\n      <th>fft<\/th>\n      <th>sex<\/th>\n      <th>gender<\/th>\n      <th>age<\/th>\n      <th>ethnicity<\/th>\n      <th>sexuality<\/th>\n      <th>disability<\/th>\n      <th>religion<\/th>\n      <th>extra_variable_1<\/th>\n      <th>extra_variable_2<\/th>\n      <th>extra_variable_3<\/th>\n      <th>hidden<\/th>\n      <th>last_upload_date<\/th>\n      <th>category<\/th>\n      <th>super_category<\/th>\n      <th>sentiment<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,4,9,20,24]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"serverSide":true,"processing":true},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[],"deps":[{"name":"jquery","version":"3.6.0","src":{"href":"jquery-3.6.0"},"meta":null,"script":"jquery-3.6.0.min.js","stylesheet":null,"head":null,"attachment":null,"all_files":true},{"name":"dt-core","version":"1.13.4","src":{"href":"dt-core-1.13.4"},"meta":null,"script":"js/jquery.dataTables.min.js","stylesheet":["css/jquery.dataTables.min.css","css/jquery.dataTables.extra.css"],"head":null,"attachment":null,"package":null,"all_files":false},{"name":"crosstalk","version":"1.2.0","src":{"href":"crosstalk-1.2.0"},"meta":null,"script":"js/crosstalk.min.js","stylesheet":"css/crosstalk.min.css","head":null,"attachment":null,"all_files":true}]} 

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
      

# mod_summary_record_server works correctly

    Code
      output$dynamic_summary_record
    Output
      $html
      <div class="row">
        <div class="col-sm-3">
          <div class="shiny-html-output col-sm-" id="proxy1-commentBox"></div>
        </div>
        <div class="col-sm-3">
          <div class="shiny-html-output col-sm-" id="proxy1-individualBox"></div>
        </div>
        <div class="col-sm-3">
          <div class="shiny-html-output col-sm-" id="proxy1-current_commentBox"></div>
        </div>
        <div class="col-sm-3">
          <div class="shiny-html-output col-sm-" id="proxy1-current_individualBox"></div>
        </div>
      </div>
      
      $deps
      list()
      

---

    Code
      output$commentBox
    Output
      $html
      <div class="small-box bg-light-blue">
        <div class="inner">
          <h3>1,981</h3>
          <p>
            <p style="font-size: 90%">Comments in Database</p>
          </p>
        </div>
        <div class="icon-large">
          <i class="far fa-comment" role="presentation" aria-label="comment icon"></i>
        </div>
      </div>
      
      $deps
      $deps[[1]]
      List of 9
       $ name      : chr "font-awesome"
       $ version   : chr "6.4.0"
       $ src       :List of 1
        ..$ href: chr "font-awesome-6.4.0"
       $ meta      : NULL
       $ script    : NULL
       $ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      

---

    Code
      output$individualBox
    Output
      $html
      <div class="small-box bg-light-blue">
        <div class="inner">
          <h3>1,000</h3>
          <p>
            <p style="font-size: 90%">Responders in Database</p>
          </p>
        </div>
        <div class="icon-large">
          <i class="fas fa-users" role="presentation" aria-label="users icon"></i>
        </div>
      </div>
      
      $deps
      $deps[[1]]
      List of 9
       $ name      : chr "font-awesome"
       $ version   : chr "6.4.0"
       $ src       :List of 1
        ..$ href: chr "font-awesome-6.4.0"
       $ meta      : NULL
       $ script    : NULL
       $ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      

---

    Code
      output$current_commentBox
    Output
      $html
      <div class="small-box bg-light-blue">
        <div class="inner">
          <h3>10</h3>
          <p>
            <p style="font-size: 90%">Comments in Current Selection</p>
          </p>
        </div>
        <div class="icon-large">
          <i class="far fa-comment-dots" role="presentation" aria-label="comment-dots icon"></i>
        </div>
      </div>
      
      $deps
      $deps[[1]]
      List of 9
       $ name      : chr "font-awesome"
       $ version   : chr "6.4.0"
       $ src       :List of 1
        ..$ href: chr "font-awesome-6.4.0"
       $ meta      : NULL
       $ script    : NULL
       $ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      

---

    Code
      output$current_individualBox
    Output
      $html
      <div class="small-box bg-light-blue">
        <div class="inner">
          <h3>5</h3>
          <p>
            <p style="font-size: 90%">Responders in Current Selection</p>
          </p>
        </div>
        <div class="icon-large">
          <i class="fas fa-users" role="presentation" aria-label="users icon"></i>
        </div>
      </div>
      
      $deps
      $deps[[1]]
      List of 9
       $ name      : chr "font-awesome"
       $ version   : chr "6.4.0"
       $ src       :List of 1
        ..$ href: chr "font-awesome-6.4.0"
       $ meta      : NULL
       $ script    : NULL
       $ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi TRUE
       - attr(*, "class")= chr "html_dependency"
      
      

