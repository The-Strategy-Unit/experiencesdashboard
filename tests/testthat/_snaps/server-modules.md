# mod_click_tables_server set up dynamic_click_tableUI correctly

    Code
      output$dynamic_click_tableUI
    Warning <simpleWarning>
      restarting interrupted promise evaluation
    Output
      $html
      <div class="container-fluid">
        <h5>Click a row to see comments related to that sub-category</h5>
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
          <style>#spinner-a80f884719a36e02edebd6b98f289997, #spinner-a80f884719a36e02edebd6b98f289997:before, #spinner-a80f884719a36e02edebd6b98f289997:after {   background: #0275D8; } #spinner-a80f884719a36e02edebd6b98f289997 {   color: #0275D8; } #spinner-a80f884719a36e02edebd6b98f289997 { font-size: 8px; }</style>
        </head>
        <div class="shiny-spinner-output-container shiny-spinner-hideui ">
          <div class="load-container shiny-spinner-hidden load1">
            <div id="spinner-a80f884719a36e02edebd6b98f289997" class="loader">Loading...</div>
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
        <div id="proxy1-comment_table" class="shiny-html-output"></div>
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
      
      

# mod_sentiment_server work correctly

    Code
      output$dynamic_sentiment_UI
    Warning <simpleWarning>
      textfont.color doesn't (yet) support data arrays
      textfont.color doesn't (yet) support data arrays
    Output
      $html
      <pre style="background-color:#005EB8; color:#fff">When the filtered data contain less than 7months of data, The sentiment plot below will be plotted as a weekly data rather than a month data. You can interact with the plot to read the underline comments</pre>
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
        <style>#spinner-0eaa74a7bcbaa527726ff3a6c1fe78ba, #spinner-0eaa74a7bcbaa527726ff3a6c1fe78ba:before, #spinner-0eaa74a7bcbaa527726ff3a6c1fe78ba:after {   background: #0275D8; } #spinner-0eaa74a7bcbaa527726ff3a6c1fe78ba {   color: #0275D8; } #spinner-0eaa74a7bcbaa527726ff3a6c1fe78ba { font-size: 8px; }</style>
      </head>
      <div class="shiny-spinner-output-container shiny-spinner-hideui ">
        <div class="load-container shiny-spinner-hidden load1">
          <div id="spinner-0eaa74a7bcbaa527726ff3a6c1fe78ba" class="loader">Loading...</div>
        </div>
        <div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item-overflow-hidden html-fill-item" id="proxy1-sentiment_plot" style="width:100%;height:400px;"></div>
      </div>
      <hr/>
      <div id="proxy1-dynamic_UI" class="shiny-html-output"></div>
      
      $deps
      $deps[[1]]
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
      
      $deps[[2]]
      List of 9
       $ name      : chr "plotly-binding"
       $ version   : chr "4.10.2"
       $ src       :List of 1
        ..$ href: chr "plotly-binding-4.10.2"
       $ meta      : NULL
       $ script    : chr "plotly.js"
       $ stylesheet: NULL
       $ head      : NULL
       $ attachment: NULL
       $ all_files : logi FALSE
       - attr(*, "class")= chr "html_dependency"
      
      

---

    Code
      output$sentiment_plot
    Output
      {"x":{"visdat":{"729c325e2cce":["function () ","plotlyVisDat"]},"cur_data":"729c325e2cce","attrs":{"729c325e2cce":{"x":{},"y":{},"hovertemplate":{},"key":{},"color":{},"colors":["#009639","#FAE100","#DA291C"],"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Comment sentiment over time (month)","xaxis":{"domain":[0,1],"automargin":true,"title":"Date (month)"},"yaxis":{"domain":[0,1],"automargin":true,"title":"% contribution","type":"-","ticksuffix":"%"},"barmode":"stack","hovermode":"closest","showlegend":false,"legend":{"yanchor":"top","y":0.5}},"source":"proxy1-sentiment_plot","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"modeBarButtons":[["toImage"]],"toImageButtonOptions":{"format":"png"}},"data":[{"x":["2020-05-01","2020-05-01","2020-05-01","2020-05-01","2020-08-01","2020-08-01","2020-09-01","2020-09-01","2020-09-01","2020-10-01","2020-11-01","2020-11-01","2020-11-01","2020-12-01","2020-12-01","2021-01-01","2021-01-01","2021-01-01","2021-01-01","2021-01-01","2021-02-01","2021-02-01","2021-02-01","2021-02-01","2021-03-01","2021-03-01","2021-03-01","2021-03-01","2021-03-01","2021-04-01","2021-04-01","2021-05-01","2021-05-01","2021-06-01","2021-06-01","2021-06-01","2021-06-01","2021-06-01"],"y":[25,25,25,25,50,50,25,25,50,100,20,40,40,50,50,4.5454545454545459,36.363636363636367,22.727272727272727,18.181818181818183,18.181818181818183,25,25,25,25,15,25,10,35,15,33.333333333333329,66.666666666666657,50,50,21.428571428571427,21.428571428571427,14.285714285714285,25,17.857142857142858],"hovertemplate":["Date: 2020-05-01 <br>Sentiment score: 1 <br>%total:  25 % <br>No. comments: 1","Date: 2020-05-01 <br>Sentiment score: 2 <br>%total:  25 % <br>No. comments: 1","Date: 2020-05-01 <br>Sentiment score: 3 <br>%total:  25 % <br>No. comments: 1","Date: 2020-05-01 <br>Sentiment score: 4 <br>%total:  25 % <br>No. comments: 1","Date: 2020-08-01 <br>Sentiment score: 1 <br>%total:  50 % <br>No. comments: 1","Date: 2020-08-01 <br>Sentiment score: 2 <br>%total:  50 % <br>No. comments: 1","Date: 2020-09-01 <br>Sentiment score: 3 <br>%total:  25 % <br>No. comments: 1","Date: 2020-09-01 <br>Sentiment score: 4 <br>%total:  25 % <br>No. comments: 1","Date: 2020-09-01 <br>Sentiment score: 5 <br>%total:  50 % <br>No. comments: 2","Date: 2020-10-01 <br>Sentiment score: 1 <br>%total:  100 % <br>No. comments: 1","Date: 2020-11-01 <br>Sentiment score: 1 <br>%total:  20 % <br>No. comments: 1","Date: 2020-11-01 <br>Sentiment score: 3 <br>%total:  40 % <br>No. comments: 2","Date: 2020-11-01 <br>Sentiment score: 5 <br>%total:  40 % <br>No. comments: 2","Date: 2020-12-01 <br>Sentiment score: 1 <br>%total:  50 % <br>No. comments: 1","Date: 2020-12-01 <br>Sentiment score: 2 <br>%total:  50 % <br>No. comments: 1","Date: 2021-01-01 <br>Sentiment score: 1 <br>%total:  4.5 % <br>No. comments: 1","Date: 2021-01-01 <br>Sentiment score: 2 <br>%total:  36.4 % <br>No. comments: 8","Date: 2021-01-01 <br>Sentiment score: 3 <br>%total:  22.7 % <br>No. comments: 5","Date: 2021-01-01 <br>Sentiment score: 4 <br>%total:  18.2 % <br>No. comments: 4","Date: 2021-01-01 <br>Sentiment score: 5 <br>%total:  18.2 % <br>No. comments: 4","Date: 2021-02-01 <br>Sentiment score: 1 <br>%total:  25 % <br>No. comments: 1","Date: 2021-02-01 <br>Sentiment score: 3 <br>%total:  25 % <br>No. comments: 1","Date: 2021-02-01 <br>Sentiment score: 4 <br>%total:  25 % <br>No. comments: 1","Date: 2021-02-01 <br>Sentiment score: 5 <br>%total:  25 % <br>No. comments: 1","Date: 2021-03-01 <br>Sentiment score: 1 <br>%total:  15 % <br>No. comments: 3","Date: 2021-03-01 <br>Sentiment score: 2 <br>%total:  25 % <br>No. comments: 5","Date: 2021-03-01 <br>Sentiment score: 3 <br>%total:  10 % <br>No. comments: 2","Date: 2021-03-01 <br>Sentiment score: 4 <br>%total:  35 % <br>No. comments: 7","Date: 2021-03-01 <br>Sentiment score: 5 <br>%total:  15 % <br>No. comments: 3","Date: 2021-04-01 <br>Sentiment score: 3 <br>%total:  33.3 % <br>No. comments: 2","Date: 2021-04-01 <br>Sentiment score: 5 <br>%total:  66.7 % <br>No. comments: 4","Date: 2021-05-01 <br>Sentiment score: 1 <br>%total:  50 % <br>No. comments: 1","Date: 2021-05-01 <br>Sentiment score: 2 <br>%total:  50 % <br>No. comments: 1","Date: 2021-06-01 <br>Sentiment score: 1 <br>%total:  21.4 % <br>No. comments: 6","Date: 2021-06-01 <br>Sentiment score: 2 <br>%total:  21.4 % <br>No. comments: 6","Date: 2021-06-01 <br>Sentiment score: 3 <br>%total:  14.3 % <br>No. comments: 4","Date: 2021-06-01 <br>Sentiment score: 4 <br>%total:  25 % <br>No. comments: 7","Date: 2021-06-01 <br>Sentiment score: 5 <br>%total:  17.9 % <br>No. comments: 5"],"key":["1","2","3","4","1","2","3","4","5","1","1","3","5","1","2","1","2","3","4","5","1","3","4","5","1","2","3","4","5","3","5","1","2","1","2","3","4","5"],"type":"bar","marker":{"colorbar":{"title":"sentiment","ticklen":2},"cmin":1,"cmax":5,"colorscale":[["0","rgba(0,150,57,1)"],["0.0416666666666667","rgba(52,157,56,1)"],["0.0833333333333333","rgba(77,163,54,1)"],["0.125","rgba(98,170,52,1)"],["0.166666666666667","rgba(117,176,50,1)"],["0.208333333333333","rgba(135,182,47,1)"],["0.25","rgba(152,188,44,1)"],["0.291666666666667","rgba(169,195,41,1)"],["0.333333333333333","rgba(185,201,37,1)"],["0.375","rgba(201,207,32,1)"],["0.416666666666667","rgba(218,213,25,1)"],["0.458333333333333","rgba(234,219,16,1)"],["0.5","rgba(250,225,0,1)"],["0.541666666666667","rgba(249,212,7,1)"],["0.583333333333333","rgba(247,199,13,1)"],["0.625","rgba(246,185,17,1)"],["0.666666666666667","rgba(244,172,20,1)"],["0.708333333333333","rgba(241,158,22,1)"],["0.75","rgba(239,144,24,1)"],["0.791666666666667","rgba(236,130,25,1)"],["0.833333333333333","rgba(233,116,26,1)"],["0.875","rgba(230,100,27,1)"],["0.916666666666667","rgba(226,84,28,1)"],["0.958333333333333","rgba(222,65,28,1)"],["1","rgba(218,41,28,1)"]],"showscale":false,"color":[1,2,3,4,1,2,3,4,5,1,1,3,5,1,2,1,2,3,4,5,1,3,4,5,1,2,3,4,5,3,5,1,2,1,2,3,4,5],"line":{"colorbar":{"title":"","ticklen":2},"cmin":1,"cmax":5,"colorscale":[["0","rgba(0,150,57,1)"],["0.0416666666666667","rgba(52,157,56,1)"],["0.0833333333333333","rgba(77,163,54,1)"],["0.125","rgba(98,170,52,1)"],["0.166666666666667","rgba(117,176,50,1)"],["0.208333333333333","rgba(135,182,47,1)"],["0.25","rgba(152,188,44,1)"],["0.291666666666667","rgba(169,195,41,1)"],["0.333333333333333","rgba(185,201,37,1)"],["0.375","rgba(201,207,32,1)"],["0.416666666666667","rgba(218,213,25,1)"],["0.458333333333333","rgba(234,219,16,1)"],["0.5","rgba(250,225,0,1)"],["0.541666666666667","rgba(249,212,7,1)"],["0.583333333333333","rgba(247,199,13,1)"],["0.625","rgba(246,185,17,1)"],["0.666666666666667","rgba(244,172,20,1)"],["0.708333333333333","rgba(241,158,22,1)"],["0.75","rgba(239,144,24,1)"],["0.791666666666667","rgba(236,130,25,1)"],["0.833333333333333","rgba(233,116,26,1)"],["0.875","rgba(230,100,27,1)"],["0.916666666666667","rgba(226,84,28,1)"],["0.958333333333333","rgba(222,65,28,1)"],["1","rgba(218,41,28,1)"]],"showscale":false,"color":[1,2,3,4,1,2,3,4,5,1,1,3,5,1,2,1,2,3,4,5,1,3,4,5,1,2,3,4,5,3,5,1,2,1,2,3,4,5]}},"xaxis":"x","yaxis":"y","_isNestedKey":false,"frame":null},{"x":[18383,18779],"y":[4.5454545454545459,100],"type":"scatter","mode":"markers","opacity":0,"hoverinfo":"none","showlegend":false,"marker":{"colorbar":{"title":"sentiment","ticklen":2,"len":0.5,"lenmode":"fraction","y":1,"yanchor":"top"},"cmin":1,"cmax":5,"colorscale":[["0","rgba(0,150,57,1)"],["0.0416666666666667","rgba(52,157,56,1)"],["0.0833333333333333","rgba(77,163,54,1)"],["0.125","rgba(98,170,52,1)"],["0.166666666666667","rgba(117,176,50,1)"],["0.208333333333333","rgba(135,182,47,1)"],["0.25","rgba(152,188,44,1)"],["0.291666666666667","rgba(169,195,41,1)"],["0.333333333333333","rgba(185,201,37,1)"],["0.375","rgba(201,207,32,1)"],["0.416666666666667","rgba(218,213,25,1)"],["0.458333333333333","rgba(234,219,16,1)"],["0.5","rgba(250,225,0,1)"],["0.541666666666667","rgba(249,212,7,1)"],["0.583333333333333","rgba(247,199,13,1)"],["0.625","rgba(246,185,17,1)"],["0.666666666666667","rgba(244,172,20,1)"],["0.708333333333333","rgba(241,158,22,1)"],["0.75","rgba(239,144,24,1)"],["0.791666666666667","rgba(236,130,25,1)"],["0.833333333333333","rgba(233,116,26,1)"],["0.875","rgba(230,100,27,1)"],["0.916666666666667","rgba(226,84,28,1)"],["0.958333333333333","rgba(222,65,28,1)"],["1","rgba(218,41,28,1)"]],"showscale":true,"color":[1,5],"line":{"color":"rgba(255,127,14,1)"}},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[],"deps":[{"name":"typedarray","version":"0.1","src":{"href":"typedarray-0.1"},"meta":null,"script":"typedarray.min.js","stylesheet":null,"head":null,"attachment":null,"all_files":false},{"name":"jquery","version":"3.5.1","src":{"href":"jquery-3.5.1"},"meta":null,"script":"jquery.min.js","stylesheet":null,"head":null,"attachment":null,"all_files":true},{"name":"crosstalk","version":"1.2.0","src":{"href":"crosstalk-1.2.0"},"meta":null,"script":"js/crosstalk.min.js","stylesheet":"css/crosstalk.min.css","head":null,"attachment":null,"all_files":true},{"name":"plotly-htmlwidgets-css","version":"2.11.1","src":{"href":"plotly-htmlwidgets-css-2.11.1"},"meta":null,"script":null,"stylesheet":"plotly-htmlwidgets.css","head":null,"attachment":null,"all_files":false},{"name":"plotly-main","version":"2.11.1","src":{"href":"plotly-main-2.11.1"},"meta":null,"script":"plotly-latest.min.js","stylesheet":null,"head":null,"attachment":null,"all_files":false}]} 

---

    Code
      output$sentiment_comment
    Output
      $html
      <pre>
        <strong>No data to show</strong>
      </pre>
      
      $deps
      list()
      

