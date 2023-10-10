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
          Download data
        </a>
        <div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="proxy1-dynamic_comment_table" style="width:100%;height:auto;"></div>
      </div>

---

    Code
      session$returned
    Output
      <div class="row">
        <a id="proxy1-download_comments" class="btn btn-default shiny-download-link " href="" target="_blank" download>
          <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
          Download data
        </a>
        <div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="proxy1-dynamic_comment_table" style="width:100%;height:auto;"></div>
      </div>

