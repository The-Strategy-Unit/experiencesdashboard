# app_ui works

    Code
      app_ui("id")
    Output
      <body class="skin-blue" style="min-height: 611px;">
        <div class="wrapper">
          <header class="main-header">
            <span class="logo"></span>
            <nav class="navbar navbar-static-top" role="navigation">
              <span style="display:none;">
                <i class="fas fa-bars" role="presentation" aria-label="bars icon"></i>
              </span>
              <a href="#" class="sidebar-toggle" data-toggle="offcanvas" role="button">
                <span class="sr-only">Toggle navigation</span>
              </a>
              <div class="navbar-custom-menu">
                <ul class="nav navbar-nav">
                  <li id="messageMenu-dynamic_messageMenu" class="shinydashboard-menu-output"></li>
                  <li class="dropdown">
                    <li class="dropdown">
                      <a onclick="onclick =window.open(&#39;https://cdu-data-science-team.github.io/PatientExperience-QDC/&#39;)" title="Go to Project Documentation Website" style="cursor: pointer;">
                        <i class="fas fa-book" role="presentation" aria-label="book icon"></i>
                      </a>
                    </li>
                    <li class="dropdown">
                      <a onclick="onclick =window.open(&#39;mailto:chris.beeley1@nhs.net?cc=oluwasegun.apejoye2@nottshc.nhs.uk&#39;)" title="Contact Project Team" style="cursor: pointer;">
                        <i class="fas fa-envelope" role="presentation" aria-label="envelope icon"></i>
                      </a>
                    </li>
                  </li>
                </ul>
              </div>
            </nav>
          </header>
          <aside id="sidebarCollapsed" class="main-sidebar" data-collapsed="false">
            <section id="sidebarItemExpanded" class="sidebar">
              <ul class="sidebar-menu" style="color: black;">
                <li>
                  <a href="#shiny-tab-experiences-user" data-toggle="tab" data-value="experiences-user" data-start-selected="1">
                    <i class="far fa-comment" role="presentation" aria-label="comment icon"></i>
                    <span>Qualitative Data Categorisation</span>
                    <small class="badge pull-right bg-green">dev</small>
                  </a>
                </li>
                <div id="filter_location_1" class="shiny-html-output"></div>
                <div id="filter_location_2" class="shiny-html-output"></div>
                <div id="filter_location_3" class="shiny-html-output"></div>
                <div id="demographics_selection_1-dynamic_demographics_selection" class="shiny-html-output"></div>
                <div id="date_filter_ui" class="shiny-html-output"></div>
              </ul>
            </section>
          </aside>
          <div class="content-wrapper">
            <section class="content">
              <a href="https://github.com/CDU-data-science-team/experiencesdashboard"
                   class="github-corner" aria-label="View source on GitHub"><svg width="80"
                   height="80" viewBox="0 0 250 250" style="fill:#64CEAA; color:#fff; position:
                   absolute; top: 50; border: 0; right: 0;" aria-hidden="true"><path d="M0,0
                   L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path><path d="M128.3,109.0
                   C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6
                   C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3
                   C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor"
                   style="transform-origin: 130px 106px;" class="octo-arm"></path><path
                   d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6
                   C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0
                   C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1
                   C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4
                   C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9
                   C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5
                   C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9
                   L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor"
                   class="octo-body"></path></svg></a><style>.github-corner:hover
                   .octo-arm{animation:octocat-wave 560ms ease-in-out}@keyframes
                   octocat-wave{0%,100%{transform:rotate(0)}20%,
                   60%{transform:rotate(-25deg)}40%,80%{transform:rotate(10deg)}}@media
                   (max-width:500px){.github-corner:hover .octo-arm{animation:none}.github-corner
                   .octo-arm{animation:octocat-wave 560ms ease-in-out}}</style>
              <div class="tab-content">
                <div role="tabpanel" class="tab-pane" id="shiny-tab-experiences-user">
                  <div id="patient_experience_ui_1-dynamicUI" class="shiny-html-output"></div>
                </div>
              </div>
            </section>
          </div>
        </div>
      </body>

# mod_click_tables_ui works

    Code
      mod_click_tables_ui("id")
    Output
      <div id="id-dynamic_click_tableUI" class="shiny-html-output"></div>

# mod_data_management_ui works

    Code
      mod_data_management_ui("id")
    Output
      <div class="container-fluid">
        <br/>
        <div class="row">
          <div class="col-sm-1">
            <button id="id-upload_new_data" type="button" class="btn btn-default action-button">
              <i class="fas fa-person-circle-plus" role="presentation" aria-label="person-circle-plus icon"></i>
              Upload new data
            </button>
          </div>
        </div>
        <hr/>
        <div id="id-data_management_UI" class="shiny-html-output"></div>
      </div>

# mod_demographics_ui works

    Code
      mod_demographics_ui("id")
    Output
      <div id="id-dynamic_demo_UI" class="shiny-html-output"></div>

# mod_documentation_page_ui works

    Code
      mod_documentation_page_ui("id")
    Output
      <br/>
      <p>This dashboard utilizes the pxtextmining API, a machine learning tool, to assign one or more subcategories to free-text comments
            based on the Qualitative Data Categorization (QDC) framework. The QDC framework is an evidence-based work that has been designed
            with several categories, each with its own set of subcategories. The categories group similar topics together to make it easier 
            for users to navigate the framework, while the subcategories reflect the actual topics that better represent the underlying data.</p>
      <p>The dashboard's visualizations and intuitive interactivity are thoughtfully created to help users effectively engage with the 
            comments and not merely quantify the data. Below is a high-level visual of the categories and subcategories:</p>
      <img src="www/framework_MVP_version.jpeg" width="100%"/>
      <hr/>
      <p>To see detailed description of the sub-categories, kindly click on the category to expand it.</p>
      <div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="id-framework_table" style="width:100%;height:auto;"></div>
      <hr/>
      <p>To get further detail about the data categorisation framework and the dashboard
              including some illustrative examples for each of the sub-categories.
              Please see the</p>
      <a href="https://cdu-data-science-team.github.io/PatientExperience-QDC/framework/framework3.html" target="_blank">
        <p>Patient Experience - QDC documentation Page</p>
      </a>

# mod_trend_overlap_ui works

    Code
      mod_trend_overlap_ui("id")
    Output
      <div id="id-dynamic_trend_overlap" class="shiny-html-output"></div>

# mod_patient_experience_ui works

    Code
      mod_patient_experience_ui("id")
    Output
      <div id="id-dynamicUI" class="shiny-html-output"></div>

# mod_trend_ui works

    Code
      mod_trend_ui("id")
    Output
      <br/>
      <div id="id-dynamic_trendUI" class="shiny-html-output"></div>

# mod_report_builder_ui works

    Code
      mod_report_builder_ui("id")
    Output
      <div id="id-dynamic_report_UI" class="shiny-html-output"></div>

# mod_search_text_ui works

    Code
      mod_search_text_ui("id")
    Output
      <div class="container-fluid">
        <p>Add multiple search terms with comma</p>
        <div class="form-group shiny-input-container">
          <label class="control-label" id="id-text_search-label" for="id-text_search">Search term(s)</label>
          <input id="id-text_search" type="text" class="shiny-input-text form-control" value="" placeholder="e.g. staff, doctor, nurse"/>
        </div>
        <hr/>
        <div id="id-dynamic_comment_ui" class="shiny-html-output"></div>
      </div>

# mod_summary_record_ui works

    Code
      mod_summary_record_ui("id")
    Output
      <div id="id-dynamic_summary_record" class="shiny-html-output"></div>

# mod_summary_ui works

    Code
      mod_summary_ui("id")
    Output
      <div id="id-dynamic_summary" class="shiny-html-output"></div>

