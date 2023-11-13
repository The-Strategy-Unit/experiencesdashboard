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
                      <a onclick="onclick =window.open(&#39;mailto:chris.beeley1@nhs.net&#39;)" title="Contact Project Team" style="cursor: pointer;">
                        <i class="fas fa-envelope" role="presentation" aria-label="envelope icon"></i>
                      </a>
                    </li>
                    <li class="dropdown">
                      <a onclick="onclick =window.open(&#39;https://github.com/CDU-data-science-team/experiencesdashboard&#39;)" title="Visit project GitHub page" style="cursor: pointer;">
                        <i class="fab fa-github" role="presentation" aria-label="github icon"></i>
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
              <div class="tab-content">
                <div role="tabpanel" class="tab-pane" id="shiny-tab-experiences-user">
                  <div class="shiny-spinner-output-container shiny-spinner-hideui ">
                    <div class="load-container shiny-spinner-hidden load1">
                      <div id="spinner-ce037ce376c5887ba6d38bdd0fa93ff0" class="loader">Loading...</div>
                    </div>
                    <div style="height:400px" class="shiny-spinner-placeholder"></div>
                    <div id="patient_experience_ui_1-dynamicUI" class="shiny-html-output"></div>
                  </div>
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
      <div class="shiny-spinner-output-container shiny-spinner-hideui ">
        <div class="load-container shiny-spinner-hidden load1">
          <div id="spinner-8037d1009a7c45bcf4543a6fd53b1e71" class="loader">Loading...</div>
        </div>
        <div style="height:400px" class="shiny-spinner-placeholder"></div>
        <div id="id-dynamic_click_tableUI" class="shiny-html-output"></div>
      </div>

# mod_data_management_ui works

    Code
      mod_data_management_ui("id")
    Output
      <div class="container-fluid">
        <br/>
        <div class="row">
          <p>
              This page is for users who wants to upload new data or amend the
              existing data in the dashboard
                </p>
          <div class="col-sm-1">
            <button id="id-upload_new_data" type="button" class="btn btn-default action-button">
              <i class="fas fa-person-circle-plus" role="presentation" aria-label="person-circle-plus icon"></i>
              Upload new data
            </button>
          </div>
        </div>
        <hr/>
        <div class="shiny-spinner-output-container shiny-spinner-hideui ">
          <div class="load-container shiny-spinner-hidden load1">
            <div id="spinner-4f4addaabb32c3acde588397b5fddf19" class="loader">Loading...</div>
          </div>
          <div style="height:400px" class="shiny-spinner-placeholder"></div>
          <div id="id-data_management_UI" class="shiny-html-output"></div>
        </div>
      </div>

# mod_demographics_ui works

    Code
      mod_demographics_ui("id")
    Output
      <div class="shiny-spinner-output-container shiny-spinner-hideui ">
        <div class="load-container shiny-spinner-hidden load1">
          <div id="spinner-a20ad783510d301bbba36f3fff1f1e4f" class="loader">Loading...</div>
        </div>
        <div style="height:400px" class="shiny-spinner-placeholder"></div>
        <div id="id-dynamic_demo_UI" class="shiny-html-output"></div>
      </div>

# mod_documentation_page_ui works

    Code
      mod_documentation_page_ui("id")
    Output
      <h4 style="color : #005EB8;">
        <strong>Making best use of the qualitative comments</strong>
      </h4>
      
            The key feature of this dashboard is the categorisation of large volumes
            of qualitative comments, it should be used to facilitate initial
            exploration of your qualitative data, before drawing fuller insight
            from the underlying qualitative comments within the sub-categories.
            Before using the dashboard, you should read the good practice guidance
            on the documentation page: <a href="https://cdu-data-science-team.github.io/PatientExperience-QDC/dashboard/dashboard_good_practice.html" target="_blank">
        <strong>Good practice guidance.</strong>
      </a>This includes important
              information, tips, and advice to help you maximise your use of the categorised qualitative comments,
              whilst avoiding the risks around relying on the quantification of qualitative data.
      <br/> <br/>
      To get further detail about the data categorisation framework and the dashboard
              including some illustrative examples for each of the sub-categories.
              Please see the <a href="https://cdu-data-science-team.github.io/PatientExperience-QDC/framework/framework3.html" target="_blank">
        <strong>Patient Experience - QDC documentation Page.</strong>
      </a>
      <h4 style="color : #005EB8;">
        <strong>Introduction to the Data Categorisation Framework</strong>
      </h4>
      <p>This dashboard utilizes the pxtextmining API, a machine learning tool, to assign one or more subcategories to free-text comments
            based on the Qualitative Data Categorization (QDC) framework. The QDC framework is an evidence-based work that has been designed
            with several categories, each with its own set of subcategories. The categories group similar topics together to make it easier
            for users to navigate the framework, while the subcategories reflect the actual topics that better represent the underlying data.</p>
      <p>The dashboard's visualizations and intuitive interactivity are thoughtfully created to help users effectively engage with the
            comments and not merely quantify the data. Below is a high-level visual of the categories and subcategories:</p>
      <img src="www/framework_MVP_version.jpeg" width="100%"/>
      <br/> <br/>
      <p>To see detailed description of the sub-categories, kindly click on the category to expand it.</p>
      <div class="shiny-spinner-output-container shiny-spinner-hideui ">
        <div class="load-container shiny-spinner-hidden load1">
          <div id="spinner-f8e944809a302ad9c1115264dc8453d1" class="loader">Loading...</div>
        </div>
        <div style="height:400px" class="shiny-spinner-placeholder"></div>
        <div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="id-framework_table" style="width:100%;height:auto;"></div>
      </div>

# mod_trend_overlap_ui works

    Code
      mod_trend_overlap_ui("id")
    Output
      <div class="shiny-spinner-output-container shiny-spinner-hideui ">
        <div class="load-container shiny-spinner-hidden load1">
          <div id="spinner-43e682cb011f04edba48fdc823ac3110" class="loader">Loading...</div>
        </div>
        <div style="height:400px" class="shiny-spinner-placeholder"></div>
        <div id="id-dynamic_trend_overlap" class="shiny-html-output"></div>
      </div>

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
      <div class="shiny-spinner-output-container shiny-spinner-hideui ">
        <div class="load-container shiny-spinner-hidden load1">
          <div id="spinner-2952843a500da0d7dc13c7f622b4db18" class="loader">Loading...</div>
        </div>
        <div style="height:400px" class="shiny-spinner-placeholder"></div>
        <div id="id-dynamic_trendUI" class="shiny-html-output"></div>
      </div>

# mod_complex_comments_ui works

    Code
      mod_complex_comments_ui("id")
    Output
      <div class="shiny-spinner-output-container shiny-spinner-hideui ">
        <div class="load-container shiny-spinner-hidden load1">
          <div id="spinner-6c68bc92f54824dd748672d47601c52b" class="loader">Loading...</div>
        </div>
        <div style="height:400px" class="shiny-spinner-placeholder"></div>
        <div id="id-dynamic_complex_tableUI" class="shiny-html-output"></div>
      </div>

# mod_search_text_ui works

    Code
      mod_search_text_ui("id")
    Output
      <div class="container-fluid">
        <h4 style="color: #005EB8">
          <strong>Appropriate use of the comment search</strong>
        </h4>
        <p>
            Before searching for comments on a specific topic, first identify whether 
            a relevant sub-category already exists in the categorisation framework. 
            If it does you should look at the comments categorised there without need 
            to use the comment search. Word searches are a simplistic approach which 
            can miss a lot of relevant data, whereas the sophisticated categorisation 
            approach used within this tool has much better results. The comment search
            is best utilised when you have a very narrow focus of interest and there 
            is a small consistent vocabulary around the topic.
            </p>
        <h4 style="color: #005EB8">
          <strong>How search is done</strong>
        </h4>
        <p>The search uses Boolean techniques to find all provided words, 
            including their singular and plural forms. For instance, if you input 
            "staff, doctors", it will also search for "staffs, doctor" and so on.".
              </p>
        <div class="form-group shiny-input-container">
          <label class="control-label" id="id-text_search-label" for="id-text_search">Search term(s) - Add multiple search terms with comma</label>
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

