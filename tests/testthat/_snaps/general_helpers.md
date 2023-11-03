# add_checkbox_buttons return expected result

    Code
      add_checkbox_buttons("inputId", "module_id", "flag_value", "bad_value")
    Output
      <div class="form-group">
        <div class="checkbox">
          <label>
            <input id="flag_inputId" type="checkbox" class="shiny-input-checkbox"
             NA onclick=get_check_info(this,"module_id")>
             <span><i class="fa-solid fa-flag" style="color:green"></i></span>
          </label>
        </div>
        <div class="checkbox">
          <label>
            <input id="bad_inputId" type="checkbox" class="shiny-input-checkbox"
             NA onclick=get_check_info(this,"module_id")>
             <span><i class="fa-solid fa-circle-xmark" style="color:red"></i></span>
          </label>
        </div>
      </div>

