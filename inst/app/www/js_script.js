// JS function to get the clicked info and send it to R
function get_check_info(this_obj, module_id) {

  // you can used this_obj in place of event.target
  //alert(event.target.id);  // or alert(this_obj.id);
  
  // can also use module_id.concat('-current_check_info')
  //alert(module_id + "-current_check_info") 

  var id = event.target.id;
  var value = $(event.target).prop('checked');
  var info = [{id: id, value: value}];

  Shiny.setInputValue(module_id + "current_check_info", 
    info, {priority: "event"});
}

