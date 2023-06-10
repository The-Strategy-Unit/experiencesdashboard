testServer(mod_summary_server, args = list(data_exists = FALSE), {
  # expect_no_error(output$dynamic_summary)

  # Example of tests you can do on the server:
  # - Checking reactiveValues
  # expect_equal(r$lg, 'EN')
  # - Checking output
  # expect_equal(output$txt, "Text")
})

# testServer(mod_summary_record_server, args = list(db_data=phase_2_db_data, filter_data=phase_2_db_data), {
#   inherits(output$commentBox, 'list') |> expect_true()
# })
