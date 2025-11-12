# Augment incomplete demo dataset to get it to work in the demo version of the
# app, Nov 2025. See https://github.com/The-Strategy-Unit/experiencesdashboard/issues/217

# Data provided by CB
test_data <- readr::read_csv("data-raw/test_data.csv") |>
  dplyr::mutate(
    comment_id = dplyr::row_number() |> as.character(),  # required for API call
    .before = 1
  )

# Get labels from quick API ----

# The provided demo data doesn't have category labels, fetch from the quick API

text_data <- test_data |>
  dplyr::select(comment_id, comment_text = comment_txt) |>  # min cols required
  dplyr::filter(!is.na(comment_text))

text_json <- text_data |> jsonlite::toJSON()

resp_label <- httr::POST(
  url = "https://connect.strategyunitwm.nhs.uk/text_api/predict_multilabel",
  body = text_json,
  encode = "json",
  httr::add_headers("Content-Type" = "application/json")
)
resp_label_parsed <- httr::content(resp, "text") |>
  jsonlite::fromJSON() |>
  tibble::as_tibble() |>
  tidyr::unnest_wider(labels, names_sep = "_")  # multiple labels may be returned

readr::write_csv(
  resp_parsed,
  "data-raw/text_data_labels.csv"
)

# Combine into demo dataset ----

# Put the data together, add new columns

load("data/framework.rda")  # contains a category lookup
framework <- framework |>
  dplyr::select(
    # beware: naming difference between data and the UI
    super_category = Category,
    category = `Sub-category`
  )

text_data_labels <- readr::read_csv(
  "data-raw/text_data_labels.csv",  # as fetched in the section above
  col_types = "c"
) |>
  dplyr::left_join(
    framework,
    by = dplyr::join_by("labels_1" == "category")  # just use first label for demo
  ) |>
  dplyr::select(comment_id, category = labels_1, super_category)

test_data_updated <- test_data |>
  dplyr::select(-category) |>
  dplyr::left_join(text_data_labels, by = "comment_id") |>
  dplyr::mutate(
    # add (assumed) full column set with placeholder values for demo
    sentiment = 1,  # the quick API doesn't do sentiment
    sex = "F",
    religion = "A",
    last_upload_date = lubridate::today(),
    last_edit_date = lubridate::today(),
    # not sure what these are for
    extra_variable_1 = "X",
    extra_variable_2 = "Y",
    extra_variable_3 = "Z"
  )

# Save with the filename that the app will be looking for, given that we've
# hard-coded R_CONFIG_ACTIVE to be 'phase_2_demo' in the run_dev_script
test_data_updated |>
  readr::write_csv("data-raw/phase_2_sample_data.csv")
