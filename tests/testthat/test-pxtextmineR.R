test_that("pxtextmineR works", {
  
  cat_preds_only <- pxtextmineR::factory_predict_unlabelled_text_r(
    dataset = tidy_trust_data,
    predictor = "comment_txt",
    pipe_path_or_object = "fitted_pipeline.sav",
    column_names = "preds_only")
  
  crit_preds_only <- pxtextmineR::factory_predict_unlabelled_text_r(
    dataset = tidy_trust_data,
    predictor = "comment_txt",
    pipe_path_or_object = "pipeline_criticality.sav",
    column_names = "preds_only")
})
