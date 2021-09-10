test_that("Previous quarter correct", {
  
  # random day in August
  
  rdia <- previous_quarter(as.Date("2021-08-22"))
  
  # first day of quarter
  
  fdoq <- previous_quarter(as.Date("2021-01-01"))
  
  # last day of quarter
  
  ldoq <- previous_quarter(as.Date("2021-06-30"))
  
  expect_equal(rdia[1], as.Date("2021-04-01"))
  
  expect_equal(fdoq[2], as.Date("2020-12-31"))
})
