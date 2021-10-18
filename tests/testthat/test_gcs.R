library(testthat)


test_that("User permissions", {
  
  # Logging using provided user
  gcs.auth(force_service_account=T)
  
  expect_true(gcs.download("entso/gen_AT_2021.RDS", "test.RDS"))
  expect_error(gcs.upload("test.RDS", "entso/gen_AT_2021.RDS"))
    
})