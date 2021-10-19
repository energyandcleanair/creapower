library(testthat)


test_that("User permissions", {
  
  # Logging using provided user
  gcs.auth(force_service_account=T)
  
  expect_true(gcs.download("eia/gen_2020.RDS", "test.RDS"))
  expect_false(suppressWarnings(gcs.upload("test.RDS", "test.RDS")))
  
  # Restore normal auth
  googleAuthR::gar_deauth()
  gcs.auth()
  
})