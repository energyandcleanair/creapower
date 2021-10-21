library(testthat)
library(tictoc)

test_that("User permissions", {
  
  # Logging using provided user
  gcs.auth()
  
  # Test that both download and caching system works
  f <- "test.RDS"
  suppressWarnings(file.remove(f))
  
  gcs.download("eia/gen_2020.RDS", f)
  expect_true(file.exists(f))
  mtime1 <- file.info(f)$mtime
  
  gcs.download("eia/gen_2020.RDS", f, only_if_modified_since = T)
  expect_true(file.exists(f))
  mtime2 <- file.info(f)$mtime
  
  expect_equal(mtime1, mtime2)
  
  # Test can't upload
  expect_false(suppressWarnings(gcs.upload("test.RDS", "test.RDS")))
  
  # Restore normal auth
  googleAuthR::gar_deauth()
  gcs.auth()
  
})