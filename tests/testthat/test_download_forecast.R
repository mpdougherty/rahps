library(rahps)

gage_id <- "canm7"
path <- Sys.getenv("HOME")

gage_file <- rahps::download_forecast(gage_id = gage_id, path = path)


test_that("download forecast", {
  expected_file <- file.path(path, paste0(gage_id, ".xml"))
  expect_true(file.exists(expected_file))
})
