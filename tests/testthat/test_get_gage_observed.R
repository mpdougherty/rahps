library(rahps)
library(XML)
library(xml2)
library(dplyr)

gage_id <- "canm7"
gage_file <- file.path(system.file("extdata", "canm7.xml",
                                   package = "rahps"))

gage_observed_df <- get_gage_observed(gage_id, gage_file)

test_that("check data structure", {
  expect_true(is.data.frame(gage_observed_df))
  expect_true(length(gage_observed_df$gage_id) == 59)
})
