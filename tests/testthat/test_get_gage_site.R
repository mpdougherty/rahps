library(rahps)
library(XML)
library(xml2)
library(dplyr)
library(htmltidy)

gage_id <- "canm7"
gage_file <- file.path(system.file("extdata", "canm7.xml",
                                   package = "rahps"))

gage_site_df <- get_gage_site(gage_id, gage_file)

test_that("check data structure", {
  expect_true(is.data.frame(gage_site_df))
  expect_true(length(gage_site_df$id) == 1)
})
