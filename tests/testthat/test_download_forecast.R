library(rahps)
library(xml2)
library(htmltidy)

gage_id <- "canm7"
path <- Sys.getenv("HOME")

gage_file <- rahps::download_forecast(gage_id, path)

# View the xml file
htmltidy::xml_view(xml2::read_xml(gage_file))


test_that("download forecast", {
  expected_file <- file.path(path, paste0(gage_id, ".xml"))
  xml <- xml2::read_xml(expected_file)

  expect_true(file.exists(expected_file))
  expect_true(class(xml)[[1]] == "xml_document")
})
