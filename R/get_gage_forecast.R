#' @title Get Gage Forecast Data
#'
#' @description Extract the gage forecast information from the input gage xml
#'   file and write it into a data frame.
#'
#' @export
#' @param gage_id      character; The 5-digit river gage identifer. The list
#'                     can be found at https://water.weather.gov/ahps/.
#' @param gage_file    character; The path to a gage forecast xml file.
#'
#' @return data frame; A data frame containing gage site information for the
#'   input gage forecast xml file.
#'
#' @importFrom XML xmlParse xmlToDataFrame getNodeSet
#' @importFrom xml2 read_xml xml_attr xml_find_all
#' @importFrom lubridate as_datetime
#' @importFrom dplyr select
#'
get_gage_forecast <- function(gage_id, gage_file) {
  # Get the gage_site data frame
  gage_site <- rahps::get_gage_site(gage_id, gage_file)

  # Convert to an XML object
  gage_XML <- XML::xmlParse(gage_file)

  # Read the xml as an xml2 object
  gage_xml2 <- xml2::read_xml(gage_file)

  # Extract the forecast nodes and convert to a data frame
  forecast <- XML::xmlToDataFrame(gage_XML,
                                  nodes = XML::getNodeSet(gage_XML,
                                                          "//forecast//datum"))
  forecast$gage_id        <- gage_site$id
  forecast$gage_name      <- gage_site$name
  forecast$gage_zero      <- gage_site$gage_zero
  forecast$gage_zerodatum <- gage_site$gage_zerodatum
  forecast$type           <- "forecast"
  forecast_timezone <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                         "//forecast"),
                                      "timezone")
  forecast_datetime <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                         "//forecast"),
                                      "issued")
  forecast$forecast_timezone <- forecast_timezone
  forecast$forecast_datetime <- lubridate::as_datetime(forecast_datetime,
                                                       tz = forecast_timezone)
  forecast$timezone <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                    "//forecast//datum/valid"),
                                      "timezone")
  forecast$datetime <- lubridate::as_datetime(forecast$valid,
                                              tz = unique(forecast$timezone))
  forecast$primary_name <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                  "//forecast//datum/primary"),
                                          "name")
  forecast$primary_value <- forecast$primary
  forecast$primary_units <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                  "//forecast//datum/primary"),
                                           "units")
  forecast$secondary_name <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                "//forecast//datum/secondary"),
                                            "name")
  forecast$secondary_value <- forecast$secondary
  forecast$secondary_units <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                "//forecast//datum/secondary"),
                                             "units")
  forecast <- dplyr::select(forecast, -c(valid, primary, secondary, pedts))
  return(forecast)
}
