#' @title Get Gage Observed Date
#'
#' @description Extract the gage observed information from the input gage xml
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
#' @examples
#' gage_id <- "canm7"
#' gage_file <- file.path(system.file("extdata", "canm7.xml",
#'                                    package = "rahps"))
#'
#' gage_observed_df <- get_gage_observed(gage_id, gage_file)
#'
#' @importFrom XML xmlParse xmlToDataFrame getNodeSet
#' @importFrom xml2 read_xml xml_attr xml_find_all
#' @importFrom lubridate as_datetime
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
get_gage_observed <- function(gage_id, gage_file) {
  # Get the gage_site data frame
  gage_site <- rahps::get_gage_site(gage_id, gage_file)

  # Convert to an XML object
  gage_XML <- XML::xmlParse(gage_file)

  # Read the xml as an xml2 object
  gage_xml2 <- xml2::read_xml(gage_file)

  # Extract the observed nodes and convert to a data frame
  observed <- XML::xmlToDataFrame(gage_XML,
                                  nodes = XML::getNodeSet(gage_XML,
                                                          "//observed//datum"))
  observed$gage_id        <- gage_site$id
  observed$gage_name      <- gage_site$name
  observed$gage_zero      <- gage_site$gage_zero
  observed$gage_zerodatum <- gage_site$gage_zerodatum
  observed$type           <- "observed"
  observed$timezone <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                   "//observed//datum/valid"),
                                      "timezone")
  observed$datetime <- lubridate::as_datetime(observed$valid,
                                              tz = unique(observed$timezone))
  observed$primary_name <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                 "//observed//datum/primary"),
                                          "name")
  observed$primary_value <- observed$primary
  observed$primary_units <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                  "//observed//datum/primary"),
                                           "units")
  observed$secondary_name <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                "//observed//datum/secondary"),
                                            "name")
  observed$secondary_value <- observed$secondary
  observed$secondary_units <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                "//observed//datum/secondary"),
                                            "units")
  observed <- dplyr::select(observed, -c(.data$valid,
                                         .data$primary,
                                         .data$secondary,
                                         .data$pedts))
  return(observed)
}
