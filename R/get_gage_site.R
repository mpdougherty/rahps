#' @title Get Gage Site Table
#'
#' @description Extract the gage site information from the input gage xml file
#'   and write it into a data frame.
#'
#' @export
#' @param gage_id      character; The 5-digit river gage identifer. The list
#'                     can be found at https://water.weather.gov/ahps/.
#' @param gage_file    character; The path to a gage forecast xml file.
#'
#' @return data frame; A data frame containing gage site information for the
#'   input gage forecast xml file.
#'
#' @importFrom XML xmlParse xmlAttrs xmlRoot xmlToDataFrame getNodeSet
#' @importFrom xml2 read_xml xml_attr xml_find_all
#' @importFrom dplyr rename_with bind_cols
#'
get_gage_site <- function(gage_id, gage_file) {
  # Convert to an XML object
  gage_XML <- XML::xmlParse(gage_file)

  # Read the xml as an xml2 object
  gage_xml2 <- xml2::read_xml(gage_file)

  # Create a data frame of the gage site attributes
  gage_site <- data.frame(as.list(XML::xmlAttrs(XML::xmlRoot(gage_XML))))

  # Extract sigstages
  gage_sigstages <- XML::xmlToDataFrame(gage_XML,
                                        nodes = XML::getNodeSet(gage_XML,
                                                                "//sigstages"))
  gage_sigstages <- dplyr::rename_with(gage_sigstages,
                                       function(x) {paste0("stage_", x)})
  gage_site <- dplyr::bind_cols(gage_site, gage_sigstages)

  # Extract sigflows
  gage_sigflows <- XML::xmlToDataFrame(gage_XML,
                                       nodes = XML::getNodeSet(gage_XML,
                                                               "//sigflows"))
  gage_sigflows <- dplyr::rename_with(gage_sigflows,
                                      function(x) {paste0("flow_", x)})
  gage_site <- dplyr::bind_cols(gage_site, gage_sigflows)

  # Extract zerodatum
  gage_zero <- XML::xmlToDataFrame(gage_XML,
                                   nodes = XML::getNodeSet(gage_XML,
                                                           "//zerodatum"))
  gage_site$gage_zero <- as.numeric(gage_zero[1])
  gage_site$gage_zerodatum <- xml2::xml_attr(xml2::xml_find_all(gage_xml2,
                                                                "//zerodatum"),
                                             "units")
  return(gage_site)
}
