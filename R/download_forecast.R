#' @title Download Gage
#'
#' @description Download the `.xml` file for the specified gage from the NWS
#' AHPS web service.
#'
#' @export
#' @param gage_id      character; The 5-digit river gage identifier. The list
#'                     can be found at https://water.weather.gov/ahps/.
#' @param path         character; The path to the folder where the `.xml` file
#'                     will be written to.
#'
#' @return character; The path to the downloaded gage forecast xml file.
#'
#' @examples
#' \donttest{
#' gage_id <- "canm7"
#' path <- Sys.getenv("HOME")
#'
#' gage_file <- rahps::download_forecast(gage_id, path)
#'
#' # View the xml file
#' htmltidy::xml_view(xml2::read_xml(gage_file))
#' }
#'
#' @importFrom utils download.file
#' @importFrom htmltidy xml_view
#'
download_forecast <- function(gage_id, path) {
  # Current nws ahps hydrograph_to_html url
  ahps_url <- "https://water.weather.gov/ahps2/hydrograph_to_xml.php"

  # Construct the URL
  gage_url <- paste0(ahps_url, "?gage=", gage_id, "&output=xml")

  # Construct the output file
  gage_file <- file.path(path, paste0(gage_id, ".xml"))

  # Download the file
  utils::download.file(gage_url, gage_file)

  return(gage_file)
}
