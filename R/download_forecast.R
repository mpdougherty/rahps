#' @title Download Gage
#'
#' @description Download the `.xml` file for the specified gage from the NWS
#' AHPS web service.
#'
#' @export
#' @param gage_id      character; The 5-digit river gage identifer. The list
#'                     can be found at https://water.weather.gov/ahps/.
#' @param path         character; The path to the folder where the `.xml` file
#'                     will be written to.
#'
#' @return Path the downloaded gage forcast .xml file.
#'
download_forecast <- function(gage_id, path) {
  # Current nws ahps hydrograph_to_html url
  ahps_url <- "https://water.weather.gov/ahps2/hydrograph_to_xml.php"

  # Construct the URL
  gage_url <- paste0(ahps_url, "?gage=", gage_id, "&output=xml")

  # Construct the output file
  gage_file <- file.path(path, paste0(gage_id, ".xml"))

  # Download the file
  download.file(gage_url, gage_file)

  return(gage_file)
}
