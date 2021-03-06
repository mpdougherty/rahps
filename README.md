<!-- README.md is generated from README.Rmd. Please edit that file -->

# rahps

An `R` package for data access and reporting for the National Weather
Service (NWS) Advanced Hydrologic Prediction Service (AHPS) river gage
forcasts.

## Package Status

[![Maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle)
[![Project Status: Active The project has reached a stable, usable state
and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.1-orange.svg?style=flat-square)](commits/master)
[![Last-changedate](https://img.shields.io/badge/last%20change-2021--05--18-yellowgreen.svg)](/commits/master)
[![Licence](https://img.shields.io/badge/licence-CC0-blue.svg)](http://choosealicense.com/licenses/cc0-1.0/)

## Latest Updates

Check out the [NEWS](NEWS.md) for details on the latest updates.

## Authors

-   Michael Dougherty, Geographer, U.S. Army Corps of Engineers
    <a itemprop="sameAs" content="https://orcid.org/0000-0002-1465-5927" href="https://orcid.org/0000-0002-1465-5927" target="orcid.widget" rel="me noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">https://orcid.org/0000-0002-1465-5927</a>

## Install

To install the `fluvgeo` package, install from GitHub using the
`devtools` package:

    remotes::install_github(repo = "mpdougherty/rahps@*release")

## Bug Reports

If you find any bugs using `rahps`, please open an
[issue](https://github.com/mpdougherty/rahps/issues).

## Getting Started

Often users want to know the river forecast for a specific gage or set
of gages. The National Weather Service (NWS) Advanced Hydrologic
Prediction Service (AHPS) provides a website to view these results
<https://water.weather.gov/ahps/>. Each dot on the map refers to a
single gage. When you mouse-over any point, it displays the 5-digit gage
identifier in the popup. This gage unique identifier can be used to
request a river gage forecast from thir web service.

In `rahps`, use the `download_forecast` function to retrieve the `.xml`
file containing the current forecast for a gage.

    gage_id <- "canm7"
    path <- Sys.getenv("HOME")

    gage_file <- rahps::download_forecast(gage_id, path)

Use the `htmltidy::xml_view` function to view the `.xml` file.

    library(xml2)
    library(htmltidy)

    # View the xml file
    htmltidy::xml_view(xml2::read_xml(gage_file))

Use the `gage_forecast` to extract the information from the `.xml` file
into a data frame.

    gage_forecast_df <- get_gage_forecast(gage_id, gage_file)
