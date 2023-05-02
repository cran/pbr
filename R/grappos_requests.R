grappos_api_url <- "https://api.grappos.com/api/search/"
grappos_retailers_url <- "https://api-v2.grappos.com/restapi/v1/retailers"

#' Query information about a location
#'
#' This function is used to find the information needed for an entire GET
#' request url based on just the zip code or city name (or regex)
#'
#' @param location Zip code or (portion of) city name
#'
#' @return A data.frame with city name, zip code, lat, and long
location_query <- function(location) {
  raw_response <- httr::GET(
    paste0(grappos_api_url, "?q=", as.character(location))
  )
  if (raw_response$status_code != 200) {
    stop("Could not find that location. Please enter a valid city or ZIP code.")
  } else {
    response <- jsonlite::fromJSON(rawToChar(raw_response$content))
    return(response)
  }
}

#' Formats url based on the location provided
#'
#' This function simply readies the url for use in a GET request
#'
#' @inheritParams location_query
#' @param dist Numeric. The distance in miles to search from the location
#' @param lim Numeric. The number of results to be returned
#' @param brand_id Numeric. The brand ID to be returned
#'
#' @return A url to be passed to \code{\link{pbr_query}}
format_pbr_url <- function(location, dist = 100, lim = 50, brand_id = 333) {
  location_info <- location_query(location)
  if (nrow(location_info) > 1) {
    city_names <- lapply(1:nrow(location_info), function(x) {
      tmp <- location_info[x, ]
      paste0(x, ": ", tmp$name, ", ", tmp$state, " ", tmp$postal_code)
    })
    cat(
      "There were multiple locations returned. Please select one: \n",
      paste("\t", city_names, collapse = "\n")
    )
    selection <- readline()
    if (!(selection %in% 1:nrow(location_info))) {
      stop("Invalid selection")
    } else {
      location_info <- location_info[selection, ]
    }
  }
  li <- function(var) {
    return(location_info[[var]])
  }
  lat <- li("lat")
  lon <- li("lon")
  zip <- li("postal_code")
  city <- li("name")
  state = li("state")
  query_url <- paste0(
    paste0(grappos_retailers_url, "?"),
    paste0("lat=", lat, "&"),
    paste0("lon=", lon, "&"),
    paste0("zip=", zip, "&"),
    paste0("city=", gsub(" ", "+", city), "&"),
    paste0("state=", state, "&"),
    paste0("distance=", dist, "&"),
    paste0("limit=", lim, "&"),
    paste0("brandID=", brand_id)
  )
  return(query_url)
}

#' Query for locations that sell cold ones
#'
#' @inheritParams location_query
#' @param ... Additional arguments to be passed to \code{\link{format_pbr_url}}
#'
#' @return A data.frame of retailers
pbr_query <- function(location, ...) {
  query_url <- format_pbr_url(location, ...)
  response_raw <- httr::GET(
    query_url,
    httr::add_headers(
      authority = 'api-v2.grappos.com',
      accept = 'application/json, text/plain, */*',
      `accept-language` = 'en-US,en;q=0.9',
      origin = 'https://locator.grappos.com',
      referer = 'https://locator.grappos.com/',
      `sec-ch-ua` = '" Not A;Brand";v="99", "Chromium";v="100", "Google Chrome";v="100"',
      `sec-ch-ua-mobile` = '?1',
      `sec-ch-ua-platform` = '"Android"',
      `sec-fetch-dest` = 'empty',
      `sec-fetch-mode` = 'cors',
      `sec-fetch-site` = 'same-site',
      `uid` = 'oL-685325132',
      `user-agent` = 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.88 Mobile Safari/537.36'
    )
  )
  if (response_raw$status_code != 200) {
    stop(
      "Something went wrong. The request returned a status of ",
      response_raw$status_code
    )
  } else {
    response <- jsonlite::fromJSON(rawToChar(response_raw$content))
    if ("accounts" %in% names(response)) {
      return(response$accounts)
    }
  }
}
