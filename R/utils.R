#' Format outlet name and address for use as a leaflet label
#'
#' Takes name and address and formats it into an HTML label. This is a shortcut
#' helper function that is used in \code{\link{pbr_me}}
#'
#' @param name Character. Name of the business
#' @param address Character. Address of the business
#' @param city Character. City name
#' @param state Character. State name
#' @param zip Character or numeric. Zip code
#'
#' @return An HTML label
format_leaflet_labels <- function(name, address, city, state, zip) {
  out <- paste(
    "<strong>", name, "</strong>",
    "<br>",
    address,
    "<br>",
    paste0(city, ", ", state, " ", zip)
  )
  return(lapply(out, htmltools::HTML))
}

#' Functions to retrieve IP address and ZIP code from IP
#'
#' These are just helper shortcut functions. \code{get_ip_address}
#' retrieves a computer's IP address from \url{https://ipinfo.io/what-is-my-ip}.
#' This is easier than getting the IP right off local computer because of bogon
#' IP addresses. \code{get_zip} pulls ZIP code location from \url{https://ipapi.co/}
#'
#' @return Either the IP address that a web browser sees
#' (\code{get_ip_address}), or a zip code (\code{get_zip})
#'
#' @name ip_zip
get_ip_address <- function() {
  ip_request <- httr::GET("https://ipinfo.io/what-is-my-ip")
  if (ip_request$status_code != 200) {
    stop("Could not find your location")
  } else {
    ip <- jsonlite::fromJSON(rawToChar(ip_request$content))$ip
    return(ip)
  }
}

#' @rdname ip_zip
get_zip <- function() {
  response <- httr::GET("https://ipapi.co/json/")
  if (response$status >= 400) {
    stop("Could not detect your ZIP code")
  }
  zip <- jsonlite::fromJSON(rawToChar(response$content))$postal
  return(zip)
}

