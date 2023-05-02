#' Retrieve and print interactive map of closest locations
#'
#' These functions will retrieve and display the locations of the closest
#' outlets for a cold one. \code{pbr_me} requires a location to be input. This is handy for
#' when you're going somewhere and want to scout out the available outlets that
#' sell what you're looking for. \code{pbr_me_asap} is for when you just need a one now and don't
#' have time to enter your zip code.
#'
#' \code{pbr_me} will display a \code{\link[leaflet]{leaflet}} map of the
#' closest retailer locations. If \code{map = FALSE} it will return a data.frame
#' of the locations. \code{pbr_me_asap} displays a leaflet map of retailer locations
#' within your current proximity for those moments when you just need a cold one now
#'
#' @param location A zip code, city name, or regular expression of such
#' @param map Logical. Output a leaflet map (TRUE, default) or not (FALSE)
#' @param ... Additional arguments passed on to \code{\link{format_pbr_url}}
#'
#' @return A leaflet map displaying closest retailer locations, or (optionally) a
#' data.frame of retailer locations
#' @export
#'
#' @name pbr_me
#'
#' @examples
#' \donttest{
#' pbr_me(54481)
#' pbr_me_asap() # for when you just don't have time to enter your zip code
#' }
pbr_me <- function(location, map = TRUE, ...) {
  pbr_locations <- pbr_query(location, ...)
  if (interactive() && map) {
    leaflet_map <-
      leaflet::leaflet(pbr_locations) |>
      leaflet::addTiles() |>
      leaflet::addMarkers(
        ~lon, ~lat,
        label = ~format_leaflet_labels(name, address, city, state, postalCode)
      )
    leaflet_map
  } else {
    return(pbr_locations)
  }
}

#' @rdname pbr_me
#' @export
pbr_me_asap <- function() {
  zip <- get_zip()
  return(pbr_me(zip))
}
