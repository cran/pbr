## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(pbr)
library(htmltools)

## ----pbr_me, eval = FALSE-----------------------------------------------------
#  pbr_me("Milwaukee, WI")

## ----pbr_me_zip, eval = FALSE-------------------------------------------------
#  pbr_me(53210)

## ----pbr_me_asap, eval = FALSE------------------------------------------------
#  pbr_me_asap()

## ----milwaukee_pbr, echo = FALSE, fig.height = 8, fig.width = 8---------------
format_leaf_lab <- function(name, address, city, state, zip) {
  out <- paste(
    "<strong>", name, "</strong>",
    "<br>",
    address,
    "<br>",
    paste0(city, ", ", state, " ", zip)
  )
  return(lapply(out, htmltools::HTML))
}
leaflet::leaflet(data = milwaukee) |>
  leaflet::addTiles() |>
  leaflet::addMarkers(
    ~lon, ~lat,
    label = ~format_leaf_lab(name, address, city, state, zip)
  )

