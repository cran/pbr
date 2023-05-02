nm <- "Foobar"
addr <- "1000 South Street"
cty <- "Milwaukee"
st <- "WI"
zip <- "53702"

exp_leaf_label <- paste(
  "<strong>", nm, "</strong> <br>",
  addr, "<br>",
  paste0(cty, ", ", st, " ", zip)
)
single_label <- lapply(exp_leaf_label, htmltools::HTML)
multi_labels <- lapply(1:4, function(x) {
  htmltools::HTML(exp_leaf_label)
})

test_that("format_leaflet_lables works correctly", {
  expect_equal(format_leaflet_labels(nm, addr, cty, st, zip), single_label)
  expect_equal(
    format_leaflet_labels(rep(nm, 4), addr, cty, st, zip),
    multi_labels
  )
})
