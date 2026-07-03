# Static export of leaflet maps via {mapview} and headless Chrome.

#' Export a leaflet map to a PNG at exact pixel dimensions
#'
#' Renders `map` in a headless browser viewport of `width` by `height`
#' CSS pixels and captures it with `mapview::mapshot2()`, falling back
#' to the legacy `mapview::mapshot()` on older mapview installations.
#' The saved image measures `width * zoom` by `height * zoom` device
#' pixels, so `zoom = 1` matches a platform specification exactly and
#' `zoom = 2` produces a retina (2x) render of the same layout.
#'
#' Zoom and layer controls are stripped from the capture; the tile
#' attribution is deliberately kept, since most tile providers require
#' it even in static exports.
#'
#' @param map A `leaflet` htmlwidget.
#' @param file Character scalar. Output path ending in `.png`.
#' @param width,height Integer scalars. Viewport size in CSS pixels.
#' @param zoom Numeric scalar. Device-pixel multiplier (default 1).
#' @param delay Numeric scalar. Seconds to wait before capture so tile
#'   layers can finish loading (default 2).
#'
#' @return Invisibly, `file`.
export_map_png <- function(map, file, width, height,
                           zoom = 1, delay = 2) {
  stopifnot(grepl("\\.png$", file, ignore.case = TRUE))
  shot_args <- list(
    x = map,
    file = file,
    vwidth = as.integer(width),
    vheight = as.integer(height),
    zoom = zoom,
    delay = delay,
    remove_controls = c(
      "zoomControl", "layersControl", "homeButton", "drawToolbar",
      "easyButton"
    )
  )
  shot_fun <- if ("mapshot2" %in% getNamespaceExports("mapview")) {
    mapview::mapshot2
  } else {
    mapview::mapshot
  }
  do.call(shot_fun, shot_args)
  invisible(file)
}
