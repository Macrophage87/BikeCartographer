# Static export of leaflet maps via {webshot2} and headless Chrome.
# The map widget is written to a self-contained HTML file with
# {htmlwidgets} and captured with {webshot2}. This deliberately avoids
# {mapview}: mapview pulls a large spatial/graphics subtree (stars,
# terra, leafem, leafpop, satellite, and the legacy PhantomJS-based
# webshot) that is unnecessary here and slows and destabilises
# deployment installs.

#' Export a leaflet map to a PNG at exact pixel dimensions
#'
#' Writes `map` to a self-contained HTML file and renders it in a
#' headless Chrome viewport of `width` by `height` CSS pixels via
#' [webshot2::webshot()]. The saved image measures `width * zoom` by
#' `height * zoom` device pixels, so `zoom = 1` matches a platform
#' specification exactly and `zoom = 2` produces a retina (2x) render
#' of the same layout.
#'
#' Zoom and layer controls are hidden with injected CSS before capture;
#' the tile attribution is deliberately kept, since most tile providers
#' require it even in static exports.
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

  # Hide interactive controls for a clean frame; keep attribution.
  hide_css <- htmltools::tags$style(
    htmltools::HTML(
      paste(
        ".leaflet-control-zoom,",
        ".leaflet-control-layers,",
        ".leaflet-control-easyButton,",
        ".leaflet-draw { display: none !important; }"
      )
    )
  )
  map <- htmlwidgets::prependContent(map, hide_css)

  # saveWidget cannot write into an arbitrary temp path without moving
  # its dependency directory, so render inside a dedicated temp dir.
  work_dir <- tempfile("mapshot_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  html_path <- file.path(work_dir, "map.html")

  htmlwidgets::saveWidget(
    map,
    file = html_path,
    selfcontained = FALSE
  )
  webshot2::webshot(
    url = html_path,
    file = file,
    vwidth = as.integer(width),
    vheight = as.integer(height),
    zoom = zoom,
    delay = delay
  )
  invisible(file)
}
