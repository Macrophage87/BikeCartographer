# Utilities for importing GPX files with {sf}. Functions are
# namespace-qualified so the file can be sourced standalone; Shiny
# (>= 1.5) auto-sources everything in R/ before running app.R.

#' Ensure a file path carries a .gpx extension
#'
#' GDAL selects the GPX driver by file extension, but files staged by
#' `shiny::fileInput()` are not guaranteed to keep one. When the path
#' lacks a `.gpx` suffix, the file is copied to a sibling path that has
#' it.
#'
#' @param path Character scalar. Path to an existing file on disk.
#'
#' @return Character scalar. A path ending in `.gpx` (possibly a copy
#'   of the original file).
ensure_gpx_extension <- function(path) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))
  if (grepl("\\.gpx$", path, ignore.case = TRUE)) {
    return(path)
  }
  gpx_path <- paste0(path, ".gpx")
  if (!file.exists(gpx_path)) {
    file.copy(path, gpx_path, overwrite = FALSE)
  }
  gpx_path
}

#' Read the vector layers of a GPX file
#'
#' Reads the `tracks`, `routes`, and `waypoints` layers of a GPX file,
#' silently skipping layers that are absent or empty, and derives an
#' elevation profile from the `track_points` (or `route_points`)
#' layer's `ele` field when available. Z/M dimensions are dropped from
#' geometries so downstream leaflet calls receive plain 2D
#' coordinates.
#'
#' @param path Character scalar. Path to a GPX file.
#'
#' @return A named list with elements `tracks`, `routes`, and
#'   `waypoints` (each an `sf` object in EPSG:4326 or `NULL`), plus
#'   `elevation` (a `distance_m`/`elevation_m` data frame from
#'   [gpx_points_to_profile()], or `NULL`).
read_gpx_layers <- function(path) {
  path <- ensure_gpx_extension(path)
  available <- tryCatch(
    sf::st_layers(path)$name,
    error = function(e) character(0L)
  )
  read_one <- function(layer) {
    if (!layer %in% available) {
      return(NULL)
    }
    out <- tryCatch(
      suppressWarnings(sf::st_read(path, layer = layer, quiet = TRUE)),
      error = function(e) NULL
    )
    if (is.null(out) || nrow(out) == 0L) {
      return(NULL)
    }
    sf::st_zm(out, drop = TRUE, what = "ZM")
  }
  points <- read_one("track_points")
  if (is.null(points)) {
    points <- read_one("route_points")
  }
  list(
    tracks = read_one("tracks"),
    routes = read_one("routes"),
    waypoints = read_one("waypoints"),
    elevation = gpx_points_to_profile(points)
  )
}

#' Split waypoints into named and unnamed sets
#'
#' A waypoint counts as "named" when its `name` attribute is present,
#' non-`NA`, and non-blank after trimming whitespace.
#'
#' @param waypoints An `sf` object of GPX waypoints, or `NULL`.
#'
#' @return A list with elements `named` and `unnamed`, each an `sf`
#'   object or `NULL` when its subset is empty.
split_named_waypoints <- function(waypoints) {
  if (is.null(waypoints) || !"name" %in% names(waypoints)) {
    return(list(named = NULL, unnamed = waypoints))
  }
  nm <- trimws(as.character(waypoints$name))
  is_named <- !is.na(nm) & nzchar(nm)
  keep <- function(idx) {
    if (any(idx)) waypoints[idx, , drop = FALSE] else NULL
  }
  list(named = keep(is_named), unnamed = keep(!is_named))
}

#' Extract display names for map features
#'
#' @param x An `sf` object that may carry a `name` attribute column.
#' @param fallback Character scalar substituted for missing or blank
#'   names.
#'
#' @return Character vector of length `nrow(x)`.
feature_names <- function(x, fallback = "Feature") {
  if (!"name" %in% names(x)) {
    return(rep(fallback, nrow(x)))
  }
  nm <- trimws(as.character(x$name))
  ifelse(is.na(nm) | !nzchar(nm), fallback, nm)
}

#' Count the features held in a GPX layer list
#'
#' @param gpx A list as returned by [read_gpx_layers()].
#'
#' @return Named integer vector with counts for `tracks`, `routes`,
#'   `waypoints`, and `named_waypoints`.
count_gpx_features <- function(gpx) {
  n_rows <- function(x) if (is.null(x)) 0L else nrow(x)
  named <- split_named_waypoints(gpx$waypoints)$named
  c(
    tracks = n_rows(gpx$tracks),
    routes = n_rows(gpx$routes),
    waypoints = n_rows(gpx$waypoints),
    named_waypoints = n_rows(named)
  )
}

#' Cumulative haversine distance along a coordinate sequence
#'
#' @param lon,lat Numeric vectors of equal length, in degrees
#'   (EPSG:4326).
#'
#' @return Numeric vector of cumulative metres, starting at 0.
cumulative_distance_m <- function(lon, lat) {
  n <- length(lon)
  if (n < 2L) {
    return(rep(0, n))
  }
  to_rad <- pi / 180
  lon_r <- lon * to_rad
  lat_r <- lat * to_rad
  dlat <- diff(lat_r)
  dlon <- diff(lon_r)
  a <- sin(dlat / 2)^2 +
    cos(lat_r[-n]) * cos(lat_r[-1L]) * sin(dlon / 2)^2
  segment <- 2 * 6371008.8 * asin(pmin(1, sqrt(a)))
  c(0, cumsum(segment))
}

#' Lightly smooth a numeric series with a centred moving average
#'
#' Used on elevations so GPS noise does not produce a jagged profile
#' or inflate climb totals. Edge positions (where the window is
#' incomplete) keep their original values.
#'
#' @param x Numeric vector.
#' @param k Integer scalar window width.
#'
#' @return Numeric vector the same length as `x`.
smooth_series <- function(x, k = 5L) {
  if (length(x) < k || k < 2L) {
    return(x)
  }
  smoothed <- as.numeric(stats::filter(x, rep(1 / k, k), sides = 2))
  ifelse(is.na(smoothed), x, smoothed)
}

#' Build an elevation profile from GPX point features
#'
#' Uses the `ele` field of the `track_points` (or `route_points`)
#' layer, with distance accumulated by haversine along the points.
#' Multi-segment tracks are concatenated in file order, so distances
#' bridge any gaps between segments.
#'
#' @param points An `sf` POINT object with an `ele` column, or `NULL`.
#'
#' @return A data frame with `distance_m` and `elevation_m`, or
#'   `NULL` when elevations are absent or unusable.
gpx_points_to_profile <- function(points) {
  if (is.null(points) || !"ele" %in% names(points)) {
    return(NULL)
  }
  ele <- suppressWarnings(as.numeric(points$ele))
  coords <- sf::st_coordinates(points)
  ok <- !is.na(ele) &
    stats::complete.cases(coords[, c("X", "Y"), drop = FALSE])
  if (sum(ok) < 2L) {
    return(NULL)
  }
  coords <- coords[ok, , drop = FALSE]
  data.frame(
    distance_m = cumulative_distance_m(coords[, "X"], coords[, "Y"]),
    elevation_m = ele[ok]
  )
}

#' Compute a padded bounding box across all GPX layers
#'
#' Combines the bounding boxes of every non-`NULL` layer, then pads the
#' result so linework does not touch the frame edge in exports.
#' Degenerate extents (for example, a single waypoint) are widened by a
#' small constant so the map can still frame them.
#'
#' @param gpx A list as returned by [read_gpx_layers()].
#' @param pad Numeric scalar. Fraction of the extent span added to each
#'   side (default 3%).
#'
#' @return Named numeric vector `c(xmin, ymin, xmax, ymax)` in
#'   EPSG:4326, or `NULL` when no layer contains features.
gpx_bounds <- function(gpx, pad = 0.03) {
  layers <- Filter(Negate(is.null), gpx[c("tracks", "routes", "waypoints")])
  if (length(layers) == 0L) {
    return(NULL)
  }
  boxes <- vapply(
    layers,
    function(x) as.numeric(sf::st_bbox(x)),
    numeric(4L)
  )
  bounds <- c(
    xmin = min(boxes[1L, ]),
    ymin = min(boxes[2L, ]),
    xmax = max(boxes[3L, ]),
    ymax = max(boxes[4L, ])
  )
  span_x <- max(bounds[["xmax"]] - bounds[["xmin"]], 0.005)
  span_y <- max(bounds[["ymax"]] - bounds[["ymin"]], 0.005)
  bounds + c(-1, -1, 1, 1) * pad * c(span_x, span_y, span_x, span_y)
}
