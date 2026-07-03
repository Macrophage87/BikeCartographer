# Elevation profile rendering. The profile is drawn as a small inline
# SVG panel built by plain string construction (no plotting packages,
# so nothing is added to the deployment manifest) and attached to the
# leaflet map as a custom control, which means it appears both in the
# interactive view and in exported PNGs: the export CSS hides only the
# zoom/layers controls, not custom ones.

#' Downsample an elevation profile for compact SVG output
#'
#' @param profile A `distance_m`/`elevation_m` data frame.
#' @param max_points Integer scalar. Maximum rows to keep.
#'
#' @return A data frame with at most `max_points + 1` rows, keeping
#'   the first and last points.
downsample_profile <- function(profile, max_points = 400L) {
  n <- nrow(profile)
  if (n <= max_points) {
    return(profile)
  }
  keep <- unique(c(seq(1L, n, by = ceiling(n / max_points)), n))
  profile[keep, , drop = FALSE]
}

#' Summary statistics for an elevation profile
#'
#' Elevations are lightly smoothed (see [smooth_series()]) before the
#' climb total is accumulated, so GPS noise does not inflate it.
#'
#' @param profile A `distance_m`/`elevation_m` data frame.
#'
#' @return A list with `distance_mi`, `gain_ft`, `min_ft`, and
#'   `max_ft`.
elevation_stats <- function(profile) {
  ele <- smooth_series(profile$elevation_m)
  gain_m <- sum(pmax(diff(ele), 0))
  list(
    distance_mi = max(profile$distance_m) / 1609.344,
    gain_ft = gain_m * 3.28084,
    min_ft = min(ele) * 3.28084,
    max_ft = max(ele) * 3.28084
  )
}

#' Render an elevation profile as an inline SVG panel
#'
#' Produces a semi-transparent panel with a filled area chart in the
#' track colour and a one-line summary (distance, total climb, and
#' elevation range in imperial units). Returns `NULL` for unusable
#' input rather than erroring, so callers can simply skip the panel.
#'
#' @param profile A `distance_m`/`elevation_m` data frame.
#' @param width,height Integer scalars. Panel size in CSS pixels.
#' @param line_color Character scalar. Hex colour matching the track.
#' @param scale Numeric scalar. Multiplier applied to the panel size,
#'   fonts, and stroke (1 = default; larger = bigger panel).
#'
#' @return Character scalar of SVG markup, or `NULL`.
elevation_profile_svg <- function(profile, width = 360L,
                                  height = 90L,
                                  line_color = "#E8552F",
                                  scale = 1) {
  if (is.null(profile) || !is.data.frame(profile) ||
        !all(c("distance_m", "elevation_m") %in% names(profile))) {
    return(NULL)
  }
  if (is.null(scale) || !is.finite(scale) || scale <= 0) {
    scale <- 1
  }
  width <- as.integer(round(width * scale))
  height <- as.integer(round(height * scale))
  usable <- stats::complete.cases(
    profile[, c("distance_m", "elevation_m")]
  )
  profile <- profile[usable, , drop = FALSE]
  if (nrow(profile) < 2L || max(profile$distance_m) <= 0) {
    return(NULL)
  }
  stats <- elevation_stats(profile)
  profile <- downsample_profile(profile)
  ele <- smooth_series(profile$elevation_m)

  pad_top <- 22 * scale
  pad_bottom <- 6 * scale
  pad_x <- 6 * scale
  font_size <- 11 * scale
  text_y <- 15 * scale
  stroke_w <- 1.8 * scale
  plot_w <- width - 2 * pad_x
  plot_h <- height - pad_top - pad_bottom
  x <- pad_x + plot_w * profile$distance_m / max(profile$distance_m)
  ele_range <- range(ele)
  span <- max(ele_range[2L] - ele_range[1L], 1)
  y <- pad_top + plot_h * (1 - (ele - ele_range[1L]) / span)
  pts <- paste(sprintf("%.1f,%.1f", x, y), collapse = " ")
  base_y <- pad_top + plot_h
  area <- paste(
    sprintf("%.1f,%.1f", x[1L], base_y),
    pts,
    sprintf("%.1f,%.1f", x[length(x)], base_y)
  )
  label <- sprintf(
    "%.1f mi \u00b7 +%s ft climb \u00b7 %s\u2013%s ft",
    stats$distance_mi,
    format(round(stats$gain_ft), big.mark = ","),
    format(round(stats$min_ft), big.mark = ","),
    format(round(stats$max_ft), big.mark = ",")
  )
  sprintf(
    paste0(
      "<svg xmlns=\"http://www.w3.org/2000/svg\" ",
      "width=\"%d\" height=\"%d\" ",
      "style=\"background: rgba(255,255,255,0.88); ",
      "border-radius: 6px; display: block;\">",
      "<text x=\"%.1f\" y=\"%.1f\" ",
      "font-family=\"Helvetica, Arial, sans-serif\" ",
      "font-size=\"%.1f\" fill=\"#333333\">%s</text>",
      "<polygon points=\"%s\" fill=\"%s\" fill-opacity=\"0.25\"/>",
      "<polyline points=\"%s\" fill=\"none\" stroke=\"%s\" ",
      "stroke-width=\"%.1f\"/>",
      "</svg>"
    ),
    width, height, pad_x, text_y, font_size, label,
    area, line_color, pts, line_color, stroke_w
  )
}
