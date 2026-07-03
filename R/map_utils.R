# Map construction helpers built on {leaflet}. Basemap selection and
# API-key handling live in R/basemap_utils.R.

#' Export size presets for common social media formats
#'
#' Pixel dimensions and aspect ratios follow the current recommended
#' sizes for the major platforms.
#'
#' @return A data frame with columns `id`, `label`, `width`, `height`,
#'   and `ratio`.
social_media_presets <- function() {
  data.frame(
    id = c(
      "instagram_square", "instagram_portrait", "instagram_landscape",
      "story_9x16", "facebook_post", "facebook_cover", "x_post",
      "x_header", "linkedin_post", "youtube_thumbnail", "pinterest_pin"
    ),
    label = c(
      "Instagram post (square)", "Instagram post (portrait)",
      "Instagram post (landscape)", "Story / Reels / TikTok",
      "Facebook post", "Facebook cover", "X / Twitter post",
      "X / Twitter header", "LinkedIn post", "YouTube thumbnail",
      "Pinterest pin"
    ),
    width = c(
      1080L, 1080L, 1080L, 1080L, 1200L, 820L, 1600L, 1500L, 1200L,
      1280L, 1000L
    ),
    height = c(
      1080L, 1350L, 566L, 1920L, 630L, 312L, 900L, 500L, 627L, 720L,
      1500L
    ),
    ratio = c(
      "1:1", "4:5", "1.91:1", "9:16", "1.91:1", "2.63:1", "16:9",
      "3:1", "1.91:1", "16:9", "2:3"
    )
  )
}

#' Choices vector for the export-size select input
#'
#' @return Named character vector mapping display labels (with pixel
#'   dimensions and aspect ratio) to preset ids.
preset_choices <- function() {
  presets <- social_media_presets()
  stats::setNames(
    presets$id,
    sprintf(
      "%s \u2014 %d \u00d7 %d px (%s)",
      presets$label, presets$width, presets$height, presets$ratio
    )
  )
}

#' Build an Awesome-Markers icon for named waypoints
#'
#' @param icon Character scalar. A Font Awesome 4 icon name, for
#'   example `"map-pin"`, `"flag"`, `"star"`, `"camera"`, or
#'   `"bicycle"`.
#' @param marker_color Character scalar. Pin colour understood by
#'   Awesome Markers, for example `"red"` or `"cadetblue"`.
#'
#' @return An icon object from [leaflet::makeAwesomeIcon()].
waypoint_awesome_icon <- function(icon = "map-pin",
                                  marker_color = "red") {
  leaflet::makeAwesomeIcon(
    icon = icon,
    library = "fa",
    markerColor = marker_color,
    iconColor = "#FFFFFF"
  )
}

#' Build the main leaflet map for a GPX file
#'
#' Draws GPX tracks as solid lines, routes as dashed lines, named
#' waypoints as icon markers with name labels and popups, and unnamed
#' waypoints as small circle markers, then zooms to the combined
#' extent. With `gpx = NULL` a basemap-only view of the United States
#' is returned.
#'
#' @param gpx A list as returned by [read_gpx_layers()], or `NULL`.
#' @param basemap_id Character scalar. A basemap id from
#'   [basemap_registry()]; unknown or unavailable ids fall back to
#'   OpenStreetMap (see [add_basemap_tiles()]).
#' @param track_color Character scalar. Hex colour for tracks and
#'   routes.
#' @param track_weight Numeric scalar. Line weight in pixels.
#' @param waypoint_icon Character scalar. Font Awesome icon name used
#'   for named waypoints (see [waypoint_awesome_icon()]).
#' @param show_elevation Logical scalar. When `TRUE` and `gpx` carries
#'   an elevation profile, an SVG profile panel is attached as a
#'   bottom-left map control (see [elevation_profile_svg()]); custom
#'   controls survive the PNG export, so the panel appears there too.
#' @param elevation_scale Numeric scalar. Size multiplier for the
#'   elevation profile panel (1 = default size).
#'
#' @return A `leaflet` htmlwidget.
build_gpx_map <- function(gpx = NULL,
                          basemap_id = "OpenStreetMap.Mapnik",
                          track_color = "#E8552F",
                          track_weight = 4,
                          waypoint_icon = "map-pin",
                          show_elevation = FALSE,
                          elevation_scale = 1) {
  if (is.null(elevation_scale) || !is.finite(elevation_scale)) {
    elevation_scale <- 1
  }
  # zoomSnap = 0 lets fitBounds pick an exact fractional zoom instead of
  # rounding to whole levels. Rounding is what made the interactive
  # preview and the exported PNG frame the track differently: they are
  # different pixel sizes, so each rounded to its own zoom. With no
  # snapping the fit is scale-invariant, so identical aspect ratios (the
  # preview matches the export size) produce identical framing.
  map <- leaflet::leaflet(
    options = leaflet::leafletOptions(zoomSnap = 0)
  )
  map <- add_basemap_tiles(map, basemap_id)

  if (is.null(gpx)) {
    return(leaflet::setView(map, lng = -98.58, lat = 39.83, zoom = 4))
  }

  if (!is.null(gpx$tracks)) {
    map <- leaflet::addPolylines(
      map,
      data = gpx$tracks,
      color = track_color,
      weight = track_weight,
      opacity = 0.9,
      label = feature_names(gpx$tracks, fallback = "Track")
    )
  }

  if (!is.null(gpx$routes)) {
    map <- leaflet::addPolylines(
      map,
      data = gpx$routes,
      color = track_color,
      weight = track_weight,
      opacity = 0.9,
      dashArray = "6 8",
      label = feature_names(gpx$routes, fallback = "Route")
    )
  }

  wpts <- split_named_waypoints(gpx$waypoints)
  if (!is.null(wpts$named)) {
    labels <- feature_names(wpts$named, fallback = "Waypoint")
    map <- leaflet::addAwesomeMarkers(
      map,
      data = wpts$named,
      icon = waypoint_awesome_icon(waypoint_icon),
      label = labels,
      popup = htmltools::htmlEscape(labels)
    )
  }
  if (!is.null(wpts$unnamed)) {
    map <- leaflet::addCircleMarkers(
      map,
      data = wpts$unnamed,
      radius = 4,
      weight = 1,
      color = "#555555",
      fillColor = "#FFFFFF",
      fillOpacity = 0.9
    )
  }

  if (isTRUE(show_elevation)) {
    svg <- elevation_profile_svg(gpx$elevation,
                                 line_color = track_color,
                                 scale = elevation_scale)
    if (!is.null(svg)) {
      map <- leaflet::addControl(
        map,
        html = svg,
        position = "bottomleft",
        className = "elevation-profile"
      )
    }
  }

  bounds <- gpx_bounds(gpx)
  if (is.null(bounds)) {
    return(leaflet::setView(map, lng = -98.58, lat = 39.83, zoom = 4))
  }
  leaflet::fitBounds(
    map,
    lng1 = bounds[["xmin"]],
    lat1 = bounds[["ymin"]],
    lng2 = bounds[["xmax"]],
    lat2 = bounds[["ymax"]]
  )
}
