# Ride with GPS import. Uses the documented v1 JSON API
# (https://ridewithgps.com/api/v1) with HTTP Basic authentication: the
# API key is the username and a user-created auth token is the
# password, so no account password ever touches this app. Route/trip
# detail responses are converted directly into the same layer
# structure produced by read_gpx_layers(), so the whole downstream map
# and export pipeline is reused unchanged.

#' Read the Ride with GPS API key from the environment
#'
#' Create an API client under your Ride with GPS account settings
#' (Developers tab); the client is assigned an API key. Set it as the
#' `RWGPS_API_KEY` environment variable: on Posit Connect, under the
#' content's Settings > Vars panel; locally, in `~/.Renviron`.
#'
#' @return Character scalar. The key, or `""` when unset.
rwgps_api_key <- function() {
  trimws(Sys.getenv("RWGPS_API_KEY", unset = ""))
}

#' Read the Ride with GPS auth token from the environment
#'
#' On the API client's management page, use "Create new Auth Token"
#' and set the value as the `RWGPS_AUTH_TOKEN` environment variable
#' (same locations as [rwgps_api_key()]). Tokens are created in the
#' Ride with GPS UI, so this app never handles your password.
#'
#' @return Character scalar. The token, or `""` when unset.
rwgps_auth_token <- function() {
  trimws(Sys.getenv("RWGPS_AUTH_TOKEN", unset = ""))
}

#' Is the Ride with GPS integration configured?
#'
#' @return Logical scalar. `TRUE` when both `RWGPS_API_KEY` and
#'   `RWGPS_AUTH_TOKEN` are set.
rwgps_available <- function() {
  nzchar(rwgps_api_key()) && nzchar(rwgps_auth_token())
}

#' Modal dialog for the Ride with GPS import workflow
#'
#' With credentials configured, shows the library picker with refresh
#' and import actions. Without credentials, shows step-by-step setup
#' instructions for creating an API client and auth token and setting
#' the environment variables, so the feature stays discoverable
#' instead of silently hidden.
#'
#' @param configured Logical scalar. Result of [rwgps_available()].
#' @param choices Named list from [rwgps_item_choices()], or `NULL`
#'   before the first successful fetch.
#' @param selected Character scalar or `NULL`. Previously selected
#'   item value, preserved across modal reopenings.
#'
#' @return A `shiny::modalDialog()` tag.
rwgps_import_modal <- function(configured, choices = NULL,
                               selected = NULL) {
  if (!configured) {
    return(shiny::modalDialog(
      title = "Connect Ride with GPS",
      shiny::tags$p(paste(
        "Importing from your Ride with GPS library needs an API key",
        "and an auth token. Your account password is never used by",
        "this app."
      )),
      shiny::tags$ol(
        shiny::tags$li(paste(
          "In your Ride with GPS account settings, open the",
          "Developers tab and create an API client; copy its API",
          "key."
        )),
        shiny::tags$li(paste(
          "On that API client's management page, click \"Create new",
          "Auth Token\" and copy the token."
        )),
        shiny::tags$li(paste(
          "Set the RWGPS_API_KEY and RWGPS_AUTH_TOKEN environment",
          "variables (Posit Connect: content Settings > Vars;",
          "locally: ~/.Renviron), then restart the app."
        ))
      ),
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
    ))
  }
  if (length(choices) == 0L) {
    choices <- c("Nothing loaded yet - press Refresh list" = "")
  }
  shiny::modalDialog(
    title = "Import from Ride with GPS",
    shiny::selectInput(
      "rwgps_item",
      "Your library (most recently updated first)",
      choices = choices,
      selected = selected,
      width = "100%"
    ),
    shiny::actionButton("rwgps_refresh", "Refresh list"),
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(
        "rwgps_import",
        "Import to map",
        class = "btn-primary"
      )
    )
  )
}

#' Build the HTTP Basic Authorization header for Ride with GPS
#'
#' Per the v1 API documentation, the API key is used as the username
#' and the auth token as the password.
#'
#' @param api_key,auth_token Character scalars. Credentials; default
#'   to the environment variables.
#'
#' @return Character scalar, e.g. `"Basic a2V5OnRva2Vu"`.
rwgps_basic_auth_header <- function(api_key = rwgps_api_key(),
                                    auth_token = rwgps_auth_token()) {
  credentials <- paste0(api_key, ":", auth_token)
  paste("Basic", jsonlite::base64_enc(charToRaw(credentials)))
}

#' Build a Ride with GPS API URL with encoded query parameters
#'
#' @param path Character scalar. Path beginning with `/`.
#' @param query Named list of scalar query parameters.
#' @param base_url Character scalar. API host.
#'
#' @return Character scalar URL.
rwgps_build_url <- function(path, query = list(),
                            base_url = "https://ridewithgps.com") {
  stopifnot(is.character(path), length(path) == 1L)
  url <- paste0(base_url, path)
  if (length(query) == 0L) {
    return(url)
  }
  encoded <- vapply(
    query,
    function(x) utils::URLencode(as.character(x), reserved = TRUE),
    character(1L)
  )
  pairs <- paste(names(encoded), encoded, sep = "=")
  paste0(url, "?", paste(pairs, collapse = "&"))
}

#' Perform an authenticated GET against the Ride with GPS API
#'
#' Error-safe by construction: missing credentials, network failures,
#' non-2xx responses, and unparsable bodies all yield `NULL` rather
#' than an error.
#'
#' @param path Character scalar. API path, e.g.
#'   `"/api/v1/routes.json"`.
#' @param query Named list of scalar query parameters.
#'
#' @return Parsed JSON (lists/data frames via
#'   [jsonlite::fromJSON()]), or `NULL` on any failure.
rwgps_get_json <- function(path, query = list()) {
  if (!rwgps_available()) {
    return(NULL)
  }
  url <- rwgps_build_url(path, query)
  dest <- tempfile(fileext = ".json")
  on.exit(unlink(dest), add = TRUE)
  status <- tryCatch(
    suppressWarnings(utils::download.file(
      url,
      destfile = dest,
      quiet = TRUE,
      mode = "wb",
      headers = c(
        Authorization = rwgps_basic_auth_header(),
        Accept = "application/json"
      )
    )),
    error = function(e) -1L
  )
  if (!identical(status, 0L) || !file.exists(dest) ||
        file.size(dest) == 0) {
    return(NULL)
  }
  tryCatch(jsonlite::fromJSON(dest), error = function(e) NULL)
}

#' List the user's Ride with GPS routes or trips
#'
#' Fetches the first page of the authenticated user's library, which
#' the API orders by `updated_at` descending (most recently touched
#' items first). Only items owned by the account are returned.
#'
#' @param kind Character scalar. `"routes"` (planned) or `"trips"`
#'   (recorded rides).
#' @param page_size Integer scalar. Items per page; the API accepts
#'   20 to 200.
#'
#' @return A data frame with `id`, `name`, and (when provided)
#'   `distance` in metres, or `NULL` on failure or an empty library.
rwgps_list_items <- function(kind = c("routes", "trips"),
                             page_size = 100L) {
  kind <- match.arg(kind)
  payload <- rwgps_get_json(
    sprintf("/api/v1/%s.json", kind),
    query = list(page = 1L, page_size = page_size)
  )
  items <- payload[[kind]]
  if (is.null(items) || !is.data.frame(items) || nrow(items) == 0L) {
    return(NULL)
  }
  wanted <- intersect(c("id", "name", "distance"), names(items))
  if (!all(c("id", "name") %in% wanted)) {
    return(NULL)
  }
  items[, wanted, drop = FALSE]
}

#' Grouped selectInput choices for Ride with GPS items
#'
#' Values encode the item as `"<kind>:<id>"` (for example
#' `"routes:12345"`); labels show the name and distance in miles.
#'
#' @param routes,trips Data frames from [rwgps_list_items()], or
#'   `NULL`.
#'
#' @return A named list of named character vectors (rendered as
#'   option groups), empty when nothing is available.
rwgps_item_choices <- function(routes = NULL, trips = NULL) {
  label_for <- function(items) {
    miles <- if ("distance" %in% names(items)) {
      suppressWarnings(as.numeric(items$distance)) / 1609.344
    } else {
      rep(NA_real_, nrow(items))
    }
    ifelse(
      is.na(miles),
      as.character(items$name),
      sprintf("%s (%.0f mi)", items$name, miles)
    )
  }
  choices <- list()
  if (!is.null(routes) && nrow(routes) > 0L) {
    choices[["Routes (planned)"]] <- stats::setNames(
      paste0("routes:", routes$id),
      label_for(routes)
    )
  }
  if (!is.null(trips) && nrow(trips) > 0L) {
    choices[["Rides (recorded)"]] <- stats::setNames(
      paste0("trips:", trips$id),
      label_for(trips)
    )
  }
  choices
}

#' Fetch the detail payload for one route or trip
#'
#' @param kind Character scalar. `"routes"` or `"trips"`.
#' @param item_id Character or numeric scalar id.
#'
#' @return The inner detail list (unwrapped from its `route`/`trip`
#'   envelope), or `NULL` on failure.
rwgps_fetch_detail <- function(kind, item_id) {
  payload <- rwgps_get_json(
    sprintf("/api/v1/%s/%s.json", kind, item_id)
  )
  if (is.null(payload)) {
    return(NULL)
  }
  singular <- sub("s$", "", kind)
  detail <- payload[[singular]]
  if (is.null(detail)) payload else detail
}

#' Thin a coordinate matrix to a maximum number of rows
#'
#' Recorded trips can carry one point per second (tens of thousands of
#' vertices on a long ride), which slows the browser and the headless
#' export. Evenly subsampled points preserve the visual shape at map
#' scales while keeping the first and last points.
#'
#' @param coords Two-column numeric matrix.
#' @param max_points Integer scalar. Maximum rows to keep.
#'
#' @return A matrix with at most `max_points + 1` rows.
thin_coordinate_matrix <- function(coords, max_points = 20000L) {
  n <- nrow(coords)
  if (n <= max_points) {
    return(coords)
  }
  keep <- unique(c(seq(1L, n, by = ceiling(n / max_points)), n))
  coords[keep, , drop = FALSE]
}

#' Convert Ride with GPS track points to an sf line
#'
#' Track points use `x` for longitude and `y` for latitude (with
#' `lng`/`lat` accepted as a fallback), per the API reference.
#'
#' @param track_points Data frame of track points from a detail
#'   response.
#' @param name Character scalar. Name attribute for the feature.
#'
#' @return A one-row `sf` LINESTRING in EPSG:4326, or `NULL` when the
#'   points are missing or unusable.
rwgps_track_to_sf <- function(track_points, name = "Track") {
  if (is.null(track_points) || !is.data.frame(track_points)) {
    return(NULL)
  }
  cols <- if (all(c("x", "y") %in% names(track_points))) {
    c("x", "y")
  } else if (all(c("lng", "lat") %in% names(track_points))) {
    c("lng", "lat")
  } else {
    return(NULL)
  }
  coords <- suppressWarnings(data.matrix(track_points[, cols]))
  coords <- coords[stats::complete.cases(coords), , drop = FALSE]
  if (nrow(coords) < 2L) {
    return(NULL)
  }
  coords <- thin_coordinate_matrix(coords)
  geometry <- sf::st_sfc(sf::st_linestring(coords), crs = 4326)
  sf::st_sf(name = name, geometry = geometry)
}

#' Convert Ride with GPS points of interest to sf waypoints
#'
#' POIs carry `name`, `description`, `lat`, and `lng` attributes.
#' Named POIs feed the app's named-waypoint icon markers.
#'
#' @param pois Data frame of points of interest from a route detail
#'   response.
#'
#' @return An `sf` POINT object in EPSG:4326 with a `name` column, or
#'   `NULL` when there are no usable POIs.
rwgps_pois_to_sf <- function(pois) {
  if (is.null(pois) || !is.data.frame(pois) || nrow(pois) == 0L) {
    return(NULL)
  }
  if (!all(c("lat", "lng") %in% names(pois))) {
    return(NULL)
  }
  keep <- intersect(c("name", "description", "lat", "lng"), names(pois))
  pois <- pois[, keep, drop = FALSE]
  usable <- stats::complete.cases(pois[, c("lat", "lng")])
  pois <- pois[usable, , drop = FALSE]
  if (nrow(pois) == 0L) {
    return(NULL)
  }
  if (!"name" %in% names(pois)) {
    pois$name <- NA_character_
  }
  sf::st_as_sf(pois, coords = c("lng", "lat"), crs = 4326)
}

#' Build an elevation profile from Ride with GPS track points
#'
#' Uses the `e` (elevation, metres) attribute, with distance taken
#' from the API's cumulative `d` attribute when present and otherwise
#' accumulated by haversine over the coordinates.
#'
#' @param track_points Data frame of track points from a detail
#'   response.
#'
#' @return A `distance_m`/`elevation_m` data frame, or `NULL` when
#'   elevations are absent or unusable.
rwgps_elevation_profile <- function(track_points) {
  if (is.null(track_points) || !is.data.frame(track_points) ||
        !"e" %in% names(track_points)) {
    return(NULL)
  }
  ele <- suppressWarnings(as.numeric(track_points$e))
  dist <- if ("d" %in% names(track_points)) {
    suppressWarnings(as.numeric(track_points$d))
  } else if (all(c("x", "y") %in% names(track_points))) {
    cumulative_distance_m(
      suppressWarnings(as.numeric(track_points$x)),
      suppressWarnings(as.numeric(track_points$y))
    )
  } else {
    NULL
  }
  if (is.null(dist)) {
    return(NULL)
  }
  ok <- !is.na(ele) & !is.na(dist)
  if (sum(ok) < 2L) {
    return(NULL)
  }
  data.frame(distance_m = dist[ok], elevation_m = ele[ok])
}

#' Convert a detail payload into the app's GPX layer structure
#'
#' Produces the same `list(tracks, routes, waypoints, elevation)`
#' shape as [read_gpx_layers()], so imported items flow through the
#' existing map, icon, bounds, elevation, and export pipeline
#' unchanged. Planned routes land in the `routes` slot (drawn dashed)
#' and recorded trips in the `tracks` slot (drawn solid).
#'
#' @param detail Detail list from [rwgps_fetch_detail()].
#' @param kind Character scalar. `"routes"` or `"trips"`.
#'
#' @return A GPX layer list, or `NULL` when no usable track exists.
rwgps_as_gpx_layers <- function(detail, kind) {
  if (is.null(detail) || !is.list(detail)) {
    return(NULL)
  }
  item_name <- detail$name
  if (is.null(item_name) || !nzchar(trimws(item_name))) {
    item_name <- if (identical(kind, "routes")) "Route" else "Ride"
  }
  line <- rwgps_track_to_sf(detail$track_points, name = item_name)
  if (is.null(line)) {
    return(NULL)
  }
  waypoints <- rwgps_pois_to_sf(detail$points_of_interest)
  elevation <- rwgps_elevation_profile(detail$track_points)
  if (identical(kind, "routes")) {
    list(
      tracks = NULL,
      routes = line,
      waypoints = waypoints,
      elevation = elevation
    )
  } else {
    list(
      tracks = line,
      routes = NULL,
      waypoints = waypoints,
      elevation = elevation
    )
  }
}

#' Import one Ride with GPS item as GPX layers
#'
#' @param choice_value Character scalar of the form `"<kind>:<id>"`,
#'   as produced by [rwgps_item_choices()]. Malformed values return
#'   `NULL` rather than erroring.
#'
#' @return A list with elements `layers` (see
#'   [rwgps_as_gpx_layers()]) and `label`, or `NULL` on any failure.
rwgps_import_layers <- function(choice_value) {
  parts <- strsplit(choice_value, ":", fixed = TRUE)[[1L]]
  valid <- length(parts) == 2L &&
    parts[[1L]] %in% c("routes", "trips") &&
    grepl("^[0-9]+$", parts[[2L]])
  if (!valid) {
    return(NULL)
  }
  detail <- rwgps_fetch_detail(parts[[1L]], parts[[2L]])
  layers <- rwgps_as_gpx_layers(detail, parts[[1L]])
  if (is.null(layers)) {
    return(NULL)
  }
  label <- detail$name
  if (is.null(label) || !nzchar(trimws(label))) {
    label <- paste("Ride with GPS item", parts[[2L]])
  }
  list(layers = layers, label = label)
}
