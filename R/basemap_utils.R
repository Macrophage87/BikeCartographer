# Basemap registry and error-safe tile handling. Standard providers
# need no key; Thunderforest and Stadia layers are driven by explicit
# XYZ URL templates plus API keys read from environment variables, so
# nothing here depends on the bundled leaflet-providers version.

#' Read the Thunderforest API key from the environment
#'
#' Thunderforest tiles always require an API key. Set the
#' `THUNDERFOREST_API_KEY` environment variable: on Posit Connect, add
#' it under the content's Settings > Vars panel; on Posit Connect
#' Cloud, add it as an environment variable when publishing; locally,
#' put it in `~/.Renviron` and restart R.
#'
#' @return Character scalar. The key, or `""` when unset.
thunderforest_api_key <- function() {
  trimws(Sys.getenv("THUNDERFOREST_API_KEY", unset = ""))
}

#' Read the Stadia Maps API key from the environment
#'
#' Stadia allows keyless requests from `localhost`, so local
#' development works without a key, but deployed apps need either the
#' `STADIA_API_KEY` environment variable (set the same way as
#' `THUNDERFOREST_API_KEY`, see [thunderforest_api_key()]) or a domain
#' registered in the Stadia dashboard. Note that PNG export fetches
#' tiles from headless Chrome without a browser referer, so exports of
#' Stadia basemaps generally need the key even on a local machine.
#'
#' @return Character scalar. The key, or `""` when unset.
stadia_api_key <- function() {
  trimws(Sys.getenv("STADIA_API_KEY", unset = ""))
}

#' Detect whether the app is running on a deployment server
#'
#' Posit Connect sets `RSTUDIO_PRODUCT=CONNECT`, and hosted Shiny
#' runtimes (Connect, Shiny Server, shinyapps.io) set `SHINY_PORT`.
#' When neither is present the app is treated as local, where Stadia's
#' keyless `localhost` access applies.
#'
#' @return Logical scalar.
is_deployed <- function() {
  identical(toupper(Sys.getenv("RSTUDIO_PRODUCT")), "CONNECT") ||
    nzchar(Sys.getenv("SHINY_PORT"))
}

#' Construct rows of the basemap registry
#'
#' Thin vectorised constructor used by [basemap_registry()]; all
#' arguments recycle across `id`.
#'
#' @param id Character. Stable ids used as `selectInput()` values.
#' @param label Character. Human-readable labels.
#' @param group Character. Option-group heading in the UI.
#' @param type Character. `"provider"` (leaflet provider plugin) or
#'   `"xyz"` (explicit URL template).
#' @param source Character. Provider identifier or XYZ URL template.
#' @param attribution Character. Attribution HTML/text for XYZ layers.
#' @param key Character. Key requirement: `"none"`, `"thunderforest"`,
#'   or `"stadia"`.
#' @param key_param Character. Query-parameter name carrying the key.
#' @param max_zoom Integer. Maximum tile zoom for XYZ layers.
#' @param subdomains Character. Tile subdomains, `""` when unused.
#'
#' @return A data frame with one row per basemap.
new_basemap <- function(id, label, group, type, source,
                        attribution = "", key = "none",
                        key_param = "", max_zoom = 19L,
                        subdomains = "") {
  data.frame(
    id = id,
    label = label,
    group = group,
    type = type,
    source = source,
    attribution = attribution,
    key = key,
    key_param = key_param,
    max_zoom = as.integer(max_zoom),
    subdomains = subdomains
  )
}

#' Full registry of supported basemaps
#'
#' Combines the keyless standard providers with Thunderforest and
#' Stadia layers defined as explicit XYZ URL templates (API key
#' appended at request-build time by [basemap_tile_url()]).
#'
#' @return A data frame with the columns documented in
#'   [new_basemap()].
basemap_registry <- function() {
  standard_ids <- c(
    "OpenStreetMap.Mapnik", "CartoDB.Positron", "CartoDB.DarkMatter",
    "CartoDB.Voyager", "Esri.WorldImagery", "Esri.WorldTopoMap",
    "Esri.WorldStreetMap", "Esri.NatGeoWorldMap", "USGS.USTopo",
    "USGS.USImagery", "USGS.USImageryTopo", "OpenTopoMap"
  )
  standard <- new_basemap(
    id = standard_ids,
    label = c(
      "OpenStreetMap", "CARTO Positron (light)", "CARTO Dark Matter",
      "CARTO Voyager", "Esri World Imagery (satellite)",
      "Esri World Topo", "Esri World Street Map",
      "Esri National Geographic", "USGS US Topo", "USGS US Imagery",
      "USGS US Imagery + Topo", "OpenTopoMap"
    ),
    group = "Standard (no API key)",
    type = "provider",
    source = standard_ids
  )

  tf_variants <- c(
    "cycle", "outdoors", "landscape", "transport", "atlas", "spinal-map"
  )
  thunderforest <- new_basemap(
    id = paste0("tf_", tf_variants),
    label = c(
      "Thunderforest OpenCycleMap", "Thunderforest Outdoors",
      "Thunderforest Landscape", "Thunderforest Transport",
      "Thunderforest Atlas", "Thunderforest Spinal Map"
    ),
    group = "Thunderforest (API key)",
    type = "xyz",
    source = paste0(
      "https://{s}.tile.thunderforest.com/",
      tf_variants,
      "/{z}/{x}/{y}.png"
    ),
    attribution = paste0(
      "Maps \u00a9 Thunderforest, ",
      "Data \u00a9 OpenStreetMap contributors"
    ),
    key = "thunderforest",
    key_param = "apikey",
    max_zoom = 20L,
    subdomains = "abc"
  )

  stadia_variants <- c(
    "alidade_smooth", "alidade_smooth_dark", "outdoors", "osm_bright",
    "stamen_toner", "stamen_terrain", "stamen_watercolor"
  )
  stadia_ext <- c(
    "png", "png", "png", "png", "png", "png", "jpg"
  )
  stadia_core_credit <- paste0(
    "\u00a9 OpenMapTiles \u00a9 OpenStreetMap contributors"
  )
  stadia_attribution <- ifelse(
    grepl("^stamen_", stadia_variants),
    paste("\u00a9 Stadia Maps \u00a9 Stamen Design", stadia_core_credit),
    paste("\u00a9 Stadia Maps", stadia_core_credit)
  )
  stadia <- new_basemap(
    id = paste0("stadia_", stadia_variants),
    label = c(
      "Stadia Alidade Smooth", "Stadia Alidade Smooth Dark",
      "Stadia Outdoors", "Stadia OSM Bright", "Stadia Stamen Toner",
      "Stadia Stamen Terrain", "Stadia Stamen Watercolor"
    ),
    group = "Stadia (API key)",
    type = "xyz",
    source = sprintf(
      "https://tiles.stadiamaps.com/tiles/%s/{z}/{x}/{y}.%s",
      stadia_variants, stadia_ext
    ),
    attribution = stadia_attribution,
    key = "stadia",
    key_param = "api_key",
    max_zoom = c(20L, 20L, 20L, 20L, 20L, 20L, 16L)
  )

  rbind(standard, thunderforest, stadia)
}

#' Is a basemap key requirement currently satisfied?
#'
#' Standard basemaps are always available. Thunderforest requires its
#' API key. Stadia is available with its key, or keyless when running
#' locally (see [is_deployed()]).
#'
#' @param key Character scalar. `"none"`, `"thunderforest"`, or
#'   `"stadia"`.
#'
#' @return Logical scalar.
basemap_key_available <- function(key) {
  switch(
    key,
    none = TRUE,
    thunderforest = nzchar(thunderforest_api_key()),
    stadia = nzchar(stadia_api_key()) || !is_deployed(),
    FALSE
  )
}

#' Registry rows for basemaps that are currently usable
#'
#' @return A data frame: the subset of [basemap_registry()] whose key
#'   requirement is satisfied (see [basemap_key_available()]).
available_basemaps <- function() {
  reg <- basemap_registry()
  ok <- vapply(reg$key, basemap_key_available, logical(1L))
  reg[ok, , drop = FALSE]
}

#' Look up the key requirement of a basemap id
#'
#' @param basemap_id Character scalar. A registry id.
#'
#' @return Character scalar: `"none"`, `"thunderforest"`, `"stadia"`,
#'   or `"none"` for unknown ids.
basemap_key_type <- function(basemap_id) {
  reg <- basemap_registry()
  key <- reg$key[reg$id == basemap_id]
  if (length(key) == 1L) key else "none"
}

#' Grouped selectInput choices for the available basemaps
#'
#' @return A named list of named character vectors, rendered by
#'   `shiny::selectInput()` as option groups. Unavailable groups are
#'   omitted entirely.
basemap_choices <- function() {
  reg <- available_basemaps()
  groups <- split(reg, factor(reg$group, levels = unique(reg$group)))
  lapply(groups, function(g) stats::setNames(g$id, g$label))
}

#' User-facing notes about basemap groups hidden for missing keys
#'
#' @return Character vector of messages, zero-length when every group
#'   is available.
hidden_basemap_notes <- function() {
  notes <- character(0L)
  if (!basemap_key_available("thunderforest")) {
    notes <- c(notes, paste(
      "Thunderforest basemaps are hidden: set the",
      "THUNDERFOREST_API_KEY environment variable (on Posit Connect:",
      "content Settings > Vars)."
    ))
  }
  if (!basemap_key_available("stadia")) {
    notes <- c(notes, paste(
      "Stadia basemaps are hidden: set the STADIA_API_KEY environment",
      "variable, or register this domain in the Stadia dashboard",
      "(keyless access only works from localhost)."
    ))
  }
  notes
}

#' Build the tile URL for an XYZ registry entry
#'
#' Appends the relevant API key as a query parameter when one is
#' configured; otherwise returns the bare template (Stadia's keyless
#' localhost mode).
#'
#' @param entry A single-row data frame from [basemap_registry()].
#'
#' @return Character scalar URL template.
basemap_tile_url <- function(entry) {
  key_value <- switch(
    entry$key,
    thunderforest = thunderforest_api_key(),
    stadia = stadia_api_key(),
    ""
  )
  if (!nzchar(entry$key_param) || !nzchar(key_value)) {
    return(entry$source)
  }
  sprintf(
    "%s?%s=%s",
    entry$source,
    entry$key_param,
    utils::URLencode(key_value, reserved = TRUE)
  )
}

#' Add basemap tiles to a leaflet map, never erroring
#'
#' Central error-safe entry point used by every map builder. Unknown
#' ids, basemaps whose API key is missing, and provider identifiers
#' rejected by the installed leaflet/leaflet.providers version all
#' fall back to plain OpenStreetMap tiles with a warning instead of
#' raising an error, so a stale bookmark, a removed key, or a version
#' mismatch can never take the app down.
#'
#' @param map A `leaflet` htmlwidget.
#' @param basemap_id Character scalar. A registry id from
#'   [basemap_registry()].
#'
#' @return The map with a tile layer added.
add_basemap_tiles <- function(map, basemap_id) {
  fallback <- function(reason) {
    warning(
      sprintf(
        "Basemap '%s' unavailable (%s); falling back to OpenStreetMap.",
        basemap_id, reason
      ),
      call. = FALSE
    )
    leaflet::addTiles(map)
  }
  reg <- basemap_registry()
  entry <- reg[reg$id == basemap_id, , drop = FALSE]
  if (nrow(entry) != 1L) {
    return(fallback("unknown basemap id"))
  }
  if (!basemap_key_available(entry$key)) {
    return(fallback("required API key is not set"))
  }
  if (identical(entry$type, "provider")) {
    tiled <- tryCatch(
      leaflet::addProviderTiles(map, entry$source),
      error = function(e) NULL
    )
    if (is.null(tiled)) {
      return(fallback("provider rejected by this leaflet install"))
    }
    return(tiled)
  }
  opts <- leaflet::tileOptions(maxZoom = entry$max_zoom)
  if (nzchar(entry$subdomains)) {
    opts$subdomains <- entry$subdomains
  }
  leaflet::addTiles(
    map,
    urlTemplate = basemap_tile_url(entry),
    attribution = entry$attribution,
    options = opts
  )
}
