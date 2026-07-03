# GPX Social Mapper ---------------------------------------------------
# Import a GPX file, style it on an interactive leaflet map, and export
# the map as a PNG sized for common social media formats.
#
# Helper functions live in R/ and are auto-sourced by Shiny (>= 1.5)
# when the app directory is run. The fallback below covers direct
# `source("app.R")` invocations as well.

library(shiny)

if (!exists("basemap_choices", mode = "function")) {
  helper_files <- sort(
    list.files("R", pattern = "[.][Rr]$", full.names = TRUE)
  )
  for (helper_file in helper_files) {
    source(helper_file)
  }
}

# Dense track logs can exceed Shiny's 5 MB upload default.
options(shiny.maxRequestSize = 30 * 1024^2)

app_css <- "
html { scrollbar-gutter: stable; }
.muted-note {
  color: #555555;
  font-size: 0.85em;
  margin-top: 4px;
}
"

# Client-side scaler: the map is rendered at the exact export pixel size
# (see output$map_scaler_ui) so leaflet fits bounds to the true export
# dimensions. This script shrinks that fixed-size element to fit the
# panel with a CSS transform, which does not change the element's layout
# size (offsetWidth/Height), so leaflet's framing is untouched. The
# result is a pixel-faithful miniature of the exported PNG. The height is
# also capped at a fraction of the viewport so tall formats (e.g. 9:16)
# stay on screen; the map is centred horizontally when width-capped.
map_scaler_js <- "
(function() {
  function applyScale() {
    var frame = document.getElementById('map_frame');
    if (!frame) { return; }
    var scaler = frame.querySelector('#map_scaler');
    if (!scaler) { return; }
    var w = parseFloat(scaler.getAttribute('data-w'));
    var h = parseFloat(scaler.getAttribute('data-h'));
    var avail = frame.clientWidth;
    if (!(w > 0 && h > 0 && avail > 0)) { return; }
    var maxH = 0.82 * window.innerHeight;
    var s = Math.min(avail / w, maxH / h);
    var tx = Math.max(0, (avail - w * s) / 2);
    scaler.style.transform = 'translateX(' + tx + 'px) scale(' + s + ')';
    frame.style.height = (h * s) + 'px';
  }
  function init() {
    var frame = document.getElementById('map_frame');
    if (!frame) { setTimeout(init, 100); return; }
    if (frame.getAttribute('data-scaler-init')) { return; }
    frame.setAttribute('data-scaler-init', '1');
    if (window.ResizeObserver) {
      new ResizeObserver(applyScale).observe(frame);
    }
    new MutationObserver(applyScale)
      .observe(frame, { childList: true, subtree: true });
    window.addEventListener('resize', applyScale);
    applyScale();
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
"

ui <- fluidPage(
  title = "GPX Social Mapper",
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML(map_scaler_js))
  ),
  titlePanel("GPX Social Mapper"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      actionButton(
        "help",
        "How to use this app",
        width = "100%",
        class = "btn-info"
      ),
      hr(),
      fileInput(
        "gpx_file",
        "GPX file",
        accept = c(".gpx", "application/gpx+xml")
      ),
      selectInput(
        "basemap",
        "Basemap (nationwide US coverage)",
        choices = basemap_choices(),
        selected = "CartoDB.Positron"
      ),
      selectInput(
        "waypoint_icon",
        "Named waypoint icon",
        choices = c(
          "Map pin" = "map-pin",
          "Flag" = "flag",
          "Star" = "star",
          "Camera" = "camera",
          "Bicycle" = "bicycle"
        )
      ),
      selectInput(
        "track_color",
        "Track colour",
        choices = c(
          "Sunset orange" = "#E8552F",
          "Deep blue" = "#2C7FB8",
          "Forest green" = "#1B9E77",
          "Plum" = "#7B3294",
          "Charcoal" = "#252525"
        )
      ),
      sliderInput(
        "track_weight",
        "Track weight (px)",
        min = 1,
        max = 10,
        value = 4,
        step = 1
      ),
      checkboxInput(
        "show_elevation",
        "Elevation profile (map & export)",
        value = TRUE
      ),
      conditionalPanel(
        condition = "input.show_elevation",
        sliderInput(
          "elevation_size",
          "Elevation profile size",
          min = 1,
          max = 3,
          value = 1,
          step = 0.25
        )
      ),
      hr(),
      selectInput("preset", "Export size", choices = preset_choices()),
      selectInput(
        "density",
        "Pixel density",
        choices = c(
          "1x (exact platform size)" = "1",
          "2x (retina)" = "2"
        )
      ),
      uiOutput("dims_note"),
      downloadButton("download_png", "Export PNG", class = "btn-primary")
    ),
    mainPanel(
      width = 9,
      div(
        id = "map_frame",
        style = paste0(
          "position: relative; width: 100%; overflow: hidden; ",
          "box-sizing: border-box; border: 1px solid #cfcfcf; ",
          "border-radius: 6px;"
        ),
        uiOutput("map_scaler_ui")
      ),
      uiOutput("layer_summary")
    )
  )
)

# --- Server helpers ---------------------------------------------------
# Top-level handlers keep server() itself thin (and comfortably under
# the lintr cyclomatic-complexity threshold). shiny::showNotification,
# showModal, removeModal, and withProgress all pick up the session
# from the current reactive domain when called inside observers.

#' Store imported layers in the session state
#'
#' @param result A GPX layer list (see [read_gpx_layers()]).
#' @param label Character scalar. Source label for the summary line.
#' @param layers_rv,label_rv `reactiveVal`s holding the session state.
#'
#' @return Invisibly, `TRUE` when stored, `FALSE` when `result` holds
#'   no features.
set_imported_layers <- function(result, label, layers_rv, label_rv) {
  if (all(vapply(result, is.null, logical(1L)))) {
    showNotification(
      "No tracks, routes, or waypoints found in this file.",
      type = "warning"
    )
    return(invisible(FALSE))
  }
  layers_rv(result)
  label_rv(label)
  invisible(TRUE)
}

#' Handle a GPX file upload
#'
#' @param file_info One row of `input$gpx_file` from
#'   `shiny::fileInput()`.
#' @param layers_rv,label_rv `reactiveVal`s holding the session state.
#'
#' @return Invisibly, `TRUE` on success, `FALSE` otherwise.
handle_gpx_upload <- function(file_info, layers_rv, label_rv) {
  result <- tryCatch(
    read_gpx_layers(file_info$datapath),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    showNotification(
      paste("Could not read GPX file:", conditionMessage(result)),
      type = "error"
    )
    return(invisible(FALSE))
  }
  set_imported_layers(result, file_info$name, layers_rv, label_rv)
}

#' Warn when exporting a Stadia basemap without an API key
#'
#' Headless Chrome sends no browser referer, so Stadia's keyless
#' localhost mode does not apply to PNG exports.
#'
#' @param basemap_id Character scalar. The selected basemap id.
#'
#' @return Invisibly, `NULL`.
warn_if_stadia_keyless <- function(basemap_id) {
  if (identical(basemap_key_type(basemap_id), "stadia") &&
        !nzchar(stadia_api_key())) {
    showNotification(
      paste(
        "PNG export fetches Stadia tiles without a browser referer,",
        "so the basemap may render blank unless STADIA_API_KEY is",
        "set."
      ),
      type = "warning",
      duration = 10
    )
  }
  invisible(NULL)
}

#' Build the "how to use" guide modal
#'
#' Returns the `modalDialog` shown when the user clicks the help button.
#' Kept as a top-level helper so `server()` stays thin.
#'
#' @return A `shiny::modalDialog`.
usage_guide_modal <- function() {
  step <- function(title, ...) tags$li(tags$strong(title), " ", ...)
  modalDialog(
    title = "How to use GPX Social Mapper",
    easyClose = TRUE,
    size = "l",
    footer = modalButton("Got it"),
    tags$p(
      "Turn a GPS route into a clean map image sized for social media, ",
      "in five steps:"
    ),
    tags$ol(
      step(
        "Upload a GPX file.",
        "Use the GPX file control to choose a .gpx export from Strava, ",
        "Ride with GPS, Garmin, Komoot, and the like. Tracks draw as ",
        "solid lines, routes as dashed lines, and named waypoints as ",
        "labelled markers."
      ),
      step(
        "Pick a basemap.",
        "Choose the map style. Standard styles need no setup; ",
        "Thunderforest and Stadia styles appear only when their API key ",
        "is configured on the server."
      ),
      step(
        "Style the route.",
        "Set the marker icon for named waypoints, the track colour, and ",
        "the line weight."
      ),
      step(
        "Add an elevation profile (optional).",
        "Leave the Elevation profile box ticked to overlay a distance and ",
        "climb chart read from the GPX elevations, and use the size ",
        "slider to scale it. Files without elevation data skip the panel."
      ),
      step(
        "Choose an export size and download.",
        "Pick a platform preset such as Instagram, Story, or X. The ",
        "preview reshapes to match exactly what the image will look like. ",
        "Choose 2x density for a sharper retina file, then click Export ",
        "PNG."
      )
    ),
    tags$hr(),
    tags$p(tags$strong("Good to know")),
    tags$ul(
      tags$li(
        "What you see is what you get: the on-screen preview matches the ",
        "framing and elevation panel of the exported PNG."
      ),
      tags$li(
        "Zoom and layer controls are hidden in the export; map ",
        "attribution is kept, as tile providers require."
      ),
      tags$li(
        "If a basemap needs an API key that is not set, the app falls ",
        "back to OpenStreetMap."
      )
    )
  )
}

server <- function(input, output, session) {
  for (note in hidden_basemap_notes()) {
    showNotification(note, type = "message", duration = 12)
  }

  gpx_layers <- reactiveVal(NULL)
  gpx_label <- reactiveVal(NULL)

  observeEvent(input$gpx_file, {
    handle_gpx_upload(input$gpx_file, gpx_layers, gpx_label)
  })

  observeEvent(input$help, {
    showModal(usage_guide_modal())
  })

  selected_preset <- reactive({
    presets <- social_media_presets()
    presets[presets$id == input$preset, , drop = FALSE]
  })

  current_map <- reactive({
    build_gpx_map(
      gpx = gpx_layers(),
      basemap_id = input$basemap,
      track_color = input$track_color,
      track_weight = input$track_weight,
      waypoint_icon = input$waypoint_icon,
      show_elevation = isTRUE(input$show_elevation),
      elevation_scale = input$elevation_size
    )
  })

  output$map <- leaflet::renderLeaflet(current_map())

  # Render the interactive map at the EXACT export pixel size; the
  # map_scaler_js in the UI head shrinks it to fit with a CSS transform.
  # Because a transform does not change layout size, leaflet fits bounds
  # to the true export dimensions, so the preview is a faithful miniature
  # of the PNG (elevation panel and all).
  output$map_scaler_ui <- renderUI({
    preset <- selected_preset()
    if (nrow(preset) == 1L) {
      w <- preset$width
      h <- preset$height
    } else {
      w <- 1600L
      h <- 900L
    }
    div(
      id = "map_scaler",
      `data-w` = w,
      `data-h` = h,
      style = sprintf(
        "width: %dpx; height: %dpx; transform-origin: top left;",
        w, h
      ),
      leaflet::leafletOutput("map", width = "100%", height = "100%")
    )
  })

  output$dims_note <- renderUI({
    preset <- selected_preset()
    req(nrow(preset) == 1L)
    density <- as.integer(input$density)
    p(
      class = "muted-note",
      sprintf(
        "Output: %d \u00d7 %d px (%s aspect)",
        preset$width * density,
        preset$height * density,
        preset$ratio
      )
    )
  })

  output$layer_summary <- renderUI({
    gpx <- gpx_layers()
    req(!is.null(gpx))
    counts <- count_gpx_features(gpx)
    label <- gpx_label()
    if (is.null(label)) {
      label <- "Loaded"
    }
    p(
      class = "muted-note",
      sprintf(
        "%s: %d track(s), %d route(s), %d waypoint(s) (%d named).",
        label,
        counts[["tracks"]],
        counts[["routes"]],
        counts[["waypoints"]],
        counts[["named_waypoints"]]
      )
    )
  })

  output$download_png <- downloadHandler(
    filename = function() {
      sprintf(
        "gpx-map_%s_%s.png",
        input$preset,
        format(Sys.time(), "%Y%m%d-%H%M%S")
      )
    },
    content = function(file) {
      preset <- selected_preset()
      req(nrow(preset) == 1L)
      warn_if_stadia_keyless(input$basemap)
      tmp_png <- tempfile(fileext = ".png")
      on.exit(unlink(tmp_png), add = TRUE)
      ok <- tryCatch(
        {
          withProgress(
            message = "Rendering PNG in headless Chrome...",
            value = 0.4,
            {
              export_map_png(
                map = current_map(),
                file = tmp_png,
                width = preset$width,
                height = preset$height,
                zoom = as.integer(input$density)
              )
            }
          )
          TRUE
        },
        error = function(e) {
          showNotification(
            paste("PNG export failed:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
          FALSE
        }
      )
      if (isTRUE(ok) && file.exists(tmp_png)) {
        file.copy(tmp_png, file, overwrite = TRUE)
      }
    }
  )
}

shinyApp(ui, server)
