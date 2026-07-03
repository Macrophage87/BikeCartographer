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
#basemap_preview {
  border: 1px solid #cfcfcf;
  border-radius: 6px;
}
.muted-note {
  color: #555555;
  font-size: 0.85em;
  margin-top: 4px;
}
"

ui <- fluidPage(
  title = "GPX Social Mapper",
  tags$head(tags$style(HTML(app_css))),
  titlePanel("GPX Social Mapper"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
      leaflet::leafletOutput("basemap_preview", height = 130),
      p(class = "muted-note", "Basemap preview (contiguous US)"),
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
      leaflet::leafletOutput("map", height = 620),
      uiOutput("layer_summary")
    )
  )
)

server <- function(input, output, session) {
  for (note in hidden_basemap_notes()) {
    showNotification(note, type = "message", duration = 12)
  }

  gpx_data <- reactive({
    req(input$gpx_file)
    result <- tryCatch(
      read_gpx_layers(input$gpx_file$datapath),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      showNotification(
        paste("Could not read GPX file:", conditionMessage(result)),
        type = "error"
      )
      return(NULL)
    }
    if (all(vapply(result, is.null, logical(1L)))) {
      showNotification(
        "No tracks, routes, or waypoints found in this GPX file.",
        type = "warning"
      )
    }
    result
  })

  selected_preset <- reactive({
    presets <- social_media_presets()
    presets[presets$id == input$preset, , drop = FALSE]
  })

  current_map <- reactive({
    gpx <- if (is.null(input$gpx_file)) NULL else gpx_data()
    build_gpx_map(
      gpx = gpx,
      basemap_id = input$basemap,
      track_color = input$track_color,
      track_weight = input$track_weight,
      waypoint_icon = input$waypoint_icon
    )
  })

  output$map <- leaflet::renderLeaflet(current_map())

  output$basemap_preview <- leaflet::renderLeaflet(
    build_basemap_preview(input$basemap)
  )

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
    req(input$gpx_file)
    gpx <- gpx_data()
    req(!is.null(gpx))
    counts <- count_gpx_features(gpx)
    p(
      class = "muted-note",
      sprintf(
        "Loaded: %d track(s), %d route(s), %d waypoint(s) (%d named).",
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
      if (identical(basemap_key_type(input$basemap), "stadia") &&
            !nzchar(stadia_api_key())) {
        showNotification(
          paste(
            "PNG export fetches Stadia tiles without a browser",
            "referer, so the basemap may render blank unless",
            "STADIA_API_KEY is set."
          ),
          type = "warning",
          duration = 10
        )
      }
      tmp_png <- tempfile(fileext = ".png")
      on.exit(unlink(tmp_png), add = TRUE)
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
      file.copy(tmp_png, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
