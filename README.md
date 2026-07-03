# GPX Social Mapper

A Shiny app that imports a GPX file, draws its tracks, routes, and
waypoints on an interactive leaflet map, and exports the map as a PNG
sized for common social media formats using `webshot2::webshot()`.

Features:

- GPX import via `{sf}` (tracks, routes, and waypoints layers, read
  defensively so partial files still load).
- Keyless basemaps with nationwide US coverage (OpenStreetMap, CARTO,
  Esri, USGS national services, OpenTopoMap), plus Thunderforest and
  Stadia layers unlocked by API-key environment variables, with a live
  mini preview of the selected style in the sidebar.
- Named waypoints rendered as Font Awesome icon markers (user-selectable
  icon) with name labels and popups; unnamed waypoints as small circles.
- One-click PNG export at exact platform dimensions (Instagram square /
  portrait / landscape, Stories/Reels/TikTok 9:16, Facebook post and
  cover, X post and header, LinkedIn, YouTube thumbnail, Pinterest pin),
  with an optional 2x retina render.

## Requirements

```r
install.packages(c(
  "shiny", "leaflet", "leaflet.providers", "sf",
  "webshot2", "htmlwidgets", "jsonlite"
))
```

PNG export writes the map to a self-contained HTML file with
`{htmlwidgets}` and captures it in headless Chrome via `{webshot2}`
(which pulls in `{chromote}`). A local Chrome/Chromium install is
required; on a headless server, install `chromium` and, if needed,
point to it with `Sys.setenv(CHROMOTE_CHROME = "/usr/bin/chromium")`.
`{sf}` needs the system GDAL/GEOS/PROJ libraries; these are already
present in the Posit Connect Cloud build environment.

## Run

```r
shiny::runApp()
```

- Import from a GPX upload **or directly from your Ride with GPS
  library** (routes and recorded rides), with route POIs arriving as
  named waypoints.
- Optional elevation profile panel (distance, total climb, and
  elevation range) drawn on the map itself, so it appears in both the
  interactive view and the exported PNG.

## Elevation profile

When **Elevation profile (map & export)** is ticked and the loaded data
carries elevations, a compact panel is attached to the bottom-left of
the map showing the profile in the track colour plus a summary line
(miles, total climb, and elevation range in feet). It is implemented as
a hand-built inline SVG added with `leaflet::addControl()` -- no
plotting packages, so the dependency manifest is unchanged -- and
because it is part of the map widget it is captured in PNG exports
(the export CSS hides only the zoom/layers controls).

Elevation sources: for GPX uploads, the `ele` field of the
`track_points` (or `route_points`) layer, with distance accumulated by
haversine; for Ride with GPS imports, the API's per-point `e`
elevation and cumulative `d` distance (haversine fallback when `d` is
absent). Elevations are lightly smoothed before the climb total is
computed so GPS noise does not inflate it. Files without elevation
data simply omit the panel.

## Ride with GPS import

An **Import from Ride with GPS** button in the sidebar opens a modal
dialog. With `RWGPS_API_KEY` and `RWGPS_AUTH_TOKEN` set, the modal
lists the 100 most recently updated routes and rides in your library
(fetched automatically on first open, with a refresh action); pick one
and import it to the map. Without credentials, the same modal shows
the setup steps below instead, so the feature stays discoverable. Planned routes draw dashed; recorded rides draw solid; route
POIs become named-waypoint icon markers. (Nice side effect: the
website's own GPX exporter omits POIs unless a checkbox is ticked,
while the API detail used here always includes them.)

Getting credentials — your password never touches this app:

1. In your Ride with GPS account settings, open the **Developers**
   tab and create an API client; copy its **API key**.
2. On that API client's management page, click **Create new Auth
   Token** and copy the token.
3. Set `RWGPS_API_KEY` and `RWGPS_AUTH_TOKEN` (Posit Connect: content
   Settings > Vars; locally: `~/.Renviron`).

Implementation notes: uses the documented v1 JSON API
(`/api/v1/routes.json`, `/api/v1/trips.json`, and the per-item detail
endpoints) with HTTP Basic authentication (API key as username, auth
token as password). Track points are converted directly to `sf`
geometry — no GPX intermediate — and flow through the exact same layer
structure as an uploaded file. Only items owned by the authenticated
account are listed. Very long rides are thinned to at most ~20,000
points for browser and export performance. Error-safe throughout: any
listing or import failure notifies and leaves the app (and the modal)
running.

## Thunderforest and Stadia basemaps (API keys)

Both families are defined as explicit XYZ tile templates in
`R/basemap_utils.R`, so they do not depend on the bundled
leaflet-providers version. Keys are read from environment variables:

| Variable                | Unlocks                                    |
| ----------------------- | ------------------------------------------ |
| `THUNDERFOREST_API_KEY` | OpenCycleMap, Outdoors, Landscape,         |
|                         | Transport, Atlas                           |
| `STADIA_API_KEY`        | Alidade Smooth (+Dark), Outdoors, OSM      |
|                         | Bright, Stamen Toner / Terrain / Watercolor|

Where to set them:

- **Posit Connect:** open the deployed content, then Settings > Vars,
  add the variable(s), and restart the content. (On Posit Connect
  Cloud, add them as environment variables when publishing.)
- **Locally:** add lines like `THUNDERFOREST_API_KEY=xxxx` to
  `~/.Renviron` and restart R.

Error-safe behavior when a map is unavailable:

- Basemap groups whose key is missing are removed from the dropdown
  entirely, and a notification at startup says which variable enables
  them. Stadia additionally stays enabled for keyless local runs,
  since Stadia allows unauthenticated requests from `localhost`
  (deployment is detected via `RSTUDIO_PRODUCT` / `SHINY_PORT`).
- Every tile request goes through `add_basemap_tiles()`, which falls
  back to plain OpenStreetMap with a warning — never an error — for
  unknown ids, missing keys, or provider names rejected by the
  installed leaflet version. A stale bookmark or a revoked key cannot
  crash the app.
- PNG export fetches tiles from headless Chrome, which sends no
  browser referer; keyless-localhost Stadia therefore may export a
  blank basemap, and the app warns accordingly. Set `STADIA_API_KEY`
  for Stadia exports even locally.

## Project layout

```
app.R                 UI + server (thin; logic lives in R/)
R/gpx_utils.R         GPX reading, waypoint splitting, bounds
R/elevation_utils.R   Elevation profile SVG panel
R/basemap_utils.R     Basemap registry, API keys, error-safe tiles
R/map_utils.R         Export presets, icons, map builders
R/rwgps_utils.R       Ride with GPS API client and converters
R/export_utils.R      webshot2 + htmlwidgets PNG export wrapper
manifest.json         Posit Connect deployment manifest
generate_manifest.R   Regenerates manifest.json locally
.lintr                lintr configuration
```

Shiny (>= 1.5) auto-sources everything in `R/` before evaluating
`app.R`, and every helper carries Roxygen2 documentation.

## Export sizing

The chosen preset sets the headless-browser viewport in CSS pixels, so
at 1x density the PNG matches the platform spec exactly (for example,
1080 x 1350 for an Instagram portrait post). The 2x option doubles the
device pixels for a retina-sharp image with identical framing. Tile
attribution is intentionally kept in exports, as most providers require
it.

## Deploying to Posit Connect Cloud (manifest.json)

Connect Cloud installs **exactly** the packages listed in
`manifest.json` and does **not** resolve transitive dependencies
itself. The manifest must therefore contain the *complete recursive
dependency closure* with mutually compatible versions — listing only
the direct dependencies causes installs to fail with errors like
`dependencies 'callr', 'chromote', ... are not available for package
'webshot2'`.

The `manifest.json` checked into this repo is a **stub** (correct
structure and file checksums, direct dependencies only). It will not
deploy as-is. Regenerate it against a real library before deploying:

1. Open this project in a Posit Cloud RStudio session (or any R
   environment that can install from CRAN/PPM).
2. From the project root, run:

   ```r
   source("generate_manifest.R")
   ```

   This installs the direct dependencies if needed and calls
   `rsconnect::writeManifest()`, which walks the full dependency tree
   and writes a complete, version-pinned `manifest.json`.
3. Commit and push the regenerated `manifest.json`, then redeploy.
   Connect Cloud pulls the manifest from the Git repo, so the
   regenerated file must be committed for the deploy to pick it up.

An `renv.lock` produced by `renv::snapshot()` is an equally valid
alternative to `manifest.json` for Connect Cloud, if you prefer an
renv-based workflow.

## Linting

```r
lintr::lint_dir()
```

The configuration in `.lintr` uses the lintr defaults (80-character
lines, snake_case object names) with `object_usage_linter` disabled:
that linter resolves symbols file-by-file against an installed package
namespace, so in a non-package Shiny app it false-positives on every
helper defined in a sibling `R/` file. All other default linters pass
with zero findings.
