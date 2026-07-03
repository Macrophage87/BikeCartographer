# GPX Social Mapper

A Shiny app that imports a GPX file, draws its tracks, routes, and
waypoints on an interactive leaflet map, and exports the map as a PNG
sized for common social media formats using `mapview::mapshot2()`.

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
install.packages(c("shiny", "leaflet", "sf", "mapview", "webshot2"))
```

PNG export renders the map in headless Chrome via `{chromote}` (pulled
in by `{webshot2}`). A local Chrome/Chromium install is required; on a
headless server, install `chromium` and, if needed, point to it with
`Sys.setenv(CHROMOTE_CHROME = "/usr/bin/chromium")`. On mapview
installations older than 2.11 (no `mapshot2()`), the export helper
falls back to the legacy `mapshot()`, which needs PhantomJS
(`webshot::install_phantomjs()`); upgrading mapview is recommended.

## Run

```r
shiny::runApp()
```

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
R/basemap_utils.R     Basemap registry, API keys, error-safe tiles
R/map_utils.R         Export presets, icons, map builders
R/export_utils.R      mapshot2/mapshot PNG export wrapper
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

## manifest.json

The included `manifest.json` is a valid skeleton: the `files` section
carries real MD5 checksums for this snapshot of the code, and the
`packages` section lists the direct dependencies. Before deploying to
Posit Connect, regenerate it against your own library so versions,
transitive dependencies, and checksums match your environment:

```sh
Rscript generate_manifest.R
```

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
