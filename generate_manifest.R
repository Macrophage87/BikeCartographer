# Regenerate a COMPLETE, deployable manifest.json.
#
# Posit Connect Cloud installs exactly the packages listed in
# manifest.json and does NOT resolve transitive dependencies itself, so
# the manifest must contain the full recursive dependency closure with
# mutually compatible versions. The only reliable way to produce that
# is to install the app's direct dependencies into a real library and
# let rsconnect walk the tree.
#
# Run this ONCE in an environment that can install from CRAN/PPM (for
# example a Posit Cloud RStudio session with this project open), then
# commit the regenerated manifest.json and redeploy:
#
#   Rscript generate_manifest.R
#
# Direct dependencies of the app (rsconnect discovers the rest):
direct_deps <- c(
  "shiny", "leaflet", "leaflet.providers", "sf",
  "webshot2", "htmlwidgets", "htmltools", "jsonlite", "rsconnect"
)

# Use Posit Public Package Manager (PPM) so both this local install and
# the deploy target pull prebuilt Linux BINARIES instead of compiling
# from source. Building terra/sf from source against the runtime's
# system GDAL (e.g. 3.4.1 on Posit Cloud) fails with a C++/GDAL API
# mismatch; PPM binaries are built against that exact system and install
# cleanly. writeManifest() records this repo, so Connect Cloud restores
# the same binaries.
# Pin to a dated Posit Package Manager snapshot on the deploy target's
# Linux distro (Ubuntu 22.04 "jammy"), whose "__linux__" path serves
# prebuilt BINARIES (the plain ".../cran/latest" URL only serves Linux
# binaries to browsers, so Connect Cloud compiled from source instead).
# The date is set BEFORE terra 1.9-34 (2026-06-19): its multidimensional
# code calls a 3-argument GDAL AsClassicDataset() that exists only in
# GDAL >= 3.8, but jammy ships GDAL 3.4.1, so 1.9-34 fails to compile and
# has no jammy binary. This snapshot resolves terra to the prior,
# GDAL-3.4-compatible 1.9-27 as a binary. Move back to ".../jammy/latest"
# once terra's fix (already in its dev branch) reaches CRAN.
options(repos = c(
  RSPM = "https://packagemanager.posit.co/cran/__linux__/jammy/2026-06-01"
))

to_install <- setdiff(direct_deps, rownames(installed.packages()))
if (length(to_install) > 0L) {
  install.packages(to_install)
}

rsconnect::writeManifest(appDir = ".")
message("Wrote manifest.json with the full dependency closure.")
