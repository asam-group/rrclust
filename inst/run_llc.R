## Load package ----------------------------------------------------------------
# Load from repository on server:
libloc <- "/Users/Layal/OFAS/doctorat/package_tools/softwares/lib_rrclust"
# libloc <- "/Users/donzel/Développement/OFAS/package_tools/softwares/lib_rrclust"

.libPaths(c(.libPaths(), libloc))
library(rrclust, lib.loc = libloc)

# Load from the local repository
library(rrclust)

# For local test or development purposes load sources virtually:
# devtools::load_all(".")
# impedes that line break makes the browser() leave.
options(browserNLdisabled = TRUE)

path_init <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/containers_dummy"
# path_init <- "/Users/Donzel/Développement/OFAS/package_tools/container_tools/containers_dummy"

path <- file.path(
  path_init,
  "params_kmeans" # kmeans clustering method
)


# Container to be archived
path_out <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/outputs"
# path_out <- "/Users/DonzeL/Développement/OFAS/_temp"
rrclust::run_kmeans(path = path, path_out = path_out)
# rrclust::run_ward(path = path, path_out = path_out)

browseURL(path_out)
