### Run a list of containers

## Load package ----------------------------------------------------------------

# Load from repository on server:
libloc <- "O:/MASS/04_software/01_r/lib_research/lib_rrclust"
.libPaths(c(.libPaths(), libloc))
library(rrclust, lib.loc = libloc)

# For local test or development purposes load sources virtually:
# devtools::load_all(".")
# verhindert, dass Zeilenumbruch browser() verlaesst
options(browserNLdisabled = TRUE)

# libloc <- "/Users/donzel/Développement/OFAS/package_tools/softwares/lib_rrclust"

# Rrclust --------------------------------------------------------------------------
dir_path_ahv <- "O:/MASS/09_mathprod/01_fh/container_tools/containers_dummy/research"
inp_dir <- file.path(dir_path_ahv, "rrclust")

path <- file.path(
  inp_dir,
  # "params_kamila" # kamila clustering method
  "params_kamila_large" # kamila clustering method with more variables
)


# Container to be archived
# path_out_init <- "O:/MASS/09_mathprod/01_fh/output/research"
path_out_init <- "C:/research/outputs"

path_out <-  file.path(path_out_init, "rrclust")
if (!file.exists(path_out)) {
  dir.create(path_out, recursive = TRUE)
}

# path_out <- "/Users/DonzeL/Développement/OFAS/_temp"
rrclust::run_kamila(path = path, path_out = path_out)


browseURL(path_out)
