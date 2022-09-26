### Run a list of containers

## Load package ----------------------------------------------------------------

# Load from repository on server:
# libloc <- "O:/MASS/04_software/01_r/lib_research/lib_rrclust"
# .libPaths(c(.libPaths(), libloc))
# library(delfin, lib.loc = libloc)

# Local
library(rrclust)

# For local test or development purposes load sources virtually:
# devtools::load_all(".")
# verhindert, dass Zeilenumbruch browser() verlaesst
options(browserNLdisabled = TRUE)

# libloc <- "/Users/donzel/Développement/OFAS/package_tools/softwares/lib_rrclust"


# root <- "O:/MASS/09_mathprod/01_fh/container_tools/inputs/research"
# root <- "C:/research/inputs/research"
root <- "/Users/Layal/OFAS/doctorat/package_tools/data"

# inp <- "inp_kamila"
inp <- "inp_kamila_large"

prepare_input(path = file.path(root,
                               inp,
                               "PARAM_INPUTS.csv"))
