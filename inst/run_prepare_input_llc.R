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

root <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/inputs"
inp <- "inp_kamila"

prepare_input(path = file.path(root,
                               inp,
                               "PARAM_INPUTS.csv"))
