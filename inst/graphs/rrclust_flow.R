# 'Package Laden'
library(rrclust)

# Pfade setzen
inp_dir <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/containers_dummy"
# Choose a container in the list --
path_file <- function(x) file.path(inp_dir, x)

paths <- file.path(inp_dir, c(
  "params_kamila" # Droit en vigueur
))


path_out <- tempdir()

# Daten vorbereiten (wenn nötig)
# prepare_input(path = path)

# Finanzhaushalt berechnen und ablegen
# run_container_ahv(path = path, datapath = datapath, outpath = outpath)

#------ Trace Diagramm ------------------------------------------------#
TF <- trace_flow({
  run_kamila(path = paths, path_out = path_out)
}) %>%
  filter(!grepl("^PARAM", df))

library(DiagrammeR)
name_plot <- "rrclust_flow.png"
wd <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/outputs/graphs/2021_06_10_5_clusters"

plot <- draw_flow(TF)


tmp <- DiagrammeRsvg::export_svg(plot)
tmp <- charToRaw(tmp) # flatten
rsvg::rsvg_png(tmp, file.path(wd, name_plot)) # saved graph as png in current working directory
