# 'Package Laden'
library(delfin)

# Pfade setzen
inp_dir <- "O:/MASS/09_mathprod/01_fh/container_tools/containers_dummy/ahv/sgk_n"
# Choose a container in the list --
path_file <- function(x) file.path(inp_dir, x)

paths <- file.path(inp_dir, c(
  "container_abr20prov_rr20_ik18_statpop19_szen20_estv221_va22001" # Droit en vigueur
))


path_out <- file.path("C:/delfin/fhh/sgk_n")

# Daten vorbereiten (wenn nötig)
# prepare_input(path = path)

# Finanzhaushalt berechnen und ablegen
# run_container_ahv(path = path, datapath = datapath, outpath = outpath)

#------ Trace Diagramm ------------------------------------------------#
TF <- trace_flow({
  run_ahv(path = paths, path_out = path_out)
}) %>%
  filter(!grepl("^PARAM", df))

library(DiagrammeR)
name_plot <- "delfin_flow.png"
wd <- "O:/MASS/06_auftraege/01_bsv/11_delfins/02_delfin/04_documentation/doc_structure"

plot <- draw_flow(TF)


tmp <- DiagrammeRsvg::export_svg(plot)
tmp <- charToRaw(tmp) # flatten
rsvg::rsvg_png(tmp, file.path(wd, name_plot)) # saved graph as png in current working directory
