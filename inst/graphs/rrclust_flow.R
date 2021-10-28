# 'Package Laden'
library(rrclust)

# Pfade setzen
dir_path_ahv <- "O:/MASS/09_mathprod/01_fh/container_tools/containers_dummy/research"
inp_dir <- file.path(dir_path_ahv, "rrclust")
# Choose a container in the list --
path_file <- function(x) file.path(inp_dir, x)

paths <- file.path(inp_dir, c(
  "params_kamila_large" 
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



# Output directory
path_out_init <- "O:/MASS/09_mathprod/01_fh/output/research"
# path_out_init <- "C:/research/outputs"
path_out <- file.path(path_out_init, "rrclust")
output_name <- "cl_kamila_20211014085917_u80844426_kamila_large"
path_output <- file.path(
  path_out,
  output_name
)

# Input directory
filenames <- list.files(path_output, full.names = TRUE, pattern = "param")
params <- list.files(filenames, full.names = TRUE, pattern = "PARAM_GLOBAL.csv$")
PARAM_GLOBAL_RRCLUST <- rrclust::tidylist_read(params)$PARAM_GLOBAL %>% 
  spread(key = key, value = value)
path_input <- c(PARAM_GLOBAL_RRCLUST$path_data,
                file.path(PARAM_GLOBAL_RRCLUST$path_data, "all"),
                file.path(PARAM_GLOBAL_RRCLUST$path_data, "kamila"))

# Retrieve outputs and inputs
all_csv <- rrclust::tidylist_read(path_output)
# all_csv$PLOTDATKAM
numb_clust <- max(as.double(all_csv$PLOTDATKAM$cluster_id))
# Graphs directory
path_allgraph <- "O:/MASS/02_team/03_math/anderes/doctorat_plc/travail/w02_rrclust/figures"
path_graphs <- file.path(path_allgraph, paste(gsub("-", "_", Sys.Date()),
                                              numb_clust,
                                              "clusters",
                                              sep = "_"
))
wd <- path_graphs

plot <- draw_flow(TF)


tmp <- DiagrammeRsvg::export_svg(plot)
tmp <- charToRaw(tmp) # flatten
rsvg::rsvg_png(tmp, file.path(wd, name_plot)) # saved graph as png in current working directory
