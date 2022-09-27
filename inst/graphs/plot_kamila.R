# Libraries
library(ggplot2)
library(rrclust)
library(xtable)
library(purrr)
library(delfin)
library(vtable)
library(plotly)
library(RColorBrewer)
library(htmlwidgets)
library(lattice)
library(qqplotr)
library(moments)

# Output directory
# path_out_init <- "O:/MASS/09_mathprod/01_fh/output/research"
# path_out_init <- "C:/research/outputs"
path_out_init <- "/Users/Layal/OFAS/doctorat/package_tools/output"

path_out <- file.path(path_out_init, "rrclust")

# output_name <- "cl_kamila_20211104141309_u80844426_kamila_large"
output_name <- "cl_kamila_20220926170231__kamila_large"

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
all_csv_inputs <- rrclust::tidylist_read(path_input)

# all_csv$PLOTDATKAM
numb_clust <- max(as.double(all_csv$PLOTDATKAM$cluster_id))

N_IND <- distinct(all_csv$PLOTDATKAM %>%
  group_by(cluster_id) %>%
  mutate(n_ind = n()) %>%
  dplyr::select(cluster_id, n_ind)) %>%
  arrange(cluster_id)

# Rente minimale
rmin <- min((all_csv$PLOTDATKAM %>%
  filter(
    scale == 44,
    age_retire == age,
    age == 64 + 1 * (sex == 0)
  ) %>%
  dplyr::select(monthly_pension))$monthly_pension)

# Rente maximale
rmax <- 2 * rmin

# Plafond max pour couples mariés
plaf <- 1.5 * rmax

# Graphs directory
# path_allgraph <- "O:/MASS/02_team/03_math/anderes/doctorat_plc/travail/w02_rrclust/figures"
path_allgraph <- "/Users/Layal/OFAS/doctorat/travail/w02_rrclust/figures"

path_graphs <- file.path(path_allgraph, paste(gsub("-", "_", Sys.Date()),
  numb_clust,
  "clusters",
  sep = "_"
))
if (file.exists(path_graphs)) stop(path_graphs, " already exists")
if (!file.exists(path_graphs)) {
  fs::dir_create(path_graphs, recurse = TRUE)
}
# Descriptive Stats directory
descrstat_dpath <- file.path(path_graphs, "descrstat")
if (file.exists(descrstat_dpath)) stop(descrstat_dpath, " already exists")
if (!file.exists(descrstat_dpath)) {
  fs::dir_create(descrstat_dpath, recurse = TRUE)
}
# Output copy directory
output_dir <- file.path(path_graphs, "r_output")
if (file.exists(output_dir)) stop(output_dir, " already exists")
if (!file.exists(output_dir)) {
  fs::dir_create(output_dir, recurse = TRUE)
  file.copy(path_output, output_dir, recursive = TRUE)
}

#--- Skewness and kurtosis -----------------------------------------------------
SKEW_KURT <- all_csv$PLOTDATKAM %>%
  group_by(cluster_id) %>%
  summarise(
    skew_mr = skewness(monthly_pension),
    kurtosis_mr = kurtosis(monthly_pension),
    skew_aadr = skewness(aadr),
    kurtosis_aadr = kurtosis(aadr)
  ) %>%
  gather(key = moment, value = value, -cluster_id) %>%
  mutate(
    moment_type = gsub("_.*(.*)", "", moment),
    var = gsub("^.+?_", "", moment),
    Moment = recode(moment_type,
      "skew" = "Skewness",
      "kurtosis" = "Kurtosis"
    ),
    var = recode(var,
      "mr" = "Monthly Pension Amount (CHF)",
      "aadr" = "AADR (CHF)"
    ),
    Cluster = cluster_id
  ) %>%
  dplyr::select(-moment, -moment_type, -cluster_id) %>%
  spread(key = var, value = value) %>%
  arrange(desc(Moment)) %>%
  mutate_if(sapply(., is.character), as.factor)



print(
  xtable(SKEW_KURT,
    align = "lcccc",
    label = "Table of Moments pro Cluster",
    caption = "Table of Moments pro Cluster"
  ),
  # tabular.environment = "longtable",
  caption.placement = "top",
  table.placement = "H",
  floating = TRUE,
  size = "\\fontsize{10pt}{11pt}\\selectfont",
  file = file.path(path_graphs, "moments_table.tex")
)
#--- Histograms pro Cluster ----------------------------------------------------
# AADR
ggplot(
  all_csv$PLOTDATKAM %>%
    group_by(cluster_id) %>%
    mutate(n_ind = n()),
  aes(log(aadr), fill = as.factor(cluster_id))
) +
  facet_wrap(~ as.factor(cluster_id),
    scales = "free_x",
    ncol = 2
  ) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1 / 3))) +
  theme_light() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(
      size = 11, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 11, color = "black", face = "bold"
    )
  ) +
  labs(
    title = "Histograms of the natural logarithm of the AADR",
    subtitle = paste0(
      "Number of obs. per cluster:",
      " C", N_IND$cluster_id[1], ": ", N_IND$n_ind[1],
      ", C", N_IND$cluster_id[2], ": ", N_IND$n_ind[2],
      ", C", N_IND$cluster_id[3], ": ", N_IND$n_ind[3],
      ", C", N_IND$cluster_id[4], ": ", N_IND$n_ind[4],
      ", C", N_IND$cluster_id[5], ": ", N_IND$n_ind[5]
    ),
    x = "Natural logarithm of AADR",
    y = "log10(Frequency)",
    caption = paste(
      Sys.Date(),
      "Llc",
      sep = ", "
    )
  ) +
  scale_y_log10() +
  scale_fill_manual("Cluster",
    breaks = c("1", "2", "3", "4", "5"),
    values = c("red", "blue", "green", "orange", "violet")
  )
ggsave(file.path(
  path_graphs,
  paste(
    numb_clust,
    "clusters_hist_log10aadr.png",
    sep = "_"
  )
),
height = 8.27,
width = 11.69
)

# AADR
ggplot(
  all_csv$PLOTDATKAM %>%
    group_by(cluster_id) %>%
    mutate(n_ind = n()),
  aes(log(aadr), fill = as.factor(cluster_id))
) +
  facet_wrap(~ as.factor(cluster_id),
    scales = "free_x",
    ncol = 2
  ) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1 / 3))) +
  theme_light() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(
      size = 11, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 11, color = "black", face = "bold"
    )
  ) +
  labs(
    title = "Histograms of the natural logarithm of the AADR",
    subtitle = paste0(
      "Number of obs. per cluster:",
      " C", N_IND$cluster_id[1], ": ", N_IND$n_ind[1],
      ", C", N_IND$cluster_id[2], ": ", N_IND$n_ind[2],
      ", C", N_IND$cluster_id[3], ": ", N_IND$n_ind[3],
      ", C", N_IND$cluster_id[4], ": ", N_IND$n_ind[4],
      ", C", N_IND$cluster_id[5], ": ", N_IND$n_ind[5]
    ),
    x = "Natural logarithm of AADR",
    y = "Frequency",
    caption = paste(
      Sys.Date(),
      "Llc",
      sep = ", "
    )
  ) +
  scale_fill_manual("Cluster",
    breaks = c("1", "2", "3", "4", "5"),
    values = c("red", "blue", "green", "orange", "violet")
  )
ggsave(file.path(
  path_graphs,
  paste(
    numb_clust,
    "clusters_hist_aadr.png",
    sep = "_"
  )
),
height = 8.27,
width = 11.69
)

# Density plot log(aadr)
ggplot(
  all_csv$PLOTDATKAM %>%
    group_by(cluster_id) %>%
    mutate(n_ind = n()),
  aes(log(aadr), fill = as.factor(cluster_id))
) +
  theme_light() +
  geom_density(alpha = 0.4) +
  theme_light() +
  facet_grid(~ as.factor(cluster_id)) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(
      size = 11, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 11, color = "black", face = "bold"
    )
  ) +
  labs(
    title = "Density of the natural logarithm of the AADR",
    subtitle = paste0(
      "Number of obs. per cluster:",
      " C", N_IND$cluster_id[1], ": ", N_IND$n_ind[1],
      ", C", N_IND$cluster_id[2], ": ", N_IND$n_ind[2],
      ", C", N_IND$cluster_id[3], ": ", N_IND$n_ind[3],
      ", C", N_IND$cluster_id[4], ": ", N_IND$n_ind[4],
      ", C", N_IND$cluster_id[5], ": ", N_IND$n_ind[5]
    ),
    x = "Natural logarithm of AADR",
    y = "Density",
    caption = paste(
      Sys.Date(),
      "Llc",
      sep = ", "
    )
  ) +
  # scale_x_continuous( # <- set the limits for your y-axis
  #   limits = c(0, plaf + 100)
  # ) +
  scale_fill_manual("Cluster Density",
    breaks = c("1", "2", "3", "4", "5"),
    values = c("red", "blue", "green", "orange", "violet")
  )
ggsave(file.path(
  path_graphs,
  paste(
    numb_clust,
    "clusters_aadr_dens.png",
    sep = "_"
  )
),
height = 8.27,
width = 11.69
)

# ECDF plot aadr
ggplot(
  all_csv$PLOTDATKAM %>%
    group_by(cluster_id) %>%
    mutate(
      n_ind = n(),
      grp.mean = mean(aadr)
    ),
  aes(log(aadr))
) +
  stat_ecdf(aes(
    group = as.factor(cluster_id),
    colour = as.factor(cluster_id)
  ),
  size = 1
  ) +
  # facet_wrap(~ as.factor(cluster_id)) +
  theme_light() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(
      size = 11, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 11, color = "black", face = "bold"
    )
  ) +
  labs(
    title = "ECDF of the AADR",
    subtitle = paste0(
      "Number of obs. per cluster:",
      " C", N_IND$cluster_id[1], ": ", N_IND$n_ind[1],
      ", C", N_IND$cluster_id[2], ": ", N_IND$n_ind[2],
      ", C", N_IND$cluster_id[3], ": ", N_IND$n_ind[3],
      ", C", N_IND$cluster_id[4], ": ", N_IND$n_ind[4],
      ", C", N_IND$cluster_id[5], ": ", N_IND$n_ind[5]
    ),
    x = "Natural logarithm of the AADR",
    y = "Empirical Cumulative Density Function",
    caption = paste(
      Sys.Date(),
      "Llc",
      sep = ", "
    )
  ) +
  scale_color_manual("Cluster",
    breaks = c("1", "2", "3", "4", "5"),
    values = c("red", "blue", "green", "orange", "violet")
  )
ggsave(file.path(
  path_graphs,
  paste(
    numb_clust,
    "clusters_aadr_ecdf.png",
    sep = "_"
  )
),
height = 8.27,
width = 11.69
)

# Normal Q-Q plot of monthly pension amount
df1 <- all_csv$PLOTDATKAM %>%
  group_by(cluster_id) %>%
  mutate(
    n_ind = n(),
    std_aadr = rangeStandardize(aadr)
  )

ggplot(
  data = df1,
  mapping = aes(
    sample = std_aadr,
    color = as.factor(cluster_id),
    fill = as.factor(cluster_id)
  )
) +
  stat_qq_band(alpha = 0.5) +
  stat_qq_point() +
  stat_qq_line() +
  facet_wrap(~ as.factor(cluster_id)) +
  theme_light() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(
      size = 11, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 11, color = "black", face = "bold"
    )
  ) +
  labs(
    title = "Normal Q-Q plot of the AADR (standardised for range)",
    subtitle = paste0(
      "Number of obs. per cluster:",
      " C", N_IND$cluster_id[1], ": ", N_IND$n_ind[1],
      ", C", N_IND$cluster_id[2], ": ", N_IND$n_ind[2],
      ", C", N_IND$cluster_id[3], ": ", N_IND$n_ind[3],
      ", C", N_IND$cluster_id[4], ": ", N_IND$n_ind[4],
      ", C", N_IND$cluster_id[5], ": ", N_IND$n_ind[5]
    ),
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
    caption = paste(
      Sys.Date(),
      "Llc",
      sep = ", "
    )
  ) +
  scale_color_manual("Cluster Sample Quantiles",
    breaks = c("1", "2", "3", "4", "5"),
    values = c("red", "blue", "green", "orange", "violet")
  ) +
  scale_fill_manual("Cluster Theoretical Quantiles",
    breaks = c("1", "2", "3", "4", "5"),
    values = c("red", "blue", "green", "orange", "violet")
  )
ggsave(file.path(
  path_graphs,
  paste(
    numb_clust,
    "clusters_aadr_qqplot.png",
    sep = "_"
  )
),
height = 8.27,
width = 11.69
)
#---  Monthly pension amount -------------------------------------------------------------
ggplot(
  all_csv$PLOTDATKAM %>%
    group_by(cluster_id) %>%
    mutate(n_ind = n()),
  aes(monthly_pension, fill = as.factor(cluster_id))
) +
  facet_wrap(~ as.factor(cluster_id),
    scales = "free_x",
    ncol = 2
  ) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1 / 3))) +
  theme_light() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(
      size = 11, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 11, color = "black", face = "bold"
    )
  ) +
  labs(
    title = paste0(
      "Histograms of the monthly pension amount (min. = ",
      rmin, " CHF,",
      " max. = ", rmax, " CHF,",
      " max. for couples = ", plaf, " CHF)"
    ),
    subtitle = paste0(
      "Number of obs. per cluster:",
      " C", N_IND$cluster_id[1], ": ", N_IND$n_ind[1],
      ", C", N_IND$cluster_id[2], ": ", N_IND$n_ind[2],
      ", C", N_IND$cluster_id[3], ": ", N_IND$n_ind[3],
      ", C", N_IND$cluster_id[4], ": ", N_IND$n_ind[4],
      ", C", N_IND$cluster_id[5], ": ", N_IND$n_ind[5]
    ),
    x = "Monthly pension amount (CHF)",
    y = "log10(Frequency)",
    caption = paste(
      Sys.Date(),
      "Llc",
      sep = ", "
    )
  ) +
  scale_x_continuous( # <- set the limits for your y-axis
    limits = c(0, plaf + 100)
  ) +
  scale_y_log10() +
  scale_fill_manual("Cluster",
    breaks = c("1", "2", "3", "4", "5"),
    values = c("red", "blue", "green", "orange", "violet")
  )
ggsave(file.path(
  path_graphs,
  paste(
    numb_clust,
    "clusters_hist_mr.png",
    sep = "_"
  )
),
height = 8.27,
width = 11.69
)

# Density plot mr
ggplot(
  all_csv$PLOTDATKAM %>%
    group_by(cluster_id) %>%
    mutate(
      n_ind = n(),
      grp.mean = mean(monthly_pension)
    ),
  aes(monthly_pension, fill = as.factor(cluster_id))
) +
  geom_density(alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = as.factor(cluster_id)),
    linetype = "dashed"
  ) +
  theme_light() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(
      size = 11, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 11, color = "black", face = "bold"
    )
  ) +
  labs(
    title = paste0(
      "Density of the monthly pension amount (min. = ",
      rmin, " CHF,",
      " max. = ", rmax, " CHF,",
      " max. for couples = ", plaf, " CHF)"
    ),
    subtitle = paste0(
      "Number of obs. per cluster:",
      " C", N_IND$cluster_id[1], ": ", N_IND$n_ind[1],
      ", C", N_IND$cluster_id[2], ": ", N_IND$n_ind[2],
      ", C", N_IND$cluster_id[3], ": ", N_IND$n_ind[3],
      ", C", N_IND$cluster_id[4], ": ", N_IND$n_ind[4],
      ", C", N_IND$cluster_id[5], ": ", N_IND$n_ind[5]
    ),
    x = "Monthly pension amount (CHF)",
    y = "Density",
    caption = paste(
      Sys.Date(),
      "Llc",
      sep = ", "
    )
  ) +
  scale_x_continuous( # <- set the limits for your y-axis
    limits = c(0, plaf + 100)
  ) +
  scale_color_manual("Cluster Mean",
    breaks = c("1", "2", "3", "4", "5"),
    values = c("red", "blue", "green", "orange", "violet")
  ) +
  scale_fill_manual("Cluster Density",
    breaks = c("1", "2", "3", "4", "5"),
    values = c("red", "blue", "green", "orange", "violet")
  )
ggsave(file.path(
  path_graphs,
  paste(
    numb_clust,
    "clusters_mr_dens.png",
    sep = "_"
  )
),
height = 8.27,
width = 11.69
)

# ECDF plot mr
ggplot(
  all_csv$PLOTDATKAM %>%
    group_by(cluster_id) %>%
    mutate(
      n_ind = n(),
      grp.mean = mean(monthly_pension)
    ),
  aes(monthly_pension)
) +
  stat_ecdf(aes(
    group = as.factor(cluster_id),
    colour = as.factor(cluster_id)
  ),
  size = 1
  ) +
  theme_light() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(
      size = 11, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 11, color = "black", face = "bold"
    )
  ) +
  labs(
    title = paste0(
      "ECDF of the monthly pension amount (min. = ",
      rmin, " CHF,",
      " max. = ", rmax, " CHF,",
      " max. for couples = ", plaf, " CHF)"
    ),
    subtitle = paste0(
      "Number of obs. per cluster:",
      " C", N_IND$cluster_id[1], ": ", N_IND$n_ind[1],
      ", C", N_IND$cluster_id[2], ": ", N_IND$n_ind[2],
      ", C", N_IND$cluster_id[3], ": ", N_IND$n_ind[3],
      ", C", N_IND$cluster_id[4], ": ", N_IND$n_ind[4],
      ", C", N_IND$cluster_id[5], ": ", N_IND$n_ind[5]
    ),
    x = "Monthly pension amount (CHF)",
    y = "Empirical Cumulative Density Function",
    caption = paste(
      Sys.Date(),
      "Llc",
      sep = ", "
    )
  ) +
  scale_x_continuous( # <- set the limits for your y-axis
    limits = c(0, plaf + 100)
  ) +
  scale_color_manual("Cluster",
    breaks = c("1", "2", "3", "4", "5"),
    values = c("red", "blue", "green", "orange", "violet")
  )
ggsave(file.path(
  path_graphs,
  paste(
    numb_clust,
    "clusters_mr_ecdf.png",
    sep = "_"
  )
),
height = 8.27,
width = 11.69
)

# Normal Q-Q plot of monthly pension amount
df <- all_csv$PLOTDATKAM %>%
  group_by(cluster_id) %>%
  mutate(n_ind = n())

ggplot(
  data = df,
  mapping = aes(
    sample = monthly_pension,
    color = as.factor(cluster_id),
    fill = as.factor(cluster_id)
  )
) +
  stat_qq_band(alpha = 0.5) +
  stat_qq_point() +
  stat_qq_line() +
  facet_wrap(~ as.factor(cluster_id)) +
  theme_light() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(
      size = 11, color = "black", face = "bold"
    ),
    strip.text.y = element_text(
      size = 11, color = "black", face = "bold"
    )
  ) +
  labs(
    title = paste0(
      "Normal Q-Q plot of the monthly pension amount (min. = ",
      rmin, " CHF,",
      " max. = ", rmax, " CHF,",
      " max. for couples = ", plaf, " CHF)"
    ),
    subtitle = paste0(
      "Number of obs. per cluster:",
      " C", N_IND$cluster_id[1], ": ", N_IND$n_ind[1],
      ", C", N_IND$cluster_id[2], ": ", N_IND$n_ind[2],
      ", C", N_IND$cluster_id[3], ": ", N_IND$n_ind[3],
      ", C", N_IND$cluster_id[4], ": ", N_IND$n_ind[4],
      ", C", N_IND$cluster_id[5], ": ", N_IND$n_ind[5]
    ),
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
    caption = paste(
      Sys.Date(),
      "Llc",
      sep = ", "
    )
  ) +
  scale_color_manual("Cluster Sample Quantiles",
    breaks = c("1", "2", "3", "4", "5"),
    values = c("red", "blue", "green", "orange", "violet")
  ) +
  scale_fill_manual("Cluster Theoretical Quantiles",
    breaks = c("1", "2", "3", "4", "5"),
    values = c("red", "blue", "green", "orange", "violet")
  )
ggsave(file.path(
  path_graphs,
  paste(
    numb_clust,
    "clusters_mr_qqplot.png",
    sep = "_"
  )
),
height = 8.27,
width = 11.69
)



#--- Scatterplot pro Cluster ---------------------------------------------------

pattern_names <- paste(colnames(all_csv$FULL_CONT_DF), collapse = "|")

plottingData <- all_csv$PLOTDATKAM %>%
  mutate_if(!grepl(pattern_names, colnames(.)), as.factor) %>%
  mutate(KamilaCluster = cluster_id)

plotOpts <- function(pl) {
  (pl + geom_point() +
    scale_shape_manual(
      values = c(
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
        11, 12, 13, 14, 15, 16
      )
    ) +
    geom_jitter())
}


# All combinations
CROSS_TIB_KAMRES <- tibble(unique_sex = unique(plottingData$sex)) %>%
  crossing(
    unique_benef_type1 = unique(plottingData$benef_type1)
  ) %>%
  rowwise() %>%
  mutate(
    dta = list(plottingData %>%
      filter(
        sex == unique_sex,
        benef_type1 == unique_benef_type1
      ))
  )


# Plots
PLOTS_TIB_KAMRES <- CROSS_TIB_KAMRES %>%
  mutate(data = list(dta)) %>%
  dplyr::select(-dta) %>%
  mutate(plots = map(list(data), rrclust::function_kamplot)) %>%
  dplyr::select(-data)

#--- Table for raw lbedu and lbass ---------------------------------------------
RR_NA_VAR <- IND_YEARLY_RR %>%
  dplyr::select(raw_contrib_m_ind = lcot,
                raw_contrib_y_ageclass = lcotg,
                raw_splitting = csplit,
                raw_bonus_m_edu = lbedu,
                raw_bonus_m_assist = lbass)


print(
  st(RR_NA_VAR,
     file = file.path(path_graphs, "RR_NA_VAR"),
     anchor = "sum_stats_rr_na_var",
     title = "Summary Statistics of the Pension Register's Variables Recoded for NAs",
     out = "latex"
  ),
  tabular.environment = "longtable",
  caption.placement = "top",
  table.placement = "",
  floating = FALSE,
  size = "\\fontsize{8pt}{9pt}\\selectfont",
  file = file.path(path_graphs, "RR_NA_VAR.tex")
)
#--- Table Summary Statistics --------------------------------------------------
RR_OASI <- mod_prepa_rr(IND_YEARLY_RR = all_csv_inputs$IND_YEARLY_RR)$RR_OASI %>%
  dplyr::select(-age_retire)

print(
  st(RR_OASI,
    file = file.path(path_graphs, "RR_DESCR"),
    anchor = "sum_stats_rr_oasi",
    title = "Summary Statistics of the Pension Register",
    out = "latex"
  ),
  tabular.environment = "longtable",
  caption.placement = "top",
  table.placement = "",
  floating = FALSE,
  size = "\\fontsize{8pt}{9pt}\\selectfont",
  file = file.path(path_graphs, "RR_DESCR.tex")
)

#--- Table Summary Statistics per Cluster --------------------------------------
cnames <- c(colnames(RR_OASI), "cluster_id")
cnames_pattern <- paste(cnames, collapse = "|")

dataclust_fun <- function(clust_number, ...) {
  DATA_CLUST <- all_csv$PLOTDATKAM %>%
    mutate(year = unique(RR_OASI$year)) %>%
    dplyr::select(grep(cnames_pattern, names(.))) %>%
    filter(cluster_id == clust_number)

  reorder_idx <- match(cnames, names(DATA_CLUST))[!is.na(match(cnames, names(DATA_CLUST)))] # Saving indices for how to reorder `second` to match `first`
  DATA_CLUST[reorder_idx] # Reordering the second vector to match the order of the first vector
  DATA_CLUST_REORDERED <- DATA_CLUST[reorder_idx] # Reordering and saving the output to a variable

  print(
    st(DATA_CLUST_REORDERED,
      file = file.path(path_graphs, paste0("DATA_CLUST_REORDERED_", clust_number)),
      anchor = paste0("sum_stats_rr_oasi_", clust_number),
      title = paste0("Summary Statistics of the Clustered Pension Register, Cluster ", clust_number),
      out = "latex"
    ),
    tabular.environment = "longtable",
    caption.placement = "top",
    table.placement = "",
    floating = FALSE,
    size = "\\fontsize{8pt}{9pt}\\selectfont",
    file = file.path(
      path_graphs,
      paste0("DATA_CLUST_REORDERED_", clust_number, ".tex")
    )
  )
}

DATA_CLUST_ALL <- tibble(clust_number = unique(all_csv$PLOTDATKAM$cluster_id)) %>%
  arrange(clust_number) %>%
  mutate(
    cnames = list(cnames),
    cnames_pattern = list(cnames_pattern)
  ) %>%
  rowwise() %>%
  mutate(data_clust_reord = list(lapply(clust_number, dataclust_fun))) %>%
  unnest(cols = data_clust_reord) %>%
  dplyr::select(
    -cnames,
    -cnames_pattern
  ) %>%
  unnest(cols = data_clust_reord)




# #--- Table cluster id, benef_type ---------------------------------------------
# tab_benef_clust <- table(all_csv$PLOTDATKAM$cluster_id, all_csv$PLOTDATKAM$benef_type)
# print(
#   xtable(format(tab_benef_clust),
#     label = "Type of Benefit and Clusters Contingency Table",
#     caption = "Type of Benefit and ClustersContingency Table"
#   ),
#   table.placement = "H",
#   caption.placement = "top",
#   include.rownames = TRUE,
#   include.colnames = TRUE,
#   size = "normalsize",
#   hline.after = c(-1, 0, nrow(tab_benef_clust)),
#   file = file.path(path_graphs, "tab_benef_clust.tex")
# )

#--- PSPLOT --------------------------------------------------------------------
psplot <- with(
  all_csv$KM_RES,
  qplot(sort(unique(cluster_id)), ps_values) +
    geom_errorbar(aes(
      x = sort(unique(cluster_id)),
      ymin = ps_values - std_err_pred_str,
      ymax = ps_values + std_err_pred_str
    ),
    width = 0.25
    )
) +
  theme_light() +
  labs(
    title = "Selecting the Optimal Number of Clusters with the Prediction Strength Criteria",
    subtitle = paste0("Optimal number of clusters: ", numb_clust, " clusters"),
    x = "Number of Clusters",
    y = "Prediction Strength Values",
    caption = paste(
      Sys.Date(),
      "Llc",
      sep = ", "
    )
  ) +
  geom_hline(yintercept = 0.8, lty = 2) +
  scale_x_continuous(breaks = sort(unique(all_csv$KM_RES$cluster_id))) +
  ylim(0, 1.1)
ggsave(file.path(path_graphs, "psplot.png"),
  height = 8.27,
  width = 11.69
)

#--- Ftable ----------------------------------------------------
DATA_FTABLE <- all_csv$PLOTDATKAM %>%
  mutate(
    ln_aadr = log(aadr),
    ln_monthly_pension = log(monthly_pension),
    benef_type = as.factor(benef_type),
    marital_stat = as.factor(marital_stat),
    nat = as.factor(nat),
    resid = as.factor(resid),
    sex = as.factor(sex),
    cluster_id = as.factor(cluster_id)
  ) %>%
  dplyr::select(
    c(
      cluster_id,
      ln_aadr,
      # age,
      # age_retire,
      benef_type1,
      marital_stat,
      # monthly_pension#,
      nat,
      resid,
      # scale,
      sex
    )
  ) %>%
  gather(
    key = variable, value = value,
    -cluster_id,
    -benef_type1,
    -sex,
    -marital_stat,
    -nat,
    -resid
  ) %>%
  mutate(
    variable = dplyr::recode(variable,
      "age" = "Age",
      "ln_aadr" = "ln(AADR)"
    ),
    benef_type1 = dplyr::recode(benef_type1,
      "1" = "Old-age pensioners",
      "0" = "Other types of OASI pensioners"
    ),
    sex = dplyr::recode(sex,
      "1" = "Female", # "Woman"
      "0" = "Male" # "Man"
    ),
    marital_stat = dplyr::recode(marital_stat,
      "1" = "Divorced",
      "2" = "Single",
      "3" = "Married",
      "4" = "Widowed"
    ),
    nat = dplyr::recode(nat,
      "1" = "Foreign",
      "0" = "Swiss"
    ),
    resid = dplyr::recode(resid,
      "1" = "Foreign Country",
      "0" = "Switzerland"
    )
  )
categ_var <- c(
  "sex",
  "nat",
  "resid",
  "benef_type1",
  "marital_stat",
  "cluster_id"
)
pattern <- as.formula(paste(" ~ ", paste(categ_var, collapse = " + ")))
xtab1 <- xtabs(pattern, DATA_FTABLE)
ftab_categ <- ftable(addmargins(xtab1, margin = 2:3, list(Total = sum)))

# ftab_categ <- ftable(DATA_FTABLE %>%
#   dplyr::select(any_of(categ_var)))

write.ftable(ftab_categ, file = file.path(
  path_graphs,
  "ftab_categ.csv"
))

print(
  xtable(format(ftab_categ),
    label = "Clusters Contingency Table",
    caption = "Clusters Contingency Table"
  ),
  tabular.environment = "longtable",
  caption.placement = "top",
  table.placement = "",
  floating = FALSE,
  size = "\\fontsize{8pt}{9pt}\\selectfont",
  file = file.path(path_graphs, "ftab_categ.tex")
)

#--- ggplots per sex, marital_stat and benef_type ------------------------------

unique_sex <- unique(all_csv$PLOTDATKAM$sex)
unique_marital_stat <- unique(all_csv$PLOTDATKAM$marital_stat)
unique_benef_type1 <- unique(all_csv$PLOTDATKAM$benef_type1)

# All combinations
CROSS_TIB <- tibble(unique_sex = unique_sex) %>%
  crossing(
    unique_marital_stat = unique_marital_stat,
    unique_benef_type1 = unique_benef_type1
  ) %>%
  rowwise() %>%
  mutate(
    dta = list(all_csv$PLOTDATKAM %>%
      filter(
        sex == unique_sex,
        marital_stat == unique_marital_stat,
        benef_type1 == unique_benef_type1
      ))
  )

# Plots
suppressWarnings(
PLOTS_TIB <- CROSS_TIB %>%
  mutate(data = list(dta)) %>%
  dplyr::select(-dta) %>%
  mutate(plots = map(list(data), rrclust::fun_ggplot_hist2)) %>%
  dplyr::select(-data)
)

#---- Characteristics of each cluster ------------------------------------------
RR_DESCR <- all_csv$PLOTDATKAM %>%
  dplyr::select(
    -c(
      marital_stat1,
      marital_stat3,
      marital_stat4,
      benef_type1,
      age_std,
      age_retire_std,
      scale_std
    )
  ) %>%
  gather(key = variable, value = values, -cluster_id) %>%
  mutate(variable = as.factor(variable)) %>%
  group_by(cluster_id, variable) %>%
  summarise(ll = list(values), .groups = "keep")

# --- Descriptive Statistics Generation with a list---------------------
list_cluster <- as.factor(RR_DESCR$cluster_id)
list_var <- as.factor(RR_DESCR$variable)
lvalues <- list(values = RR_DESCR$ll)
names(lvalues$values) <- paste(as.character(list_cluster),
  as.character(list_var),
  sep = "_"
)

# Function to produce descriptive statistics
descr_stat_fun(
  descrstat_dpath = descrstat_dpath,
  lvalues = lvalues
)

browseURL(path_graphs)
#--- Plotly --------------------------------------------------------------------

# # Function Plot KAMILA results
# function_kamplotly <- function(dta) {
#   KAMRESDATA <- dta %>%
#     # filter(
#     #   sex == 0,
#     #   benef_type1 == 1
#     # ) %>%
#     arrange(nat, resid) %>%
#     mutate(
#       natres = case_when(
#         nat == 1 &
#           resid == 1 ~ "Foreign living in a foreign country",
#         nat == 1 &
#           resid == 0 ~ "Foreign living in Switzerland",
#         nat == 0 &
#           resid == 1 ~ "Swiss living in a foreign country",
#         nat == 0 &
#           resid == 0 ~ "Swiss living in Switzerland"
#       ),
#       benef_type1 = dplyr::recode(benef_type1,
#                                   "1" = "Old-age pension",
#                                   "0" = "Other OASI pension types"
#       ),
#       sex = dplyr::recode(sex,
#                           "0" = "Male",
#                           "1" = "Female"
#       ),
#       marital_stat = dplyr::recode(marital_stat,
#                                    "1" = "Divorced",
#                                    "2" = "Single",
#                                    "3" = "Married",
#                                    "4" = "Widowed"
#       ),
#       nat = dplyr::recode(nat,
#                           "1" = "Foreign",
#                           "0" = "Swiss"
#       ),
#       resid = dplyr::recode(resid,
#                             "1" = "Foreign Country",
#                             "0" = "Switzerland"
#       )
#     )
#
#
#
#   # pal <- brewer.pal(length(unique(KAMRESDATA$KamilaCluster)), "Paired")
#   axx <- list(
#     nticks = 50,
#     range = c(-25, 75)
#   )
#
#   axy <- list(
#     nticks = 50,
#     range = c(-25, 75)
#   )
#
#   axz <- list(
#     nticks = 50,
#     range = c(0, 50)
#   )
#
#   myplot1 <- KAMRESDATA %>%
#     plot_ly(
#       x = ~age, y = ~scale, z = ~ log(monthly_pension),
#       # colors = pal,
#       type = "scatter3d",
#       mode = "markers",
#       marker = list(size = 5)
#     ) %>%
#     add_markers(color = ~KamilaCluster) %>%
#     layout(
#       scene = list(
#         xaxis = list(title = "x = Age"),
#         yaxis = list(title = "y = Scale"),
#         zaxis = list(title = "z = ln(Monthly pension amount)")
#       ),
#       title = paste0(
#         "Distribution of Monthly pension amount Natural Logarithm: ",
#         paste(unique(KAMRESDATA$sex),
#               unique(KAMRESDATA$benef_type1),
#               "beneficiaries",
#               sep = " "
#         )
#       ),
#       showlegend = TRUE,
#       legend = list(
#         orientation = "h",
#         yanchor = "bottom",
#         xanchor = "center",
#         y = 0,
#         x = 0.5
#       )
#     ) %>%
#     add_annotations(
#       text = "Cluster", xref = "paper", yref = "paper",
#       x = 0.35, xanchor = "left",
#       y = 0, yanchor = "bottom", # Same y as legend below
#       legendtitle = TRUE, showarrow = FALSE
#     )
#
#   # Save plotly
#   htmlwidgets::saveWidget(
#     widget = myplot1,
#     file = file.path(
#       path_graphs,
#       paste(
#         "sex", unique(dta$sex),
#         "typerent", unique(dta$benef_type1),
#         "mr_plotly.html",
#         sep = "_"
#       )
#     )
#   )
#
#
#   myplot2 <- KAMRESDATA %>%
#     plot_ly(
#       x = ~age, y = ~scale, z = ~ log(aadr),
#       # colors = pal,
#       type = "scatter3d",
#       mode = "markers",
#       marker = list(size = 5)
#     ) %>%
#     add_markers(color = ~KamilaCluster) %>%
#     layout(
#       scene = list(
#         xaxis = list(title = "x = Age"),
#         yaxis = list(title = "y = Scale"),
#         zaxis = list(title = "z = ln(AADR)")
#       ),
#       title = paste0(
#         "Distribution of AADR Natural Logarithm: ",
#         paste(unique(KAMRESDATA$sex),
#               unique(KAMRESDATA$benef_type1),
#               "beneficiaries",
#               sep = " "
#         )
#       ),
#       showlegend = TRUE,
#       legend = list(
#         orientation = "h",
#         yanchor = "bottom",
#         xanchor = "center",
#         y = 0,
#         x = 0.5
#       )
#     ) %>%
#     add_annotations(
#       text = "Cluster", xref = "paper", yref = "paper",
#       x = 0.35, xanchor = "left",
#       y = 0, yanchor = "bottom", # Same y as legend below
#       legendtitle = TRUE, showarrow = FALSE
#     )
#
#   # Save plotly
#   htmlwidgets::saveWidget(
#     widget = myplot2,
#     file = file.path(
#       path_graphs,
#       paste(
#         "sex", unique(dta$sex),
#         "typerent", unique(dta$benef_type1),
#         "aadr_plotly.html",
#         sep = "_"
#       )
#     )
#   )
# }
#
#
# # Plots
# PLOTLY_TIB_KAMRES <- CROSS_TIB_KAMRES %>%
#   mutate(data = list(dta)) %>%
#   dplyr::select(-dta) %>%
#   mutate(plots = map(list(data), function_kamplotly)) %>%
#   dplyr::select(-data)



# # FULL_CONT_DF$ln_aadr <- log(FULL_CONT_DF$aadr)
# # conVars <- data.frame(scale(FULL_CONT_DF))
# conVars <- data.frame(FULL_CONT_DF)
# catVarsFac <- lapply(
#   CATFACTOR %>%
#     mutate(marital_stat = FULL_CATEG_DF$marital_stat),
#   factor
# )
# # catVarsDum <- dummyCodeFactorDf(catVarsFac)
# kamRes <- kmres
# KamilaCluster <- factor(kmres$finalMemb)
# plottingData <- cbind(
#   conVars,
#   catVarsFac,
#   KamilaCluster = factor(kamRes$finalMemb)
# )
