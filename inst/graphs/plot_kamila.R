
# Libraries
library(ggplot2)
library(rrclust)
library(xtable)
library(purrr)
library(delfin)
library(vtable)

# Output directory
path_out <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/outputs"
output_name <- "cl_kamila_20210318093616_layal_kamila"
path_output <- file.path(
  path_out,
  output_name
)

path_input <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/inputs/inp_kamila/all"

# Retrieve outputs and inputs
all_csv <- rrclust::tidylist_read(path_output)
all_csv_inputs <- rrclust::tidylist_read(path_input)

# all_csv$PLOTDATKAM
numb_clust <- max(as.double(all_csv$PLOTDATKAM$cluster_id))

# Graphs directory
path_allgraph <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/outputs/graphs"
path_graphs <- file.path(path_allgraph, paste(numb_clust, "clusters", sep = "_"))
if (file.exists(path_graphs)) stop(path_graphs, " already exists")
if (!file.exists(path_graphs)) {
  fs::dir_create(path_graphs, recursive = TRUE)
}
# Descriptive Stats directory
descrstat_dpath <- file.path(path_graphs, "descrstat")
if (file.exists(descrstat_dpath)) stop(descrstat_dpath, " already exists")
if (!file.exists(descrstat_dpath)) {
  fs::dir_create(descrstat_dpath, recursive = TRUE)
}
# Output copy directory
output_dir <- file.path(path_graphs, "r_output")
if (file.exists(output_dir)) stop(output_dir, " already exists")
if (!file.exists(output_dir)) {
  fs::dir_create(output_dir, recursive = TRUE)
  file.copy(path_output, output_dir, recursive = TRUE)
}
#--- Table Variable characteristics tibble ---------------------------------------------
RR_OASI <- mod_prepa_rr(all_csv_inputs$IND_YEARLY_RR)$RR_OASI %>%
  dplyr::select(-age_retire)

print(
  st(RR_OASI,
     file = file.path(path_graphs, "RR_DESCR"),
     anchor = "sum_stats_rr_oasi",
     out = 'latex'),
  tabular.environment = "longtable",
  caption.placement = "top",
  table.placement = "",
  floating = FALSE,
  size = "\\fontsize{8pt}{9pt}\\selectfont",
  file = file.path(path_graphs, "RR_DESCR.tex")
)
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
  ylim(0, 1.1) +
  ggsave(file.path(path_graphs, "psplot.png"),
    height = 8.27,
    width = 11.69
  )

#--- Ftable ----------------------------------------------------
DATA_FTABLE <- all_csv$PLOTDATKAM %>%
  mutate(
    ln_aadr = log(aadr),
    ln_monthly_rent = log(monthly_rent),
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
      # monthly_rent#,
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
      "1" = "Old-age insurance beneficiaries",
      "0" = "Survivor insurance beneficiaries"
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

# Function to make ggplots for all combinations
fun_ggplot_hist2 <- function(dta) {
  GGDATA <- dta %>%
    mutate(
      ln_aadr = log(aadr),
      ln_monthly_rent = log(monthly_rent),
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
        # monthly_rent,
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
        "1" = "Old-age insurance beneficiaries",
        "0" = "Survivor insurance beneficiaries"
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
        "1" = "Foreign Nationality",
        "0" = "Swiss Nationality"
      ),
      resid = dplyr::recode(resid,
        "1" = "Living Abroad",
        "0" = "Living in CH"
      )
    )

  n_ind <- unique(GGDATA %>%
    mutate(n_ind = n()) %>%
    dplyr::select(n_ind))$n_ind


  ggplot(
    GGDATA,
    aes(value, fill = as.factor(cluster_id))
  ) +
    facet_wrap(benef_type1 + sex + nat + resid ~ marital_stat + variable,
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
      title = "Histograms for continuous variables in clusters",
      subtitle = paste0(
        numb_clust, " clusters, ",
        n_ind, " individuals"
      ),
      x = "Value",
      y = "Frequency",
      caption = paste(
        Sys.Date(),
        "Llc",
        sep = ", "
      )
    ) +
    scale_fill_manual("Cluster",
      breaks = c("1", "2", "3", "4"),
      values = c("red", "blue", "green", "orange")
    ) +
    ggsave(file.path(
      path_graphs,
      paste(
        "hist",
        "sex", unique(GGDATA$sex),
        "typerent", unique(GGDATA$benef_type1),
        "mstat", unique(GGDATA$marital_stat),
        numb_clust,
        "clusters.png",
        sep = "_"
      )
    ),
    height = 8.27,
    width = 11.69
    )
}

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
PLOTS_TIB <- CROSS_TIB %>%
  mutate(data = list(dta)) %>%
  dplyr::select(-dta) %>%
  mutate(plots = map(list(data), fun_ggplot_hist2)) %>%
  dplyr::select(-data)


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
