path_out <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/outputs"

output_name <- "cl_kamila_20210311161651_layal_kamila"
path_output <- file.path(
  path_out,
  output_name
)
path_graphs <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/outputs/graphs"
subfolder <- "without_mr"

library(ggplot2)
library(rrclust)
library(xtable)

all_csv <- rrclust::tidylist_read(path_output)
all_csv$PLOTDATKAM
numb_clust <- max(as.double(all_csv$PLOTDATKAM$cluster_id))

#--- Table cluster id, benef_type ---------------------------------------------
tab_benef_clust <- table(all_csv$PLOTDATKAM$cluster_id, all_csv$PLOTDATKAM$benef_type)
print(
  xtable(format(tab_benef_clust)),
  file = file.path(path_graphs, subfolder, "tab_benef_clust.tex")
)

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
  ggsave(file.path(path_graphs, subfolder, "psplot.png"),
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
ftab_categ <- ftable(DATA_FTABLE %>%
  dplyr::select(any_of(categ_var)))
write.ftable(ftab_categ, file = file.path(
  path_graphs, subfolder,
  "ftab_categ.csv"
))


print(
  xtable(format(ftab_categ)),
  file = file.path(path_graphs, subfolder, "ftab_categ.tex")
)

#--- Tibble 1.1 ----------------------------------------------------
GATHERED_TIB1_1 <- all_csv$PLOTDATKAM %>%
  filter(
    sex == 1,
    marital_stat == 3,
    benef_type1 == 1
  )
#--- Tibble 1.2 ----------------------------------------------------
GATHERED_TIB1_2 <- all_csv$PLOTDATKAM %>%
  filter(
    sex == 0,
    marital_stat == 3,
    benef_type1 == 1
  )
#--- Tibble 1.3 ----------------------------------------------------
GATHERED_TIB1_3 <- all_csv$PLOTDATKAM %>%
  filter(
    sex == 1,
    marital_stat == 3,
    benef_type1 == 0
  )
#--- Tibble 1.4 ----------------------------------------------------
GATHERED_TIB1_4 <- all_csv$PLOTDATKAM %>%
  filter(
    sex == 1,
    marital_stat == 3,
    benef_type1 == 1
  )
#--- Tibble 1.5 ----------------------------------------------------
GATHERED_TIB1_5 <- all_csv$PLOTDATKAM %>%
  filter(
    sex == 1,
    marital_stat == 1,
    benef_type1 == 1
  )
#--- Tibble 1.6 ----------------------------------------------------
GATHERED_TIB1_6 <- all_csv$PLOTDATKAM %>%
  filter(
    sex == 0,
    marital_stat == 1,
    benef_type1 == 1
  )
#--- Tibble 1.7 ----------------------------------------------------
GATHERED_TIB1_7 <- all_csv$PLOTDATKAM %>%
  filter(
    sex == 1,
    marital_stat == 2,
    benef_type1 == 1
  )
#--- Tibble 1.8 ----------------------------------------------------
GATHERED_TIB1_8 <- all_csv$PLOTDATKAM %>%
  filter(
    sex == 0,
    marital_stat == 2,
    benef_type1 == 1
  )
#--- Tibble 1.9 ----------------------------------------------------
GATHERED_TIB1_9 <- all_csv$PLOTDATKAM %>%
  filter(
    sex == 1,
    marital_stat == 4,
    benef_type1 == 1
  )

#--- Tibble 1.10 ----------------------------------------------------
GATHERED_TIB1_10 <- all_csv$PLOTDATKAM %>%
  filter(
    sex == 0,
    marital_stat == 4,
    benef_type1 == 1
  )
# --- FUNCTION ggplot hitogram -------------------------------------------------
fun_ggplot_hist <- function(DATA_GGPLOT, path_graphs) {
  DATA_GGPLOT %>%
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
    ) %>%
    ggplot(aes(value, fill = as.factor(cluster_id))) +
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
      subtitle = paste0(numb_clust, " clusters"),
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
      path_graphs, subfolder,
      paste(
        "hist",
        "sex", unique(DATA_GGPLOT$sex),
        "typerent", unique(DATA_GGPLOT$benef_type1),
        "mstat", unique(DATA_GGPLOT$marital_stat),
        numb_clust,
        "clusters.png",
        sep = "_"
      )
    ),
    height = 8.27,
    width = 11.69
    )

  # Return
  print(file.path(
    path_graphs, subfolder,
    paste(
      "hist",
      "sex", unique(DATA_GGPLOT$sex),
      "typerent", unique(DATA_GGPLOT$benef_type1),
      "mstat", unique(DATA_GGPLOT$marital_stat),
      numb_clust,
      "clusters.png",
      sep = "_"
    )
  ))
}

g01 <- fun_ggplot_hist(
  DATA_GGPLOT = GATHERED_TIB1_1,
  path_graphs = path_graphs
)
g02 <- fun_ggplot_hist(
  DATA_GGPLOT = GATHERED_TIB1_2,
  path_graphs = path_graphs
)

g03 <- fun_ggplot_hist(
  DATA_GGPLOT = GATHERED_TIB1_3,
  path_graphs = path_graphs
)
g04 <- fun_ggplot_hist(
  DATA_GGPLOT = GATHERED_TIB1_4,
  path_graphs = path_graphs
)


g05 <- fun_ggplot_hist(
  DATA_GGPLOT = GATHERED_TIB1_5,
  path_graphs = path_graphs
)
g06 <- fun_ggplot_hist(
  DATA_GGPLOT = GATHERED_TIB1_6,
  path_graphs = path_graphs
)

g07 <- fun_ggplot_hist(
  DATA_GGPLOT = GATHERED_TIB1_7,
  path_graphs = path_graphs
)
g08 <- fun_ggplot_hist(
  DATA_GGPLOT = GATHERED_TIB1_8,
  path_graphs = path_graphs
)

g09 <- fun_ggplot_hist(
  DATA_GGPLOT = GATHERED_TIB1_9,
  path_graphs = path_graphs
)
g10 <- fun_ggplot_hist(
  DATA_GGPLOT = GATHERED_TIB1_10,
  path_graphs = path_graphs
)


#---  CROSS_TIB ----------------------------------------------------------------
unique_sex <- unique(all_csv$PLOTDATKAM$sex)
unique_marital_stat <- unique(all_csv$PLOTDATKAM$marital_stat)
unique_benef_type1 <- unique(all_csv$PLOTDATKAM$benef_type1)

CROSS_TIB <- tibble(unique_sex = unique_sex) %>%
  crossing(
    unique_marital_stat = unique_marital_stat,
    unique_benef_type1 = unique_benef_type1
  ) %>%
  rowwise() %>%
  mutate(dta = list(all_csv$PLOTDATKAM %>%
    filter(
      sex == unique_sex,
      marital_stat == unique_marital_stat,
      benef_type1 == unique_benef_type1
    ))) %>%
  rowwise() %>%
  mutate(ggpl = list(map(dta, fun_ggplot_hist2)))


fun_ggplot_hist2 <- function(DATA_GGPLOT) {

  ggplot_number <- DATA_GGPLOT %>%
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
    ) %>%
    ggplot(aes(value, fill = as.factor(cluster_id))) +
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
      subtitle = paste0(numb_clust, " clusters"),
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
      path_graphs, subfolder,
      paste(
        "hist",
        "sex", unique(DATA_GGPLOT$sex),
        "typerent", unique(DATA_GGPLOT$benef_type1),
        "mstat", unique(DATA_GGPLOT$marital_stat),
        numb_clust,
        "clusters.png",
        sep = "_"
      )
    ),
    height = 8.27,
    width = 11.69
    )

  # Return
  print(file.path(
    path_graphs, subfolder,
    paste(
      "hist",
      "sex", unique(DATA_GGPLOT$sex),
      "typerent", unique(DATA_GGPLOT$benef_type1),
      "mstat", unique(DATA_GGPLOT$marital_stat),
      numb_clust,
      "clusters.png",
      sep = "_"
    )
  ))

}
