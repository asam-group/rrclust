path_out <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/outputs"

output_name <- "cl_kamila_20210318093616_layal_kamila"
path_output <- file.path(
  path_out,
  output_name
)
path_graphs <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/outputs/graphs"
subfolder <- "without_mr"

library(ggplot2)
library(rrclust)
library(xtable)
library(purrr)

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
      path_graphs, subfolder,
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
