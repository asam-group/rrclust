#' @title Function to make scatterplot using clusters distribution
#' @param dta Filtered dataframe for each desired population sample
#' @export
function_kamplot <- function(dta) {
  KAMRESDATA <- dta %>%
    # filter(
    #   sex == 0,
    #   benef_type1 == 1
    # ) %>%
    arrange(nat, resid) %>%
    mutate(
      natres = case_when(
        nat == 1 &
          resid == 1 ~ "Foreign living in a foreign country",
        nat == 1 &
          resid == 0 ~ "Foreign living in Switzerland",
        nat == 0 &
          resid == 1 ~ "Swiss living in a foreign country",
        nat == 0 &
          resid == 0 ~ "Swiss living in Switzerland"
      ),
      benef_type1 = dplyr::recode(benef_type1,
        "1" = "Old-age insurance",
        "0" = "Survivor insurance"
      ),
      sex = dplyr::recode(sex,
        "0" = "Male",
        "1" = "Female"
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

  #--- Log Monthly Rent Plot ---------------------------------------------------

  # Log Monthly Rent pro Age
  kamPlot1 <- KAMRESDATA %>%
    ggplot(
      aes(
        x = age,
        y = log(monthly_rent),
        # shape = groups2,
        color = KamilaCluster
      )
    ) +
    facet_wrap(marital_stat ~ natres) +
    theme_light() +
    labs(
      title = "Distribution of Monthly Rent Natural Logarithm",
      subtitle = paste(unique(KAMRESDATA$sex),
        unique(KAMRESDATA$benef_type1),
        "beneficiaries",
        sep = " "
      ),
      x = "Age",
      y = "Monthly Rent Natural Logarithm",
      caption = paste(
        Sys.Date(),
        "Llc",
        sep = ", "
      )
    ) +
    theme(
      strip.text.x = element_text(
        size = 11, color = "black", face = "bold"
      ),
      strip.text.y = element_text(
        size = 11, color = "black", face = "bold"
      ),
      legend.position = "bottom",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    ) +
    scale_color_discrete("Cluster") +
    scale_x_continuous(breaks = seq(0, 100, 10))

  p1 <- plotOpts(kamPlot1) +
    ggsave(file.path(
      path_graphs,
      paste(
        "sex", unique(dta$sex),
        "typerent", unique(dta$benef_type1),
        "mr_age.png",
        sep = "_"
      )
    ),
    height = 8.27,
    width = 11.69
    )

  # Log Monthly Rent pro Scale
  kamPlot2 <- KAMRESDATA %>%
    ggplot(
      aes(
        x = scale,
        y = log(monthly_rent),
        # shape = groups2,
        color = KamilaCluster
      )
    ) +
    facet_wrap(marital_stat ~ natres) +
    theme_light() +
    labs(
      title = "Distribution of Monthly Rent Natural Logarithm",
      subtitle = paste(unique(KAMRESDATA$sex),
        unique(KAMRESDATA$benef_type1),
        "beneficiaries",
        sep = " "
      ),
      x = "Scale",
      y = "Monthly Rent Natural Logarithm",
      caption = paste(
        Sys.Date(),
        "Llc",
        sep = ", "
      )
    ) +
    theme(
      strip.text.x = element_text(
        size = 11, color = "black", face = "bold"
      ),
      strip.text.y = element_text(
        size = 11, color = "black", face = "bold"
      ),
      legend.position = "bottom",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    ) +
    scale_color_discrete("Cluster") +
    scale_x_continuous(breaks = seq(0, 44, 4))

  p2 <- plotOpts(kamPlot2) +
    ggsave(file.path(
      path_graphs,
      paste(
        "sex", unique(dta$sex),
        "typerent", unique(dta$benef_type1),
        "mr_scale.png",
        sep = "_"
      )
    ),
    height = 8.27,
    width = 11.69
    )

  if (KAMRESDATA$age_retire > 0) {
    # Log Monthly Rent pro age_retire
    kamPlot5 <- KAMRESDATA %>%
      ggplot(
        aes(
          x = age_retire,
          y = log(monthly_rent),
          # shape = groups2,
          color = KamilaCluster
        )
      ) +
      facet_wrap(marital_stat ~ natres) +
      theme_light() +
      labs(
        title = "Distribution of Monthly Rent Natural Logarithm",
        subtitle = paste(unique(KAMRESDATA$sex),
          unique(KAMRESDATA$benef_type1),
          "beneficiaries",
          sep = " "
        ),
        x = "Age of Effective Retirement",
        y = "Monthly Rent Natural Logarithm",
        caption = paste(
          Sys.Date(),
          "Llc",
          sep = ", "
        )
      ) +
      theme(
        strip.text.x = element_text(
          size = 11, color = "black", face = "bold"
        ),
        strip.text.y = element_text(
          size = 11, color = "black", face = "bold"
        ),
        legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)
      ) +
      scale_color_discrete("Cluster") +
      scale_x_continuous(breaks = seq(min(KAMRESDATA$age_retire), max(KAMRESDATA$age_retire), 1))

    p5 <- plotOpts(kamPlot5) +
      ggsave(file.path(
        path_graphs,
        paste(
          "sex", unique(dta$sex),
          "typerent", unique(dta$benef_type1),
          "mr_age_retire.png",
          sep = "_"
        )
      ),
      height = 8.27,
      width = 11.69
      )
  }
  #--- AADR Plot ---------------------------------------------------------------
  # Log AADR pro Age
  kamPlot3 <- KAMRESDATA %>%
    ggplot(
      aes(
        x = age,
        y = log(aadr),
        # shape = groups2,
        color = KamilaCluster
      )
    ) +
    facet_wrap(marital_stat ~ natres) +
    theme_light() +
    labs(
      title = "Distribution of AADR Natural Logarithm",
      subtitle = paste(unique(KAMRESDATA$sex),
        unique(KAMRESDATA$benef_type1),
        "beneficiaries",
        sep = " "
      ),
      x = "Age",
      y = "AADR Natural Logarithm",
      caption = paste(
        Sys.Date(),
        "Llc",
        sep = ", "
      )
    ) +
    theme(
      strip.text.x = element_text(
        size = 11, color = "black", face = "bold"
      ),
      strip.text.y = element_text(
        size = 11, color = "black", face = "bold"
      ),
      legend.position = "bottom",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    ) +
    scale_color_discrete("Cluster") +
    scale_x_continuous(breaks = seq(0, 100, 10))

  p3 <- plotOpts(kamPlot3) +
    ggsave(file.path(
      path_graphs,
      paste(
        "sex", unique(dta$sex),
        "typerent", unique(dta$benef_type1),
        "aadr_age.png",
        sep = "_"
      )
    ),
    height = 8.27,
    width = 11.69
    )

  # Log AADR pro Scale
  kamPlot4 <- KAMRESDATA %>%
    ggplot(
      aes(
        x = scale,
        y = log(aadr),
        # shape = groups2,
        color = KamilaCluster
      )
    ) +
    facet_wrap(marital_stat ~ natres) +
    theme_light() +
    labs(
      title = "Distribution of AADR Natural Logarithm",
      subtitle = paste(unique(KAMRESDATA$sex),
        unique(KAMRESDATA$benef_type1),
        "beneficiaries",
        sep = " "
      ),
      x = "Scale",
      y = "AADR Natural Logarithm",
      caption = paste(
        Sys.Date(),
        "Llc",
        sep = ", "
      )
    ) +
    theme(
      strip.text.x = element_text(
        size = 11, color = "black", face = "bold"
      ),
      strip.text.y = element_text(
        size = 11, color = "black", face = "bold"
      ),
      legend.position = "bottom",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    ) +
    scale_color_discrete("Cluster") +
    scale_x_continuous(breaks = seq(0, 44, 4))

  p4 <- plotOpts(kamPlot4) +
    ggsave(file.path(
      path_graphs,
      paste(
        "sex", unique(dta$sex),
        "typerent", unique(dta$benef_type1),
        "aadr_scale.png",
        sep = "_"
      )
    ),
    height = 8.27,
    width = 11.69
    )

  if (KAMRESDATA$age_retire > 0) {
    # Log AADR pro Age_retire
    kamPlot6 <- KAMRESDATA %>%
      ggplot(
        aes(
          x = age_retire,
          y = log(aadr),
          # shape = groups2,
          color = KamilaCluster
        )
      ) +
      facet_wrap(marital_stat ~ natres) +
      theme_light() +
      labs(
        title = "Distribution of AADR Natural Logarithm",
        subtitle = paste(unique(KAMRESDATA$sex),
          unique(KAMRESDATA$benef_type1),
          "beneficiaries",
          sep = " "
        ),
        x = "Age of Effective Retirement",
        y = "AADR Natural Logarithm",
        caption = paste(
          Sys.Date(),
          "Llc",
          sep = ", "
        )
      ) +
      theme(
        strip.text.x = element_text(
          size = 11, color = "black", face = "bold"
        ),
        strip.text.y = element_text(
          size = 11, color = "black", face = "bold"
        ),
        legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)
      ) +
      scale_color_discrete("Cluster") +
      scale_x_continuous(breaks = seq(min(KAMRESDATA$age_retire), max(KAMRESDATA$age_retire), 1))

    p6 <- plotOpts(kamPlot6) +
      ggsave(file.path(
        path_graphs,
        paste(
          "sex", unique(dta$sex),
          "typerent", unique(dta$benef_type1),
          "aadr_age_retire.png",
          sep = "_"
        )
      ),
      height = 8.27,
      width = 11.69
      )
  }
}


#' @title Function to make histograms using clusters distribution
#' @param dta Filtered dataframe for each desired population sample
#' @export
#'
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
                      breaks = c("1", "2", "3", "4", "5"),
                      values = c("red", "blue", "green", "orange", "violet")
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