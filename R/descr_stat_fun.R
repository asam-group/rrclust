#' @title Function to generate descriptive statistics
#'
#' @description Generates descriptive statistics as the mean, the median, ...
#'
#' @param descrstat_dpath directory to store the tex files containing the descriptive
#' statistics.
#'
#' @param lvalues List of variables and values to be analysed. The list must have
#' variable names.
#'
#' @return a some latex outputs.
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#'
#' @export

# - `Last change`: 2019-08-09 / Llc
# - `Code review`:
# - `Last test`: 2019-08-09 / Llc

descr_stat_fun <- function(descrstat_dpath, lvalues) {

  # --- Needed Libraries ---------------------------------------------------------
  library(Hmisc) # procedures: label, units, contents, describe, bystats, summarize, html, format.df

  # --- Loop -------------------------------------------------------------------
  for (i in 1:length(lvalues$values)) {
    x <- c(lvalues$values[[i]])
    name_x <- names(lvalues$values)[i]

    file_tex <- file.path(
      descrstat_dpath,
      paste0("descr_stats_", name_x, ".tex")
    )

    file_hist_tex <- file.path(
      descrstat_dpath,
      paste0("descr_hist_", name_x, ".tex")
    )

    # --- Function describe for the histogram ----------------------------------

    des.x <- Hmisc::describe(x, descript = name_x)
    tab1l <- latex(des.x, file = file_hist_tex)

    # --- Definition of ad hoc functions ---------------------------------------
    # Number of non NA observations
    nmiss <- function(x) {
      sum(!is.na(x))
    }

    # Standard error
    se <- function(x) {
      # Standard error of x
      apply(x, 2, sd, na.rm = T) / sqrt(nmiss(x))
    }


    # --- Function "My descriptive" ------------------------------------------------

    mydescriptive <- function(x) {

      # Ad hoc table of descriptive statistics
      des <- c(
        # "N non NA" = nmiss(x),
        "Mean" = mean(x, na.rm = T),
        "S.D." = apply(x, 2, sd, na.rm = T),
        "S.E." = se(x),
        "Min." = min(x, na.rm = T),
        "1st quartile" = quantile(x, probs = 0.25, na.rm = T),
        "Median" = median(x, na.rm = T),
        "3rd quartile" = quantile(x, probs = 0.75, na.rm = T),
        "Max." = max(x, na.rm = T)
      )
      return(des)
    }
    tab2_1 <- Hmisc::bystats(y = x, title = name_x, fun = function(x) mydescriptive(x), nmiss = T)
    tab2 <- tab2_1[1, ]
    names(tab2) <- c(
      names(tab2)[1:6],
      "1st quartile",
      names(tab2)[8],
      "3rd quartile",
      names(tab2)[10]
    )
    label(tab2) <- name_x

    tab2l <- latex(format(round(tab2, 2), scientific = F),
      file = file_tex,
      label = paste0("descr_stats_", name_x),
      size = "small",
      rowlabel = name_x
    )
  }
}
