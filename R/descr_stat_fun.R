#' Function to generate descriptive statistics
#'
#' Generate descriptive statistics as the mean, the median, among others.
#'
#' @param descrstat_dpath Directory to store the tex files containing the
#' descriptive statistics.
#' @param lvalues List of variables and values to be analysed. The list must
#' have variable names.
#'
#' @return Latex tables.
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
descr_stat_fun <- function(descrstat_dpath, lvalues) {
  for (i in seq_along(lvalues$values)) {
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

    des.x <- describe(x, descript = name_x)
    tab1l <- latex(des.x, file = file_hist_tex)
    tab1l

    nmiss <- function(x) {
      sum(!is.na(x))
    }

    se <- function(x) {
      apply(x, 2, sd, na.rm = TRUE) / sqrt(nmiss(x))
    }

    mydescriptive <- function(x) {
      des <- c(
        "Mean" = mean(x, na.rm = TRUE),
        "S.D." = apply(x, 2, sd, na.rm = TRUE),
        "S.E." = se(x),
        "Min." = min(x, na.rm = TRUE),
        "1st quartile" = quantile(x, probs = 0.25, na.rm = TRUE),
        "Median" = median(x, na.rm = TRUE),
        "3rd quartile" = quantile(x, probs = 0.75, na.rm = TRUE),
        "Max." = max(x, na.rm = TRUE)
      )
      return(des)
    }
    tab2_1 <- bystats(
      y = x,
      title = name_x,
      fun = function(x) mydescriptive(x),
      nmiss = TRUE
    )
    tab2 <- tab2_1[1, ]
    names(tab2) <- c(
      names(tab2)[1:6],
      "1st quartile",
      names(tab2)[8],
      "3rd quartile",
      names(tab2)[10]
    )
    label(tab2) <- name_x

    tab2l <- latex(format(round(tab2, 2), scientific = FALSE),
      file = file_tex,
      label = paste0("descr_stats_", name_x),
      size = "small",
      rowlabel = name_x
    )
    tab2l
  }
}
