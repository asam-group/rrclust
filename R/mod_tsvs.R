#' @title Splitting the data into a Training and a Validation sets
#'
#' @description Splits the data into a training and a validation sets, given
#' the too large number of observations, in order to determine the best number of
#' clusters for the KAMILA algorithm.
#'
#' @param RR_OASI a data frame containing the all the data.
#'
#' @param PARAM_GLOBAL a data frame containing the parameters. We use the following:
#'  - `pct_sample_ts`: percentage of observations which build the training set.
#'
#' @param list List of input data frames.
#'
#' @return a `tidylist` containing the following tidy data frames:
#'   - `RR_OASI_TS` : Training set of categorical data.
#'   - `RR_OASI_VS`: Validation set of categorical data.
#'
#' @references [www.geeksforgeeks.org](https://www.geeksforgeeks.org/the-validation-set-approach-in-r-programming/)
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
#' @example inst/examples/ex-mod_tsvs.R

mod_tsvs <- function(RR_OASI,
                     PARAM_GLOBAL,
                     list = NULL) {
  mod_init()

  # Setting seed to generate a reproducible random sampling
  set.seed(100)

  # Choose a categorical balanced variable to do the splitting: sex and check
  # if it is balanced
  freqtable <- table(RR_OASI$sex)
  proptable <- prop.table(freqtable) # approximately balanced

  # Dividing the complete RR_OASI dataset into 2 parts having ratio of
  # (1 - PARAM_GLOBAL$pct_sample_ts)% and PARAM_GLOBAL$pct_sample_ts%
  oasi_spl <- sample.split(RR_OASI$sex,
    SplitRatio = PARAM_GLOBAL$pct_sample_ts / 100
  )

  # Training set
  # Selecting that part of RR_OASI dataset which belongs to the
  # PARAM_GLOBAL$pct_sample_ts% of the dataset
  # divided in previous step
  RR_OASI_TS <- subset(RR_OASI, oasi_spl == TRUE)

  # Validation set
  # Selecting that part of RR_OASI dataset which belongs to the
  # (1 - PARAM_GLOBAL$pct_sample_ts)% of the dataset
  # divided in previous step
  RR_OASI_VS <- subset(RR_OASI, oasi_spl == FALSE)

  # checking number of rows and column in training and validation datasets
  print(dim(RR_OASI_TS))
  print(dim(RR_OASI_VS))
  print(proptable)

  mod_return(
    RR_OASI_TS,
    RR_OASI_VS
  )
}
