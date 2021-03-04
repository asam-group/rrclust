#' @title Splitting the continuous from the categorical data
#'
#' @description Splits the continuous from the categorical data, in order to use
#' a clustering method.
#'
#' @param RR_OASI a data frame containing the all the data.
#' @param RR_OASI_TS a training dataset containing x% of the data.
#' @param RR_OASI_VS a validation dataset containing (100 - x)% of the data.
#'
#' @param list List of input data frames.
#'
#' @return a `tidylist` containing the following tidy data frames:
#'   - `CATEG_DF`: contains only categorical variables (factors)
#'   - `CONT_DF` : contains only continuous variables (numeric)

#'
#' @references [www.geeksforgeeks.org](https://www.geeksforgeeks.org/the-validation-set-approach-in-r-programming/)
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#' @import caTools
#' @export

# - `Last change`: 2021-02-25 / Llc

mod_catcontvar <- function(RR_OASI,
                           RR_OASI_TS,
                           RR_OASI_VS,
                           list = NULL) {
  mod_init()


  # Desired categorical variables
  categ_var <- c(
    "sex",
    "nat",
    "resid",
    "benef_type",
    "marital_stat",
    "scale"
  )

  # Desired continuous variables
  cont_var <- c(
    "aadr",
    "monthly_rent",
    "age",
    "age_retire"
  )

  #--- Full dataset ------------------------------------------------------------
  # Dataframe of categorical variables
  CATEG_DF <- RR_OASI %>%
    dplyr::select(any_of(categ_var)) %>%
    # Transform all variables as factors
    mutate_all(as.factor)

  # Dataframe of continuous variables
  CONT_DF <- RR_OASI %>%
    dplyr::select(any_of(cont_var)) %>%
    # Transform all variables as numeric
    mutate_all(as.numeric)

  # checking number of rows and column training datasets
  print(dim(CATEG_DF))
  print(dim(CONT_DF))

  #--- Training set ------------------------------------------------------------
  # Dataframe of categorical variables
  CATEG_DF_TS <- RR_OASI_TS %>%
    dplyr::select(any_of(categ_var)) %>%
    # Transform all variables as factors
    mutate_all(as.factor)

  # Dataframe of continuous variables
  CONT_DF_TS <- RR_OASI_TS %>%
    dplyr::select(any_of(cont_var)) %>%
    # Transform all variables as numeric
    mutate_all(as.numeric)

  # checking number of rows and column training datasets
  print(dim(CATEG_DF_TS))
  print(dim(CONT_DF_TS))

  #--- Validation set ------------------------------------------------------------
  # Dataframe of categorical variables
  CATEG_DF_VS <- RR_OASI_VS %>%
    dplyr::select(any_of(categ_var)) %>%
    # Transform all variables as factors
    mutate_all(as.factor)

  # Dataframe of continuous variables
  CONT_DF_VS <- RR_OASI_VS %>%
    dplyr::select(any_of(cont_var)) %>%
    # Transform all variables as numeric
    mutate_all(as.numeric)

  # checking number of rows and column validation datasets
  print(dim(CATEG_DF_VS))
  print(dim(CONT_DF_VS))

  mod_return(
    CATEG_DF,
    CONT_DF,

    CATEG_DF_TS,
    CONT_DF_TS,

    CATEG_DF_VS,
    CONT_DF_VS
  )
}