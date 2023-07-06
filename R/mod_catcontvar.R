#' Splitting the continuous from the categorical data
#'
#' @description Splits the continuous from the categorical data, in order to use
#' a clustering method.
#'
#' @param RR_OASI a data frame containing the all the data, whose variables are:
#'  - `year`: Year of the pension register extract.
#'  - `age`: Age of the individual.
#'  - `age_retire`: Retirement age.
#'  - `sex`: Sex, if 1: female, if 0:male
#'  - `nat`: Nationality, if 1: Foreign, if 0:Swiss.
#'  - `resid`: Residence, if 1: Foreign, if 0:Swiss.
#'  - `benef_type1`: If 1, Old-age type of benefit (dummy)
#'  - `benef_type2`: Widow type of benefit (dummy)
#'  - `benef_type3`: Father's orphan type of benefit (dummy)
#'  - `benef_type4`: Mother's orphan type of benefit (dummy)
#'  - `benef_type5`: Twice orphan type of benefit (dummy)
#'  - `benef_type6`: Spouse's compl. type of benefit (dummy)
#'  - `benef_type7`: Father's child rent type of benefit (dummy)
#'  - `benef_type8`: Mother's child rent type of benefit (dummy)
#'  - `benef_type` : Types of benefits type of benefit (categorical)
#'  - `marital_stat1`: Divorced marital status (dummy)
#'  - `marital_stat2`: Single as reference category marital status (dummy)
#'  - `marital_stat3`: Married marital status (dummy)
#'  - `marital_stat4`: Widowed marital status (dummy)
#'  - `marital_stat`: Marital Status
#'  - `splitting`: If 1, splitting of the revenues, 0 otherwise.
#'  - `capping`:  If 1, the pension is capped, 0 otherwise.
#'  - `contrib_m_ind`: total number of OASI contribution months per individual.
#'  - `contrib_y_ageclass`: total number of contribution years per age group.
#'  - `bonus_m_edu`: number of months paid with a bonus for educative tasks.
#'  - `bonus_m_assist`: number of months paid with a bonus for assistance/care
#'  tasks.
#'
#' @param RR_OASI_TS a training dataset containing x% of the data.
#'
#' @param RR_OASI_VS a validation dataset containing (100 - x)% of the data.
#'
#' @param PARAM_GLOBAL a data frame containing the parameters. We use the
#' following:
#'  - `categ_var`: Chosen categorical variables
#'  - `cont_var`: Chosen continuous variables
#'
#' @param list List of input data frames.
#'
#' @return a `tidylist` containing the following tidy data frames:
#'   - `CATEG_DF`: Contains only categorical variables (factors).
#'   - `CONT_DF` : Contains only continuous variables (numeric).
#'   - `CATEG_DF_TS`: Contains only categorical variables (factors),
#'   training set.
#'   - `CONT_DF_TS` : Contains only continuous variables (numeric),
#'   training set.
#'   - `CATEG_DF_VS`: Contains only categorical variables (factors),
#'    validation set.
#'   - `CONT_DF_VS` : Contains only continuous variables (numeric),
#'   validation set.
#'
#' @references \url{https://www.geeksforgeeks.org/the-validation-set-approach-in-r-programming/}
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export

mod_catcontvar <- function(RR_OASI,
                           RR_OASI_TS,
                           RR_OASI_VS,
                           PARAM_GLOBAL,
                           list = NULL) {
  mod_init()

  categ_var <- separate_at_comma(PARAM_GLOBAL$categ_var)
  cont_var <- separate_at_comma(PARAM_GLOBAL$cont_var)

  CATEG_DF <- RR_OASI |>
    select(any_of(categ_var)) |>
    mutate_all(as.factor)

  CONT_DF <- RR_OASI |>
    select(any_of(cont_var)) |>
    mutate_all(as.numeric)

  print(dim(CATEG_DF))
  print(dim(CONT_DF))

  CATEG_DF_TS <- RR_OASI_TS |>
    select(any_of(categ_var)) |>
    mutate_all(as.factor)

  CONT_DF_TS <- RR_OASI_TS |>
    select(any_of(cont_var)) |>
    mutate_all(as.numeric)

  print(dim(CATEG_DF_TS))
  print(dim(CONT_DF_TS))

  CATEG_DF_VS <- RR_OASI_VS |>
    select(any_of(categ_var)) |>
    mutate_all(as.factor)

  CONT_DF_VS <- RR_OASI_VS |>
    select(any_of(cont_var)) |>
    mutate_all(as.numeric)

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
