#' Prepare the pension register data
#'
#' Rename the variables of the pension register.
#'
#' @param IND_YEARLY_RR A data frame containing the data of the pension register
#'   subsetted for one year only.
#' @param list List of input data frames.
#'
#' @return a `tidylist` containing the following tidy data frames:
#'   - `RR_OASI` : A data frame containing all OASI data, whose variables are:
#'   variables:
#'     - `year`: Year of the pension register extract.
#'     - `age`: Age of the individual.
#'     - `age_retire`: Effective retirement age.
#'     - `sex`: Sex, female if female, 0 if male.
#'     - `nat`: Nationality, foreign if 1, 0 if Swiss.
#'     - `resid`: Residence, foreign if 1, 0 if Swiss.
#'     - `benef_type1`: Old-age type of benefit if 1, 0 otherwise (dummy).
#'     - `benef_type2`: Widow type of benefit if 1, 0 otherwise (dummy).
#'     - `benef_type3`: Father's orphan type of benefit if 1, 0 otherwise (dummy).
#'     - `benef_type4`: Mother's orphan type of benefit if 1, 0 otherwise (dummy).
#'     - `benef_type5`: Twice orphan type of benefit if 1, 0 otherwise (dummy).
#'     - `benef_type6`: Spouse's compl. type of benefit if 1, 0 otherwise (dummy).
#'     - `benef_type7`: Father's child rent type of benefit if 1, 0 otherwise
#'     (dummy).
#'     - `benef_type8`: Mother's child rent type of benefit if 1, 0 otherwise
#'     (dummy).
#'     - `benef_type` : Types of benefits type of benefit (categorical).
#'     - `marital_stat1`: Divorced marital status if 1, 0 otherwise (dummy).
#'     - `marital_stat2`: Single as reference category marital status if 1, 0
#'     otherwise (dummy).
#'     - `marital_stat3`: Married marital status if 1, 0 otherwise (dummy).
#'     - `marital_stat4`: Widowed marital status if 1, 0 otherwise (dummy).
#'     - `marital_stat`: Marital status.
#'     - `splitting`: If 1, splitting of the revenues, 0 otherwise.
#'     - `capping`:  If 1, the pension is capped, 0 otherwise.
#'     - `contrib_m_ind`: Total number of OASI contribution months per individual.
#'     - `contrib_y_ageclass`: Total number of contribution years per age group.
#'     - `bonus_m_edu`: Number of months paid with a bonus for educative tasks.
#'     - `bonus_m_assist`: Number of months paid with a bonus for assistance/care
#'     tasks.
#'
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @autoglobal
#' @export
#' @example inst/examples/ex-mod_prepa_rr.R
mod_prepa_rr <- function(IND_YEARLY_RR,
                         list = NULL) {
  mod_init()

  RR_OASI1 <- if ("lbedu" %in% names(IND_YEARLY_RR)) {
    IND_YEARLY_RR |>
      dplyr::rename(
        "aadr" = ram,
        "monthly_pension" = monatliche_rente,
        "age" = alt,
        "year" = jahr,
        "resid" = dom,
        "contrib_m_ind" = lcot,
        "contrib_y_ageclass" = lcotg,
        "splitting" = csplit,
        "bonus_m_edu" = lbedu,
        "bonus_m_assist" = lbass,
        "capping" = cplaf
      ) |>
      mutate(
        contrib_m_ind = case_when(
          is.na(contrib_m_ind) ~ as.double(-0),
          TRUE ~ as.double(contrib_m_ind)
        ),
        contrib_y_ageclass = case_when(
          is.na(contrib_y_ageclass) ~ as.double(-0),
          TRUE ~ as.double(contrib_y_ageclass)
        ),
        splitting = case_when(
          is.na(splitting) ~ as.double(-0),
          TRUE ~ as.double(splitting)
        ),
        bonus_m_edu = case_when(
          is.na(bonus_m_edu) ~ as.double(-0),
          TRUE ~ as.double(bonus_m_edu)
        ),
        bonus_m_assist = case_when(
          is.na(bonus_m_assist) ~ as.double(-0),
          TRUE ~ as.double(bonus_m_assist)
        )
      )
  } else {
    IND_YEARLY_RR |>
      dplyr::rename(
        "aadr" = ram,
        "monthly_pension" = monatliche_rente,
        "age" = alt,
        "year" = jahr,
        "resid" = dom
      )
  }

  RR_OASI2 <- RR_OASI1 |>
    mutate(
      benef_type = dplyr::recode(gpr,
        "rvieillesse_simple" = 1, # "Old-age"
        "rveuve" = 2, # "Widow"
        "rorphelin_pere_simple" = 3, # "Father's orphan"
        "rorphelin_mere_simple" = 4, # "Mother's orphan"
        "rorphelin_double" = 5, # "Twice orphan",
        "rcompl_femme" = 6, # "Spouse's compl.",
        "renfant_pere_simple" = 7, # "Father's child rent"
        "renfant_mere_simple" = 8 # "Mother's child rent"
      ),
      sex = dplyr::recode(sex,
        "f" = 1, # "Woman"
        "m" = 0 # "Man"
      ),
      nat = dplyr::recode(nat,
        "au" = 1, # "Foreign",
        "ch" = 0 # "Swiss"
      ),
      resid = dplyr::recode(resid,
        "au" = 1, # "Foreign",
        "ch" = 0, # "Swiss"
      ),
      marital_stat = dplyr::recode(zv,
        "geschieden" = 1, # "Divorced",
        "ledig" = 2, # "Single",
        "verheiratet" = 3, # "Married",
        "verwitwet" = 4 # "Widowed"
      ),
      # Mutate the total years of contribution
      scale = round(eprc * 44, 0),
    ) |>
    mutate_if(is.character, as.factor) |>
    mutate(
      marital_stat1 = case_when(marital_stat == 1 ~ 1, TRUE ~ 0),
      marital_stat2 = case_when(marital_stat == 2 ~ 1, TRUE ~ 0),
      marital_stat3 = case_when(marital_stat == 3 ~ 1, TRUE ~ 0),
      marital_stat4 = case_when(marital_stat == 4 ~ 1, TRUE ~ 0),
      benef_type1 = case_when(benef_type == 1 ~ 1, TRUE ~ 0),
      benef_type2 = case_when(benef_type == 2 ~ 1, TRUE ~ 0),
      benef_type3 = case_when(benef_type == 3 ~ 1, TRUE ~ 0),
      benef_type4 = case_when(benef_type == 4 ~ 1, TRUE ~ 0),
      benef_type5 = case_when(benef_type == 5 ~ 1, TRUE ~ 0),
      benef_type6 = case_when(benef_type == 6 ~ 1, TRUE ~ 0),
      benef_type7 = case_when(benef_type == 7 ~ 1, TRUE ~ 0),
      benef_type8 = case_when(benef_type == 8 ~ 1, TRUE ~ 0),
      age_retire = case_when(
        is.na(age_ret) ~ as.double(-99999),
        TRUE ~ as.double(age_ret)
      ),
    ) |>
    select(
      -zv,
      -gpr
    )

  RR_OASI <- if ("napref" %in% names(RR_OASI2)) {
    RR_OASI2 |>
      select(
        -napref
      ) |>
      mutate_all(as.numeric)
  } else {
    RR_OASI2 |>
      mutate_all(as.numeric)
  }

  mod_return(
    RR_OASI
  )
}
