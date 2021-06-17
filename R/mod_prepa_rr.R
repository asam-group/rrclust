#' @title Preparation of the register of rents data.
#'
#' @description Prepares the variables of the register of rents.
#'
#' @param IND_YEARLY_RR a data frame containing the data of the register of rents
#' subsetted for one year only.
#'
#' @param list List of input data frames.
#'
#' @return a `tidylist` containing the following tidy data frames:
#'   - `RR_OASI` : contains all the beneficiaries of the OASI.
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#'
#' @export

# - `Last change`: 2021-06-17 / Llc

mod_prepa_rr <- function(IND_YEARLY_RR,
                         list = NULL) {
  mod_init()

  # --- Recoding and renaming the variables ------------------------------------
  RR_OASI <- IND_YEARLY_RR %>%
    dplyr::rename(
      "aadr" = ram,
      "monthly_rent" = monatliche_rente,
      "age" = alt,
      "year" = jahr,
      "resid" = dom
    ) %>%
    # Rename the realizations of the variables
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
    ) %>%
    # Transform character variables to factors
    mutate_if(sapply(., is.character), as.factor) %>%
    mutate(
      # Dummy variables for each marital status
      marital_stat1 = case_when(marital_stat == 1 ~ 1, TRUE ~ 0),
      marital_stat2 = case_when(marital_stat == 2 ~ 1, TRUE ~ 0),
      marital_stat3 = case_when(marital_stat == 3 ~ 1, TRUE ~ 0),
      marital_stat4 = case_when(marital_stat == 4 ~ 1, TRUE ~ 0),

      # Dummy variables for each benefit type
      benef_type1 = case_when(benef_type == 1 ~ 1, TRUE ~ 0),
      benef_type2 = case_when(benef_type == 2 ~ 1, TRUE ~ 0),
      benef_type3 = case_when(benef_type == 3 ~ 1, TRUE ~ 0),
      benef_type4 = case_when(benef_type == 4 ~ 1, TRUE ~ 0),
      benef_type5 = case_when(benef_type == 5 ~ 1, TRUE ~ 0),
      benef_type6 = case_when(benef_type == 6 ~ 1, TRUE ~ 0),
      benef_type7 = case_when(benef_type == 7 ~ 1, TRUE ~ 0),
      benef_type8 = case_when(benef_type == 8 ~ 1, TRUE ~ 0),

      # Recode age_ret for NA values
      age_retire = case_when(
        is.na(age_ret) ~ as.double(-99999),
        TRUE ~ as.double(age_ret)
      ),
    ) %>%
    dplyr::select(
      -zv,
      -gpr,
      -napref
    )


  mod_return(
    RR_OASI
  )
}
