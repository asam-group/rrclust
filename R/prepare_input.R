#' @title Preparation of the inputs for the calculation
#'
#' @description Reading, preparing and saving the inputs.
#'
#' @param path path to container
#'
#' @return nothing. Output (the input data) is stored in the assigned location
#'
#' @author [Layal Christine Lettry](mailto:layalchristine.lettry@unifr.ch)
#'
#' @export

# Last change: 2021-03-03 / Llc


prepare_input <- function(path,
                          path_out = file.path(dirname(path)),
                          overwrite = FALSE) {

  # Parameters -----------------------------------------------------------------

  PARAM_INPUTS <- read_param(path)

  # Paths ----------------------------------------------------------------------

  inp_path_kamila <- file.path(path_out, "kamila")

  ensure_path <- function(path) {
    # do not allow overwriting if overwrite == FALSE
    if (!overwrite && file.exists(path)) stop(path, "already exists")
    if (!file.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
    file.remove(list.files(path, full.names = TRUE))
  }

  ensure_path(inp_path_kamila)

  # Input Data -----------------------------------------------------------------

  # Input register of rents

  IND_YEARLY_RR <- mod_input_ind_yearly_rr(
    PARAM_INPUTS = PARAM_INPUTS
  )$IND_YEARLY_RR

  # Input ideal number of clusters gstar

  KM_RES <- mod_input_kmres(
    PARAM_INPUTS = PARAM_INPUTS
  )$KM_RES

  ## Collect and store inputs --------------------------------------------------

  # Geltende Ordnung AHV
  go <- tidylist(
    AHV_ABRECHNUNG_DEF,
    AHV_ABRECHNUNG_PROV,
    IV_ABRECHNUNG_DEF,
    IV_ABRECHNUNG_PROV,
    EO_ABRECHNUNG_DEF,
    EO_ABRECHNUNG_PROV,
    ASSURES_FACULTATIFS,
    BEV_POP,
    POP_SCENARIO_BEV,
    POP_SCENARIO_EPT,
    ECKWERTE,
    EMIGRATION_POP,
    EMIGRATION_SCENARIO_ALT,
    EMIGRATION_SCENARIO_NEU,
    ESTV,
    FRONTALIERS_OBS,
    FRONTALIERS_SCEN,
    ESPVIE,
    IK,
    INDICES,
    MINIMALRENTE,
    MORTALITE,
    SAISONNIERS,
    ANT_AJO_FLEX,
    IND_PRODUCTIVITE,
    IV_SCHULD,
    RR_AVS,
    ZINS_SCEN,
    UEBR_EINN_SCEN,
    NOMBRE_RENTIERS
  )


  tidylist_write(go, inp_path_go)

  # Massnahmen AHV 21

  massnahmen <- tidylist(
    AHV_AV2020_MASSN,
    RENTE_MAX65,
    CI_MAX65,
    RR_HAUTSREV_HF,
    RR_BASMOYREV_HF,
    RR_RENTENPOLYGON,
    BENEF_BNS,
    ERTRAG_ANLAGEN_HIST,
    FAKTOREN_GS,
    FAKTOR_PLAF,
    RED_IV_SCHULD
  )

  tidylist_write(massnahmen, inp_path_massnahmen_ahv)

  # Rententabellen

  rententab <- tidylist(
    RENTES_MIN_SKALA,
    LOHNINDEX,
    CS,
    AUFWFAKT_FULL,
    QUOTITE_RENTE_G
  )

  tidylist_write(rententab, inp_path_rententab)

  # Beitragstabellen

  beitragstab <- tidylist(
    HIST_BAREME_DEGRESSIF
  )



  tidylist_write(beitragstab, inp_path_beitragstab)

  # CAS Population Llc

  go <- c(
    go,
    tidylist(
      PROB_ETAT_CIVIL
    )
  )

  tidylist_write(go, inp_path_caspop_llc)
  tidylist_write(massnahmen, inp_path_massnahmen_caspop_llc)
}
