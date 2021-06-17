# Libraries
library(codetools)
library(delfin)
library(stringr)
library(openxlsx)

# Working Directory
wd <- "O:/MASS/06_auftraege/01_bsv/11_delfins/02_delfin/04_documentation/doc_structure"
# wd <- "C:/delfin"

# Delfin version
version_delfin <- getNamespaceVersion("delfin")

# Filename
filename <- paste0(
  "delfin_",
  paste0(
    "vrs",
    gsub("\\.", "_", version_delfin)
  ),
  ".xlsx"
)

# Function to retrieve functions from delfin
gen_fun <- function(elmnt) {

  # Get the name of the retrieved function
  if (class(elmnt) == "function") {
    x <- deparse(substitute(elmnt))
  } else {
    x <- elmnt
  }

  # Exported objects from delfin
  list_exp_delf <- list(getNamespaceExports("delfin"))

  if (!is.na(x == "NA")) {
    if (x != ".browser_at_settings" &
      x != "scenario_reforme" &
      x != "scenario" &
      x != "rente_ram_knick_go" &
      x != "delta_eomax" &
      x != "trace_this" &
      x != "tidylist_ensure" &
      x != "justierung_ls" &
      x != "write_sheet_eo_go" &  #à revoir
      x != "write_sheet_eo_round" &  #à revoir
      x != "mod_init" &
      x != "mod_return") {
      y1 <- findGlobals(x)
      y <- y1[!grepl(x, y1)]

      # Create function to detect match for each str
      any_match <- function(str) {
        any(sapply(list_exp_delf, function(x) {
          str_detect(str, x)
        }))
      }


      # sapply each element in string list to any_match func
      z <- y[sapply(y, any_match)]


      # Return dpc
      dpc <- z
    } else {

      # Return dpc with NA if conditions are not fulfilled
      dpc <- NA_character_
    }
  } else {

    # Return dpc with NA if conditions are not fulfilled
    dpc <- NA_character_
  }
}

# exported functions from delfin
exp_delf <- getNamespaceExports("delfin")

# mod_opt functions
vect_mod_opt1 <- c(exp_delf[grepl("^mod_opt", exp_delf)])
vect_mod_opt <- vect_mod_opt1[!grepl("eo", vect_mod_opt1)] %>%
  sort()
# For EO
vect_mod_opt_eo <- vect_mod_opt1[grepl("eo", vect_mod_opt1)] %>%
  sort()

# wraps functions
vect_wraps1 <- c(
  exp_delf[grepl("^wrap", exp_delf)],
  exp_delf[grepl("^prepare", exp_delf)]
) %>%
  sort()

excpt_wraps <- c(
  vect_wraps1[grepl(c("eo"), vect_wraps1)],
  vect_wraps1[grepl(c("massnahmen"), vect_wraps1)],
  vect_wraps1[grepl(c("caspop"), vect_wraps1)]
) %>%
  sort()

# Create function to detect match for each str
any_match_neg <- function(str) {
  any(sapply(excpt_wraps, function(x) {
    str_detect(str, x, negate = FALSE)
  }))
}

# sapply each element in string list to any_match func
VECT_WRAPS <- tibble(vect = vect_wraps1[!sapply(vect_wraps1, any_match_neg)]) %>%
  arrange(vect) %>%
  # take only the not memoised (with "_")
  filter(
    vect != "wrap_ahv",
    vect != "wrap_beitragstab",
    vect != "wrap_rententab",
    vect != "wrap_rententab_dcai"
  )

# For AHV
vect_wraps_ahv <- (VECT_WRAPS %>%
  filter(
    !grepl("beitragstab", vect),
    !grepl("rententab", vect)
  ) %>%
  select(vect))$vect

# For Rententab
vect_wraps_rententab <- (VECT_WRAPS %>%
  filter(grepl("rententab", vect)) %>%
  select(vect))$vect

# For Beitragstab
vect_wraps_beitragstab <- (VECT_WRAPS %>%
  filter(grepl("beitragstab", vect)) %>%
  select(vect))$vect

# For EO
vect_wraps_eo1 <- vect_wraps1[grepl(c("eo"), vect_wraps1)] %>%
  sort()
vect_wraps_eo <- vect_wraps_eo1[!grepl(c("massnahmen"), vect_wraps_eo1)] %>%
  sort()

#--- AHV -----------------------------------------------------------------------
# AHV Dependencies table
DPC_AHV <- tibble(
  fun_name = c(
    "run_ahv",
    vect_wraps_ahv
  )
) %>%
  rowwise() %>%
  mutate(level0 = lapply(fun_name, gen_fun)) %>%
  unnest(cols = c(level0)) %>%
  rowwise() %>%
  mutate(level1 = lapply(level0, gen_fun)) %>%
  unnest(cols = c(level1)) %>%
  rowwise() %>%
  mutate(level2 = lapply(level1, gen_fun)) %>%
  unnest(cols = c(level2)) %>%
  rowwise() %>%
  mutate(level3 = lapply(level2, gen_fun)) %>%
  unnest(cols = c(level3)) %>%
  rowwise() %>%
  mutate(level4 = lapply(level3, gen_fun)) %>%
  unnest(cols = c(level4))


# AHV_MASSN Dependencies table
DPC_AHV_MASSN <- tibble(
  fun_name = c(
    "wrap_ahv_massnahmen",
    vect_mod_opt
  )
) %>%
  rowwise() %>%
  mutate(level0 = lapply(fun_name, gen_fun)) %>%
  unnest(cols = c(level0)) %>%
  rowwise() %>%
  mutate(level1 = lapply(level0, gen_fun)) %>%
  unnest(cols = c(level1)) %>%
  rowwise() %>%
  mutate(level2 = lapply(level1, gen_fun)) %>%
  unnest(cols = c(level2)) %>%
  rowwise() %>%
  mutate(level3 = lapply(level2, gen_fun)) %>%
  unnest(cols = c(level3)) %>%
  rowwise() %>%
  mutate(level4 = lapply(level3, gen_fun)) %>%
  unnest(cols = c(level4))

#--- RENTENTAB -----------------------------------------------------------------
# RENTENTAB Dependencies table
DPC_RENTENTAB <- tibble(
  fun_name = c(
    "run_rententab",
    vect_wraps_rententab
  )
) %>%
  rowwise() %>%
  mutate(level0 = lapply(fun_name, gen_fun)) %>%
  unnest(cols = c(level0)) %>%
  rowwise() %>%
  mutate(level1 = lapply(level0, gen_fun)) %>%
  unnest(cols = c(level1)) %>%
  rowwise() %>%
  mutate(level2 = lapply(level1, gen_fun)) %>%
  unnest(cols = c(level2)) %>%
  rowwise() %>%
  mutate(level3 = lapply(level2, gen_fun)) %>%
  unnest(cols = c(level3)) %>%
  rowwise() %>%
  mutate(level4 = lapply(level3, gen_fun)) %>%
  unnest(cols = c(level4))

#--- BEITRAGSTAB -----------------------------------------------------------------
# BEITRAGSTAB Dependencies table
DPC_BEITRAGSTAB <- tibble(
  fun_name = c(
    "run_beitragstab",
    vect_wraps_rententab
  )
) %>%
  rowwise() %>%
  mutate(level0 = lapply(fun_name, gen_fun)) %>%
  unnest(cols = c(level0)) %>%
  rowwise() %>%
  mutate(level1 = lapply(level0, gen_fun)) %>%
  unnest(cols = c(level1)) %>%
  rowwise() %>%
  mutate(level2 = lapply(level1, gen_fun)) %>%
  unnest(cols = c(level2)) %>%
  rowwise() %>%
  mutate(level3 = lapply(level2, gen_fun)) %>%
  unnest(cols = c(level3)) %>%
  rowwise() %>%
  mutate(level4 = lapply(level3, gen_fun)) %>%
  unnest(cols = c(level4))

#--- EO -----------------------------------------------------------------------
# EO Dependencies table
DPC_EO <- tibble(
  fun_name = c(
    "run_eo",
    vect_wraps_eo
  )
) %>%
  rowwise() %>%
  mutate(level0 = lapply(fun_name, gen_fun)) %>%
  unnest(cols = c(level0)) %>%
  rowwise() %>%
  mutate(level1 = lapply(level0, gen_fun)) %>%
  unnest(cols = c(level1)) %>%
  rowwise() %>%
  mutate(level2 = lapply(level1, gen_fun)) %>%
  unnest(cols = c(level2)) %>%
  rowwise() %>%
  mutate(level3 = lapply(level2, gen_fun)) %>%
  unnest(cols = c(level3)) %>%
  rowwise() %>%
  mutate(level4 = lapply(level3, gen_fun)) %>%
  unnest(cols = c(level4))


# EO_MASSN Dependencies table
DPC_EO_MASSN <- tibble(
  fun_name = c(
    "wrap_eo_massnahmen",
    vect_mod_opt_eo
  )
) %>%
  rowwise() %>%
  mutate(level0 = lapply(fun_name, gen_fun)) %>%
  unnest(cols = c(level0)) %>%
  rowwise() %>%
  mutate(level1 = lapply(level0, gen_fun)) %>%
  unnest(cols = c(level1)) %>%
  rowwise() %>%
  mutate(level2 = lapply(level1, gen_fun)) %>%
  unnest(cols = c(level2)) %>%
  rowwise() %>%
  mutate(level3 = lapply(level2, gen_fun)) %>%
  unnest(cols = c(level3)) %>%
  rowwise() %>%
  mutate(level4 = lapply(level3, gen_fun)) %>%
  unnest(cols = c(level4))

#--- Tidylist ------------------------------------------------------------------
tl_dpc <- tidylist(
  DPC_AHV, DPC_AHV_MASSN,
  DPC_RENTENTAB,
  DPC_BEITRAGSTAB,
  DPC_EO, DPC_EO_MASSN
)

#--- Output --------------------------------------------------------------------
# Create Workbook object and add worksheets
wb <- createWorkbook()

# Add worksheets
addWorksheet(wb, "DPC_AHV")
addWorksheet(wb, "DPC_AHV_MASSN")
addWorksheet(wb, "DPC_RENTENTAB")
addWorksheet(wb, "DPC_BEITRAGSTAB")
addWorksheet(wb, "DPC_EO")
addWorksheet(wb, "DPC_EO_MASSN")

# Write Data in each worksheet
writeData(wb, "DPC_AHV", DPC_AHV, startCol = 2, startRow = 3, rowNames = TRUE)
writeData(wb, "DPC_AHV_MASSN", DPC_AHV_MASSN, startCol = 2, startRow = 3, rowNames = TRUE)
writeData(wb, "DPC_EO", DPC_EO, startCol = 2, startRow = 3, rowNames = TRUE)
writeData(wb, "DPC_EO_MASSN", DPC_EO_MASSN, startCol = 2, startRow = 3, rowNames = TRUE)
writeData(wb, "DPC_RENTENTAB", DPC_RENTENTAB, startCol = 2, startRow = 3, rowNames = TRUE)
writeData(wb, "DPC_BEITRAGSTAB", DPC_BEITRAGSTAB, startCol = 2, startRow = 3, rowNames = TRUE)

# save workbook
# openXL(wb)
saveWorkbook(
  wb,
  file.path(wd, filename)
)

browseURL(wd)
