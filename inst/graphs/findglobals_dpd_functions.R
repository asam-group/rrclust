# Libraries
library(codetools)
library(rrclust)
library(stringr)
library(openxlsx)

# Working Directory
wd <- "/Users/Layal/OFAS/doctorat/package_tools/container_tools/outputs/graphs/2021_06_10_5_clusters"

# rrclust version
version_rrclust <- getNamespaceVersion("rrclust")

# Filename
filename <- paste0(
  "rrclust_",
  paste0(
    "vrs",
    gsub("\\.", "_", version_rrclust)
  ),
  ".xlsx"
)

# Function to retrieve functions from rrclust
gen_fun <- function(elmnt) {

  # Get the name of the retrieved function
  if (class(elmnt) == "function") {
    x <- deparse(substitute(elmnt))
  } else {
    x <- elmnt
  }

  # Exported objects from rrclust
  list_exp_rrcl <- list(getNamespaceExports("rrclust"))

  if (!is.na(x == "NA")) {
    if (x != ".browser_at_settings" &
      x != "scenario_reforme" &
      x != "scenario" &
      x != "rente_ram_knick_go" &
      x != "delta_eomax" &
      x != "trace_this" &
      x != "tidylist_ensure" &
      x != "justierung_ls" &
      x != "write_sheet_eo_go" & # à revoir
      x != "write_sheet_eo_round" & # à revoir
      x != "mod_init" &
      x != "mod_return") {
      y1 <- findGlobals(x)
      y <- y1[!grepl(x, y1)]

      # Create function to detect match for each str
      any_match <- function(str) {
        any(sapply(list_exp_rrcl, function(x) {
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

# exported functions from rrclust
exp_rrcl <- getNamespaceExports("rrclust")


# wraps functions
vect_wraps1 <- c(
  exp_rrcl[grepl("^wrap", exp_rrcl)],
  exp_rrcl[grepl("^prepare", exp_rrcl)]
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
    vect != "wrap_kamila",
    vect != "wrap_computation_kamila",
    vect != "wrap_prepadata"
  )

# For Kamila
vect_wraps_kamila <- (VECT_WRAPS %>%
  dplyr::select(vect))$vect



#--- KAMILA -----------------------------------------------------------------------
# KAMILA Dependencies table
DPC_KAMILA <- tibble(
  fun_name = c(
    "run_kamila",
    vect_wraps_kamila
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
  DPC_KAMILA
)

#--- Output --------------------------------------------------------------------
# Create Workbook object and add worksheets
wb <- createWorkbook()

# Add worksheets
addWorksheet(wb, "DPC_KAMILA")

# Write Data in each worksheet
writeData(wb, "DPC_KAMILA", DPC_KAMILA, startCol = 2, startRow = 3, rowNames = TRUE)

# save workbook
# openXL(wb)
saveWorkbook(
  wb,
  file.path(wd, filename)
)

#Export to txt file to write it into latex
library(readxl)
excel_rrclust <- read_excel(file.path(wd, filename))
write.table(excel_rrclust,
            file = paste0(file.path(wd, filename), ".txt"),
            sep = " & ", row.names = FALSE, quote = FALSE)
browseURL(wd)
