#' Read and write the inputs for the calculation in a tidylist
#'
#' Read and write the inputs for the calculation in a tidylist.
#'
#' @param path Path to container.
#' @param path_out Path to output container.
#' @param overwrite if TRUE, the output should be overwritten.
#'
#' @return Side-effect. Write csv files into the intput directory.
#' @author [Layal Christine Lettry](mailto:layal.lettry@gmail.com)
#' @export
prepare_input <- function(path,
                          path_out = file.path(dirname(path)),
                          overwrite = FALSE) {
  PARAM_INPUTS <- read_param(path)
  inp_path_all <- file.path(path_out, "all")
  inp_path_kamila <- file.path(path_out, "kamila")

  ensure_path <- function(path) {
    # do not allow overwriting if overwrite == FALSE
    if (!overwrite && file.exists(path)) stop(path, "already exists")
    if (!file.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
    file.remove(list.files(path, full.names = TRUE))
  }

  ensure_path(inp_path_all)
  ensure_path(inp_path_kamila)

  IND_YEARLY_RR <- mod_input_ind_yearly_rr(
    PARAM_INPUTS = PARAM_INPUTS
  )$IND_YEARLY_RR

  all_inputs <- tidylist(
    IND_YEARLY_RR
  )
  tidylist_write(all_inputs, inp_path_all)

  kamila_inputs <- tidylist()

  tidylist_write(kamila_inputs, inp_path_kamila)
}
