#' Check whether all inputs are tidy tibbles
#'
#' Check whether all inputs are tidy tibbles.
#'
#' @param mod.function Name of the module.
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @autoglobal
#' @return Invisible.
#' @export

# init and return functions for mod_ functions
mod_init <- function(mod.function = NULL) {
  fenv <- sys.frame(-1) # environment of the function from which it is called

  # optional character string; if NULL it is guessed from the surrounding
  # function call (beta)
  if (is.null(mod.function)) {
    mod.function <- as.character(sys.call(-1)[[1]])
  }

  # progress indicator
  if ("shiny" %in% rownames(installed.packages()) &&
    !is.null(shiny::getDefaultReactiveDomain())) {
    # if in shiny session
    incProgress(amount = 0.02, detail = mod.function)
  } else {
    message("Running: ", mod.function)
  }

  # not fully clear to me why get wants an env one level higher, but seems
  # to work this way
  argnames <- names(formals(get(mod.function, envir = sys.frame(-2))))

  if (!"list" %in% argnames) {
    stop(
      "function definition of ",
      mod.function,
      "does not have 'list' argument"
    )
  }

  argnames <- setdiff(argnames, "list")

  list <- get("list", envir = fenv)

  if (!is.null(list) && !inherits(list, "function")) {
    list <- tidylist_ensure(list)

    is.present <- argnames %in% names(list)
    if (!all(is.present)) {
      stop(
        "some args are not present in 'list': ",
        paste(argnames[!is.present], collapse = "\n")
      )
    }
    list <- list[argnames]
    for (name.i in names(list)) {
      assign(x = name.i, value = list[[name.i]], envir = fenv)
    }
  }

  not.found <- argnames[!argnames %in% ls(envir = fenv)]

  if (length(not.found) > 0) {
    stop("Cannot find data frames: ", paste(not.found, collapse = ", "))
  }

  z <- lapply(argnames, get, envir = fenv)
  names(z) <- argnames
  z <- tidylist_ensure(z)

  trace_this(z, at = "input", mod = mod.function)
  invisible(z)
}

#' mod_return() for the modules
#'
#' Checks whether all the ouptus are tibbles.
#'
#' @param ... any tidylist containing tibble or simply tibble.
#'
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#'
#' @export
mod_return <- function(...) {
  z <- list(...)
  if (inherits(z[[1]], "list")) {
    z <- z[[1]]
  } else {
    nnl <- as.list(match.call())[-1]
    nn <- vapply(nnl, as.character, "")
    names(z) <- nn
  }

  mod.function <- as.character(sys.call(-1)[[1]])

  if (mod.function != "_f") {
    argnames <- names(formals(get(mod.function, envir = sys.frame(-2))))
    if (!"list" %in% argnames) {
      stop(
        "function definition of ",
        mod.function,
        "does not have 'list' argument"
      )
    }
    argnames <- setdiff(argnames, "list")
  }

  z <- tidylist_ensure(z)

  trace_this(z, at = "output", mod = mod.function)
  z
}

#' Trace the inputs (\code{\link{mod_init}}) and the outputs
#'
#' Trace the inputs (\code{\link{mod_init}}) and the
#' outputs (\code{\link{mod_return}}).
#'
#' @param x Tidylist.
#' @param at öevel of the module ("input" or "output").
#' @param mod Name of the module function.
#' @author [Christoph Sax](mailto:christoph@cynkra.com)
#' @autoglobal
#' @export
trace_this <- function(x, at = "", mod = "") {
  trace.env <- getOption("trace.env", NULL)
  if (!is.null(trace.env)) {
    x <- tidylist_ensure(x)
    tr <- lapply(
      x,
      function(e) {
        data.frame(
          var = names(e),
          stringsAsFactors = FALSE
        )
      }
    )

    if (length(tr) == 0) {
      stop("do not use `mod_init()` in functions without arguments.")
    }

    df <- bind_rows(tr, .id = "name") |>
      rename(df = name) |>
      mutate(at = at) |>
      mutate(mod = mod)

    df$nframe <- sys.nframe()

    if (!exists("TRACE_DF", envir = trace.env)) {
      TRACE_DF <- df
    } else {
      TRACE_DF <- rbind(get("TRACE_DF", envir = trace.env), df)
    }
    assign("TRACE_DF", TRACE_DF, envir = trace.env)
  }
}
