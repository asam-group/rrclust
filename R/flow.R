#' Trace and visualize workflow
#'
#' 
#' Trace and visualize workflow.
#' @param expr expression for which the flow should be traced, e.g.
#' `{run_kamila(path = paths, path_out = path_out)}`.
#' @author [Christoph Sax](mailto:christoph@christophsax.com)
#' @export
trace_flow <- function(expr) {
  withr::with_options(list(trace.env = new.env()), {
    assign("TRACE_DF", NULL, envir = getOption("trace.env", NULL))

    eval(expr = expr, envir = sys.frame(-1))

    TRACE_DF <- get("TRACE_DF", envir = getOption("trace.env", NULL))

    as.data.frame(TRACE_DF)
  })
}

#' Draw data flow.
#'
#' Draw data flow.
#'
#' @param DF Data frame to describe in the data flow.
#' @author [Christoph Sax](mailto:christoph@christophsax.com)
#' @autoglobal
#' @export
draw_flow <- function(DF) {
  if (!rlang::is_installed("DiagrammeR")) {
    stop("The package {DiagrammerR} is required for this action")
  }

  if (nrow(DF) == 0) stop("DF has no rows.")

  # sufficient, as long as we don't use var names
  DF <- unique(mutate(DF, var = NULL))

  if (length(unique(DF$nframe)) > 1) {
    top.level <- min(DF$nframe)
    TOP_LEVEL <- filter(DF, nframe == top.level)
    DF0 <- filter(DF, nframe > top.level)
  } else {
    TOP_LEVEL <- DF
    DF0 <- DF
  }

  ndf0 <- rbind(
    data.frame(
      nodes = unique(DF0$mod),
      class = "mod",
      stringsAsFactors = FALSE
    ),
    data.frame(
      nodes = unique(DF0$df),
      class = "df",
      stringsAsFactors = FALSE
    )
  )

  ndf0$shape <- "box"
  ndf0$shape[ndf0$class == "mod"] <- "ellipse"

  top.level.output <- filter(TOP_LEVEL, at == "output")$df
  top.level.input <- filter(TOP_LEVEL, at == "input")$df

  ndf0$fillcolor <- "red"
  ndf0$fillcolor[ndf0$nodes %in% top.level.input] <- "LightGreen"
  ndf0$fillcolor[ndf0$nodes %in% top.level.output] <- "blue"

  ndf0$fillcolor[ndf0$class == "mod"] <- "DarkGreen"
  ndf0$style <- "filled"

  ndf <- create_node_df(
    n = nrow(ndf0),
    type = "type_1",
    label = ndf0$nodes,
    shape = ndf0$shape,
    fillcolor = ndf0$fillcolor,
    style = "filled"
  )

  DF0_OUT <- filter(DF0, at == "output")
  DF0_IN <- filter(DF0, at == "input")

  edf0 <- rbind(
    data.frame(from = DF0_IN$df, to = DF0_IN$mod, stringsAsFactors = FALSE),
    data.frame(from = DF0_OUT$mod, to = DF0_OUT$df, stringsAsFactors = FALSE)
  )

  edf <- create_edge_df(
    from = match(edf0$from, ndf$label),
    to = match(edf0$to, ndf$label)
  )

  create_graph(
    nodes_df = ndf,
    edges_df = edf,
    directed = TRUE,
    attr_theme = NULL
  ) |>
    add_global_graph_attrs(
      attr = "overlap",
      value = "true",
      attr_type = "node"
    ) |>
    render_graph()
}
