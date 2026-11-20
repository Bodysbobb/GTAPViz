# GTAPViz MCP HELPERS ==========================================================
# Conversion of JSON arguments to R values, dataset references, validation with
# helpful error messages, output-file tracking and small text formatters.

# JSON -> R VALUES -------------------------------------------------------------

#' JSON scalar or array to an atomic vector (integers become doubles so the
#' recorded code reads `20`, not `20L`).
#' @keywords internal
#' @noRd
.mcp_vec <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x)) {
    if (!length(x)) return(character())
    x <- unlist(x, use.names = FALSE)
  }
  if (is.integer(x)) x <- as.numeric(x)
  x
}

#' @keywords internal
#' @noRd
.mcp_chr <- function(x) {
  v <- .mcp_vec(x)
  if (is.null(v)) NULL else as.character(v)
}

#' Generic JSON value to R: objects become named lists, arrays of scalars
#' become vectors.
#' @keywords internal
#' @noRd
.mcp_value <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x)) {
    nms <- names(x)
    if (!is.null(nms) && all(nzchar(nms))) return(lapply(x, .mcp_value))
    if (length(x) && all(vapply(x, function(e) is.atomic(e) && length(e) == 1L, logical(1)))) {
      return(.mcp_vec(x))
    }
    return(lapply(x, .mcp_value))
  }
  if (is.integer(x)) as.numeric(x) else x
}

#' `filter_var`: object {column: [values]} -> named list; array -> vector
#' (filters the Variable column).
#' @keywords internal
#' @noRd
.mcp_filter_value <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x) && length(x) && !is.null(names(x)) && all(nzchar(names(x)))) {
    return(lapply(x, .mcp_vec))
  }
  v <- .mcp_vec(x)
  if (!length(v)) NULL else v
}

#' `split_by`: [] or false -> FALSE; "none" -> FALSE; string(s) -> character.
#' @keywords internal
#' @noRd
.mcp_split_value <- function(x) {
  if (is.null(x)) return(NULL)
  if (isFALSE(x)) return(FALSE)
  if (is.list(x) && !length(x)) return(FALSE)
  v <- .mcp_chr(x)
  if (!length(v) || all(tolower(v) %in% c("", "none", "false", "no"))) return(FALSE)
  v
}

#' Optional column name where "" / "none" mean NULL.
#' @keywords internal
#' @noRd
.mcp_nullable_string <- function(x) {
  v <- .mcp_chr(x)
  if (!length(v) || tolower(v[1]) %in% c("", "none", "null", "false")) return(NULL)
  v[1]
}

#' Convert one argument according to its declared type.
#' @keywords internal
#' @noRd
.mcp_convert <- function(x, type) {
  switch(
    type,
    string = { v <- .mcp_chr(x); if (length(v)) v[1] else NULL },
    strings = .mcp_chr(x),
    number = { v <- .mcp_vec(x); if (length(v)) as.numeric(v[1]) else NULL },
    numbers = { v <- .mcp_vec(x); if (length(v)) as.numeric(v) else NULL },
    integer = { v <- .mcp_vec(x); if (length(v)) as.numeric(round(as.numeric(v[1]))) else NULL },
    boolean = { v <- .mcp_vec(x); if (length(v)) isTRUE(as.logical(v[1])) else NULL },
    filter = .mcp_filter_value(x),
    split = .mcp_split_value(x),
    nullable_string = .mcp_nullable_string(x),
    pdf = .mcp_pdf_value(x),
    value = .mcp_value(x),
    stop("Unknown argument type: ", type)
  )
}

#' `export_as_pdf`: "no"/"yes"/"merged" (or booleans).
#' @keywords internal
#' @noRd
.mcp_pdf_value <- function(x) {
  v <- .mcp_vec(x)
  if (!length(v)) return(FALSE)
  if (is.logical(v)) return(isTRUE(v[1]))
  v <- tolower(as.character(v[1]))
  if (v %in% c("merged", "merge")) return("merged")
  v %in% c("yes", "true", "separate", "pdf")
}

#' Arguments present in the JSON input, converted by `spec` (name -> type).
#' Explicit nulls are kept as NULL arguments.
#' @keywords internal
#' @noRd
.mcp_build_args <- function(args, spec) {
  out <- list()
  for (nm in names(spec)) {
    if (!nm %in% names(args)) next
    out[nm] <- list(.mcp_convert(args[[nm]], spec[[nm]]))
  }
  out
}

#' @keywords internal
#' @noRd
.mcp_arg <- function(args, name, type = "string", default = NULL) {
  if (!name %in% names(args) || is.null(args[[name]])) return(default)
  value <- .mcp_convert(args[[name]], type)
  if (is.null(value)) default else value
}

#' Default of a GTAPViz function argument (for constant defaults only).
#' @keywords internal
#' @noRd
.mcp_formal_default <- function(fun, arg) {
  f <- formals(get(fun, envir = asNamespace("GTAPViz")))
  if (!arg %in% names(f)) return(NULL)
  tryCatch(eval(f[[arg]], envir = baseenv()), error = function(e) NULL)
}

# DATASET REFERENCES ---------------------------------------------------------------

#' All plot-ready tables in the workspace (data frames with a Value column).
#' @keywords internal
#' @noRd
.mcp_dataset_index <- function(srv) {
  out <- list()
  for (obj in sort(ls(srv$ws))) {
    x <- get(obj, envir = srv$ws)
    if (is.data.frame(x)) {
      if ("Value" %in% names(x)) {
        out[[length(out) + 1L]] <- list(ref = obj, object = obj, table = NULL, df = x)
      }
    } else if (is.list(x) && length(x) && !is.null(names(x))) {
      for (key in names(x)) {
        if (is.data.frame(x[[key]]) && "Value" %in% names(x[[key]])) {
          out[[length(out) + 1L]] <- list(
            ref = sprintf('%s[["%s"]]', obj, key), object = obj, table = key, df = x[[key]]
          )
        }
      }
    }
  }
  out
}

#' @keywords internal
#' @noRd
.mcp_no_data_error <- function() {
  stop("No GTAP data is loaded yet. Call load_gtap_results first ",
       "(use folder = \"example\" for the demo data).", call. = FALSE)
}

#' Resolve a dataset reference such as sl4.plot.data[["REG"]],
#' sl4.plot.data$REG, sl4.plot.data/REG or sl4.plot.data.
#' @keywords internal
#' @noRd
.mcp_ref <- function(srv, ref, want = c("table", "any")) {
  want <- match.arg(want)
  index <- .mcp_dataset_index(srv)
  if (!length(index)) .mcp_no_data_error()
  examples <- paste(utils::head(vapply(index, `[[`, "", "ref"), 6), collapse = ", ")

  ref <- .mcp_chr(ref)
  if (!length(ref) || !nzchar(ref[1])) {
    stop("`data` must name a dataset, e.g. ", examples, ". Call describe_data to list all.", call. = FALSE)
  }
  ref <- trimws(ref[1])
  pattern <- "^`?([A-Za-z.][A-Za-z0-9._]*)`?\\s*(?:\\[\\[\\s*[\"']([^\"']+)[\"']\\s*\\]\\]|\\$`?([^`]+)`?|/(.+))?$"
  m <- regmatches(ref, regexec(pattern, ref, perl = TRUE))[[1]]
  if (!length(m)) {
    stop("Cannot read dataset reference '", ref, "'. Use e.g. ", examples, call. = FALSE)
  }
  obj <- m[2]
  key <- m[3:5]
  key <- key[!is.na(key) & nzchar(key)]

  if (!exists(obj, envir = srv$ws, inherits = FALSE)) {
    stop("No dataset named '", obj, "'. Available: ", examples,
         ". Call describe_data to list all.", call. = FALSE)
  }
  x <- get(obj, envir = srv$ws)

  if (length(key)) {
    if (!is.list(x) || is.data.frame(x) || !key[1] %in% names(x)) {
      tables <- if (is.list(x) && !is.data.frame(x)) paste(names(x), collapse = ", ") else "(none)"
      stop("'", obj, "' has no table '", key[1], "'. Tables in ", obj, ": ", tables, call. = FALSE)
    }
    res <- list(expr = call("[[", as.name(obj), key[1]), value = x[[key[1]]],
                object = obj, table = key[1])
  } else {
    res <- list(expr = as.name(obj), value = x, object = obj, table = NULL)
  }

  if (want == "table" && !is.data.frame(res$value)) {
    if (is.list(res$value) && length(res$value) == 1L && is.data.frame(res$value[[1]])) {
      key <- names(res$value)[1]
      res <- list(expr = call("[[", as.name(obj), key), value = res$value[[1]], object = obj, table = key)
    } else {
      keys <- names(res$value)[vapply(res$value, is.data.frame, logical(1))]
      stop("'", obj, "' is a list of tables. Pick one, e.g. ",
           paste(sprintf('%s[["%s"]]', obj, utils::head(keys, 5)), collapse = ", "), call. = FALSE)
    }
  }
  res$label <- paste(deparse(res$expr), collapse = "")
  res
}

#' Datasets to modify (rename/sort/convert). Defaults to every loaded dataset
#' object, optionally only those containing `column`.
#' @keywords internal
#' @noRd
.mcp_targets <- function(srv, refs = NULL, column = NULL) {
  refs <- .mcp_chr(refs)
  if (length(refs)) return(lapply(refs, function(r) .mcp_ref(srv, r, want = "any")))
  index <- .mcp_dataset_index(srv)
  if (!length(index)) .mcp_no_data_error()
  if (!is.null(column)) {
    index <- Filter(function(d) column %in% names(d$df), index)
    if (!length(index)) {
      stop("No loaded dataset has a column '", column, "'.", call. = FALSE)
    }
  }
  objs <- unique(vapply(index, `[[`, "", "object"))
  lapply(objs, function(o) .mcp_ref(srv, o, want = "any"))
}

#' All data frames contained in a resolved reference.
#' @keywords internal
#' @noRd
.mcp_frames <- function(value) {
  if (is.data.frame(value)) return(list(value))
  if (is.list(value)) return(Filter(is.data.frame, value))
  list()
}

# VALIDATION ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
.mcp_values_text <- function(values, n = 40L) {
  values <- unique(as.character(values))
  shown <- utils::head(values, n)
  more <- length(values) - length(shown)
  paste0(paste(shown, collapse = ", "), if (more > 0) sprintf(" ... (+%d more)", more) else "")
}

#' Check that columns named in arguments exist in the data.
#' @keywords internal
#' @noRd
.mcp_check_columns <- function(df, call_args, args_to_check) {
  for (a in intersect(args_to_check, names(call_args))) {
    cols <- call_args[[a]]
    if (is.null(cols) || isFALSE(cols) || !is.character(cols)) next
    missing <- setdiff(cols, names(df))
    if (length(missing)) {
      stop(sprintf("`%s`: column %s not found. Available columns: %s",
                   a, paste0("'", missing, "'", collapse = ", "),
                   paste(names(df), collapse = ", ")), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Apply `filter_var` like the plotting functions do, with clear errors when
#' values do not exist (including case hints).
#' @keywords internal
#' @noRd
.mcp_apply_filter <- function(df, filter_var, variable_col = "Variable") {
  if (is.null(filter_var)) return(df)
  conditions <- if (is.list(filter_var)) filter_var else stats::setNames(list(filter_var), variable_col)
  for (col in names(conditions)) {
    if (!col %in% names(df)) {
      stop(sprintf("filter_var: column '%s' not found. Available columns: %s",
                   col, paste(names(df), collapse = ", ")), call. = FALSE)
    }
    wanted <- as.character(conditions[[col]])
    present <- unique(as.character(df[[col]]))
    missing <- setdiff(wanted, present)
    if (length(missing)) {
      hints <- present[tolower(present) %in% tolower(missing)]
      hint <- if (length(hints)) sprintf(" Did you mean: %s?", paste(hints, collapse = ", ")) else ""
      msg <- sprintf("filter_var: value(s) %s not found in column '%s'.%s Available values: %s",
                     paste0("'", missing, "'", collapse = ", "), col, hint, .mcp_values_text(present))
      if (length(missing) == length(wanted)) stop(msg, call. = FALSE)
      warning(msg, call. = FALSE)
    }
    df <- df[as.character(df[[col]]) %in% wanted, , drop = FALSE]
  }
  df
}

#' Number of figures a call will create. Mirrors the plotting functions: one
#' figure per unit and per variable (unless variables are on the x-axis), per
#' split_by combination, per x value when unstacked, and per panel when
#' separate_figure = TRUE.
#' @keywords internal
#' @noRd
.mcp_count_figures <- function(df, split_by, panel_var = NULL, separate_figure = FALSE,
                               x_axis_from = NULL, unstack = FALSE,
                               variable_col = "Variable", unit_col = "Unit") {
  cols <- unit_col
  if (!identical(x_axis_from, variable_col)) cols <- c(cols, variable_col)
  if (is.character(split_by)) cols <- c(cols, split_by)
  cols <- intersect(unique(cols), names(df))
  n <- if (length(cols)) nrow(unique(df[cols])) else 1L
  if (isTRUE(unstack) && is.character(x_axis_from) && x_axis_from %in% names(df)) {
    n <- n * length(unique(df[[x_axis_from]]))
  }
  if (isTRUE(separate_figure) && is.character(panel_var) && panel_var %in% names(df)) {
    n <- n * length(unique(df[[panel_var]]))
  }
  max(n, 1L)
}

# OUTPUT FILES -------------------------------------------------------------------

#' @keywords internal
#' @noRd
.mcp_snapshot <- function(dir) {
  if (is.null(dir) || !dir.exists(dir)) return(NULL)
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  if (!length(files)) return(data.frame(path = character(), mtime = numeric(), size = numeric()))
  info <- file.info(files)
  data.frame(path = files, mtime = as.numeric(info$mtime), size = info$size, stringsAsFactors = FALSE)
}

#' Files created or modified since `before`, oldest first.
#' @keywords internal
#' @noRd
.mcp_changed_files <- function(before, dir) {
  after <- .mcp_snapshot(dir)
  if (is.null(after) || !nrow(after)) return(character())
  if (is.null(before) || !nrow(before)) {
    changed <- rep(TRUE, nrow(after))
  } else {
    m <- match(after$path, before$path)
    changed <- is.na(m) | after$mtime != before$mtime[m] | after$size != before$size[m]
  }
  after <- after[changed & basename(after$path) != "gtapviz_session.R", , drop = FALSE]
  after <- after[order(after$mtime), , drop = FALSE]
  .mcp_path(after$path)
}

# TEXT FORMATTING ----------------------------------------------------------------

#' Compact markdown table.
#' @keywords internal
#' @noRd
.mcp_md_table <- function(df, digits = 4L) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!nrow(df)) return("(no rows)")
  cells <- lapply(df, function(col) {
    if (is.numeric(col)) {
      out <- as.character(round(col, digits))
      out[is.na(col)] <- "NA"
      out
    } else {
      out <- as.character(col)
      out[is.na(out)] <- ""
      gsub("|", "/", out, fixed = TRUE)
    }
  })
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  rows <- do.call(paste, c(cells, sep = " | "))
  paste(c(header, sep, paste0("| ", rows, " |")), collapse = "\n")
}

#' Text description of one table: columns, values and variables.
#' @keywords internal
#' @noRd
.mcp_describe_table <- function(ref, df, max_values = 30L, max_vars = 80L) {
  lines <- sprintf("%s: %d rows x %d columns", ref, nrow(df), ncol(df))
  lines <- c(lines, "", "Columns:")
  meta_cols <- c("Variable", "Description", "Unit")
  for (col in setdiff(names(df), meta_cols)) {
    x <- df[[col]]
    if (is.numeric(x) && col %in% c("Value", "PeriodRange") || (is.numeric(x) && length(unique(x)) > max_values)) {
      rng <- suppressWarnings(range(x, na.rm = TRUE))
      lines <- c(lines, sprintf("- %s (numeric): from %s to %s", col, signif(rng[1], 5), signif(rng[2], 5)))
    } else {
      u <- unique(as.character(x))
      lines <- c(lines, sprintf("- %s (%d value%s): %s", col, length(u), if (length(u) == 1) "" else "s",
                                .mcp_values_text(u, max_values)))
    }
  }
  if ("Variable" %in% names(df)) {
    keep <- intersect(meta_cols, names(df))
    vars <- unique(df[keep])
    vars <- vars[!duplicated(vars$Variable), , drop = FALSE]
    lines <- c(lines, "", sprintf("Variables (%d):", nrow(vars)))
    shown <- utils::head(vars, max_vars)
    lines <- c(lines, .mcp_md_table(shown))
    if (nrow(vars) > max_vars) {
      lines <- c(lines, sprintf("... and %d more: %s", nrow(vars) - max_vars,
                                .mcp_values_text(vars$Variable[-seq_len(max_vars)], 60)))
    }
  }
  paste(lines, collapse = "\n")
}

#' One-line summary per table, grouped by dataset object.
#' @keywords internal
#' @noRd
.mcp_overview <- function(srv) {
  index <- .mcp_dataset_index(srv)
  if (!length(index)) return("No data loaded yet. Call load_gtap_results (folder = \"example\" for demo data).")
  std <- c("Value", "Variable", "Unit", "Description", "Subtotal", "Dimension", "Experiment")
  lines <- "Loaded datasets (pass the reference in quotes as `data`):"
  for (obj in unique(vapply(index, `[[`, "", "object"))) {
    items <- Filter(function(d) d$object == obj, index)
    lines <- c(lines, "", sprintf("%s (%d table%s)", obj, length(items), if (length(items) == 1) "" else "s"))
    for (d in items) {
      df <- d$df
      dims <- setdiff(names(df), std)
      vars <- if ("Variable" %in% names(df)) unique(as.character(df$Variable)) else character()
      lines <- c(lines, sprintf(
        "- %s: %d rows | dimensions: %s | %d variable%s: %s",
        d$ref, nrow(df), if (length(dims)) paste(dims, collapse = ", ") else "-",
        length(vars), if (length(vars) == 1) "" else "s", .mcp_values_text(vars, 10)
      ))
    }
  }
  if (any(vapply(index, function(d) "Experiment" %in% names(d$df), logical(1)))) {
    exps <- unique(unlist(lapply(index, function(d) as.character(d$df$Experiment))))
    lines <- c(lines, "", sprintf("Experiments: %s", .mcp_values_text(exps)))
  }
  paste(lines, collapse = "\n")
}

# RD (HELP) TEXT -----------------------------------------------------------------

#' Argument descriptions from the installed help pages.
#' @keywords internal
#' @noRd
.mcp_rd_arguments <- function(topic) {
  db <- tryCatch(tools::Rd_db("GTAPViz"), error = function(e) list())
  rd <- db[[paste0(topic, ".Rd")]]
  if (is.null(rd)) return(character())
  tags <- vapply(rd, function(x) {
    t <- attr(x, "Rd_tag")
    if (is.null(t)) "" else t
  }, character(1))
  pos <- which(tags == "\\arguments")
  if (!length(pos)) return(character())
  out <- character()
  for (item in rd[[pos[1]]]) {
    if (identical(attr(item, "Rd_tag"), "\\item") && length(item) >= 2L) {
      text <- function(x) trimws(gsub("\\s+", " ", paste(unlist(x), collapse = "")))
      for (nm in trimws(strsplit(text(item[[1]]), ",")[[1]])) out[nm] <- text(item[[2]])
    }
  }
  out
}
