# GTAPViz MCP SERVER ===========================================================
#
# A small Model Context Protocol (MCP) server over stdio: JSON-RPC 2.0 messages,
# one per line on stdin/stdout. It lets AI apps (Claude Desktop, Claude Code,
# Cursor, VS Code, Gemini CLI, ...) call GTAPViz on the user's own computer.
#
# Design:
# - Every tool builds a real GTAPViz R call, evaluates it in a private workspace
#   and appends the formatted code to a session script, so every figure and
#   table can be reproduced in plain R without the AI.
# - stdout carries protocol messages only. Anything R prints while a tool runs
#   is captured and returned to the AI as text.
# - Nothing is sent anywhere by this server: it only answers the local AI app
#   that started it.

.mcp_protocol_versions <- c("2025-11-25", "2025-06-18", "2025-03-26", "2024-11-05")

#' @title Start the GTAPViz MCP Server for AI Assistants
#' @md
#'
#' @description
#' Starts a [Model Context Protocol](https://modelcontextprotocol.io) (MCP) server
#' that lets AI assistants (Claude Desktop, Claude Code, Cursor, VS Code, Gemini CLI
#' and other MCP clients) use GTAPViz on your computer: load `.sl4`/`.har`
#' results, inspect them, and create figures and tables from plain-language
#' requests.
#'
#' You normally never call this function yourself. Run [mcp_install()] once and
#' your AI app starts the server whenever it needs it.
#'
#' @param output_dir Character or `NULL`. Default folder for figures, tables and
#'   the session script when the AI does not choose one. If `NULL` (default),
#'   outputs go to an `out` folder next to the input folder (or to
#'   `~/GTAPViz-output` for the example data).
#' @param allow_r_code Logical. If `TRUE`, the AI may also run arbitrary R code
#'   in the session (tool `run_r_code`) for custom data manipulation. Default is
#'   `FALSE`, which limits the AI to GTAPViz functions.
#'
#' @details
#' **Reproducibility.** Every action is executed as ordinary GTAPViz R code and
#' appended to a session script (`gtapviz_session.R`, saved in the output
#' folder). Running that script in R recreates every figure and table.
#'
#' **Privacy.** The server runs locally and only talks to the AI app that
#' started it. Your files are read on your computer. Results that a tool returns
#' (summaries, numbers, preview images) become part of your conversation with
#' the AI provider you chose, just like anything else you type there.
#'
#' @return Invisibly `NULL`, once the AI app closes the connection.
#'
#' @author Pattawee Puangchit
#' @seealso [mcp_install()], [mcp_config()]
#' @export
#'
#' @examples
#' \dontrun{
#' # Started by the AI app, e.g. from a terminal:
#' # Rscript -e "GTAPViz::mcp_serve()"
#' }
mcp_serve <- function(output_dir = NULL, allow_r_code = FALSE) {
  if (interactive()) {
    message(
      "mcp_serve() is started by your AI app, not run in the console.\n",
      "Run GTAPViz::mcp_install() once, then restart your AI app.\n",
      "See ?mcp_install for details."
    )
    return(invisible(NULL))
  }
  srv <- .mcp_new_server(output_dir = output_dir, allow_r_code = allow_r_code)
  .mcp_stdio_loop(srv)
  invisible(NULL)
}

# SERVER STATE -----------------------------------------------------------------

#' @keywords internal
#' @noRd
.mcp_new_server <- function(output_dir = NULL, allow_r_code = FALSE) {
  srv <- new.env(parent = emptyenv())
  srv$ws <- new.env(parent = .mcp_exports_env())
  srv$output_dir <- if (!is.null(output_dir)) .mcp_path(output_dir) else NULL
  srv$allow_r_code <- isTRUE(allow_r_code)
  srv$steps <- list()
  srv$started <- Sys.time()
  srv$tools <- .mcp_tool_registry(srv$allow_r_code)
  srv
}

#' Environment that mimics library(GTAPViz): exported functions only, so the
#' recorded script behaves the same after library(GTAPViz).
#' @keywords internal
#' @noRd
.mcp_exports_env <- function() {
  ns <- asNamespace("GTAPViz")
  env <- new.env(parent = globalenv())
  for (f in getNamespaceExports(ns)) {
    assign(f, get(f, envir = ns), envir = env)
  }
  env
}

#' @keywords internal
#' @noRd
.mcp_pkg_version <- function() {
  as.character(utils::packageVersion("GTAPViz"))
}

#' @keywords internal
#' @noRd
.mcp_guide_text <- function() {
  path <- system.file("mcp", "guide.md", package = "GTAPViz")
  if (!nzchar(path) || !file.exists(path)) {
    return("GTAPViz MCP server. Call load_gtap_results, then describe_data, then a plot tool.")
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# TRANSPORT ------------------------------------------------------------------

#' @keywords internal
#' @noRd
.mcp_stdio_loop <- function(srv) {
  con <- file("stdin", open = "r")
  on.exit(close(con), add = TRUE)
  out <- stdout()

  # Stray print() of a plot must not create Rplots.pdf or touch stdout.
  grDevices::pdf(NULL)
  on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)

  repeat {
    line <- readLines(con, n = 1L, warn = FALSE, encoding = "UTF-8")
    if (length(line) == 0L) break
    if (!nzchar(trimws(line))) next
    response <- .mcp_handle_message(srv, line)
    if (!is.null(response)) .mcp_send(out, response)
  }
}

#' @keywords internal
#' @noRd
.mcp_send <- function(out, msg) {
  json <- tryCatch(
    .mcp_json(msg),
    error = function(e) {
      .mcp_json(.mcp_rpc_error(msg$id, -32603L, paste("Could not encode response:", conditionMessage(e))))
    }
  )
  writeLines(enc2utf8(json), con = out, useBytes = TRUE)
  flush(out)
}

#' @keywords internal
#' @noRd
.mcp_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null",
                                digits = NA, force = TRUE))
}

# JSON-RPC ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
.mcp_handle_message <- function(srv, line) {
  msg <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) e)
  if (inherits(msg, "error")) {
    return(.mcp_rpc_error(NULL, -32700L, "Parse error: invalid JSON"))
  }
  # JSON-RPC batch (allowed by older protocol versions)
  if (is.list(msg) && is.null(names(msg)) && length(msg) > 0L) {
    responses <- Filter(Negate(is.null), lapply(msg, function(m) .mcp_dispatch(srv, m)))
    return(if (length(responses)) responses else NULL)
  }
  .mcp_dispatch(srv, msg)
}

#' @keywords internal
#' @noRd
.mcp_dispatch <- function(srv, msg) {
  if (!is.list(msg) || is.null(msg$method)) return(NULL)
  is_request <- "id" %in% names(msg) && !is.null(msg$id)
  params <- msg$params
  if (is.null(params)) params <- list()

  result <- tryCatch(
    switch(
      msg$method,
      "initialize" = .mcp_initialize(srv, params),
      "ping" = .mcp_empty_object(),
      "tools/list" = list(tools = .mcp_tool_list(srv)),
      "tools/call" = .mcp_tools_call(srv, params),
      "prompts/list" = list(prompts = .mcp_prompt_list()),
      "prompts/get" = .mcp_prompt_get(params),
      "resources/list" = list(resources = list()),
      "resources/templates/list" = list(resourceTemplates = list()),
      .mcp_rpc_condition(-32601L, paste0("Method not found: ", msg$method))
    ),
    mcp_rpc_error = function(e) e,
    error = function(e) .mcp_rpc_condition(-32603L, conditionMessage(e))
  )

  if (!is_request) return(NULL)
  if (inherits(result, "mcp_rpc_error")) {
    return(.mcp_rpc_error(msg$id, result$code, conditionMessage(result)))
  }
  list(jsonrpc = "2.0", id = msg$id, result = result)
}

#' @keywords internal
#' @noRd
.mcp_rpc_condition <- function(code, message) {
  structure(
    class = c("mcp_rpc_error", "error", "condition"),
    list(message = message, call = NULL, code = code)
  )
}

#' @keywords internal
#' @noRd
.mcp_rpc_error <- function(id, code, message) {
  list(jsonrpc = "2.0", id = id, error = list(code = code, message = message))
}

#' @keywords internal
#' @noRd
.mcp_empty_object <- function() {
  structure(list(), names = character(0))
}

#' @keywords internal
#' @noRd
.mcp_initialize <- function(srv, params) {
  requested <- params$protocolVersion
  version <- if (is.character(requested) && requested %in% .mcp_protocol_versions) {
    requested
  } else {
    .mcp_protocol_versions[1]
  }
  list(
    protocolVersion = version,
    capabilities = list(
      tools = list(listChanged = FALSE),
      prompts = list(listChanged = FALSE)
    ),
    serverInfo = list(name = "gtapviz", title = "GTAPViz", version = .mcp_pkg_version()),
    instructions = .mcp_guide_text()
  )
}

# TOOL EXECUTION ---------------------------------------------------------------

#' @keywords internal
#' @noRd
.mcp_tool_list <- function(srv) {
  unname(lapply(srv$tools, function(t) {
    t$handler <- NULL
    t
  }))
}

#' @keywords internal
#' @noRd
.mcp_tools_call <- function(srv, params) {
  name <- params$name
  if (!is.character(name) || is.null(srv$tools[[name]])) {
    stop(.mcp_rpc_condition(-32602L, paste0("Unknown tool: ", format(name))))
  }
  args <- params$arguments
  if (is.null(args)) args <- list()
  .mcp_run_tool(srv, srv$tools[[name]], args)
}

#' Run a tool handler with all R output captured. Errors become tool results
#' with isError = TRUE so the AI can read them and try again.
#' @keywords internal
#' @noRd
.mcp_run_tool <- function(srv, tool, args) {
  logs <- character()
  warns <- character()
  value <- NULL
  printed <- utils::capture.output(
    value <- tryCatch(
      withCallingHandlers(
        tool$handler(srv, args),
        message = function(m) {
          logs <<- c(logs, conditionMessage(m))
          invokeRestart("muffleMessage")
        },
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    ),
    type = "output"
  )
  console <- .mcp_clean_log(c(printed, logs))

  if (inherits(value, "error")) {
    text <- paste0("Error: ", conditionMessage(value))
    if (length(warns)) {
      text <- paste0(text, "\n\nWarnings:\n", paste0("- ", unique(warns), collapse = "\n"))
    }
    if (length(console)) {
      text <- paste0(text, "\n\nR console output:\n", paste(utils::tail(console, 25), collapse = "\n"))
    }
    return(list(content = list(.mcp_text(text)), isError = TRUE))
  }
  .mcp_tool_result(value, console, warns)
}

#' Build an MCP tool result from a handler's return value:
#' list(text, code, files, images, show_log).
#' @keywords internal
#' @noRd
.mcp_tool_result <- function(value, console = character(), warns = character()) {
  if (is.character(value)) value <- list(text = value)
  parts <- character()
  if (length(value$text)) parts <- c(parts, paste(value$text, collapse = "\n"))
  if (length(value$files)) {
    shown <- utils::head(value$files, 40)
    more <- length(value$files) - length(shown)
    parts <- c(parts, paste0(
      "Files saved (", length(value$files), "):\n",
      paste0("- ", shown, collapse = "\n"),
      if (more > 0) paste0("\n- ... and ", more, " more") else ""
    ))
  }
  if (length(value$code) && any(nzchar(value$code))) {
    parts <- c(parts, paste0(
      "R code (added to the session script):\n```r\n",
      paste(value$code, collapse = "\n"), "\n```"
    ))
  }
  if (length(warns)) {
    parts <- c(parts, paste0("Warnings:\n", paste0("- ", unique(warns), collapse = "\n")))
  }
  if (isTRUE(value$show_log) && length(console)) {
    parts <- c(parts, paste0("R console output:\n", paste(utils::tail(console, 30), collapse = "\n")))
  }
  content <- list(.mcp_text(paste(parts, collapse = "\n\n")))
  for (img in value$images) {
    preview <- .mcp_image_content(img)
    if (!is.null(preview)) content <- c(content, list(preview))
  }
  list(content = content, isError = FALSE)
}

#' @keywords internal
#' @noRd
.mcp_text <- function(text) {
  list(type = "text", text = paste(text, collapse = "\n"))
}

#' Downscaled PNG preview (base64) so the AI can look at a figure.
#' @keywords internal
#' @noRd
.mcp_image_content <- function(path, max_px = 1200L) {
  if (!file.exists(path)) return(NULL)
  raw <- NULL
  if (requireNamespace("magick", quietly = TRUE)) {
    raw <- tryCatch({
      img <- magick::image_read(path)
      info <- magick::image_info(img)
      if (max(info$width, info$height) > max_px) {
        img <- magick::image_resize(img, paste0(max_px, "x", max_px, ">"))
      }
      magick::image_write(img, format = "png")
    }, error = function(e) NULL)
  }
  if (is.null(raw)) {
    size <- file.size(path)
    if (is.na(size) || size > 900 * 1024) return(NULL)
    raw <- readBin(path, "raw", size)
  }
  # jsonlite wraps base64 lines; MCP clients expect one unbroken string
  data <- gsub("[\r\n]", "", jsonlite::base64_enc(raw))
  list(type = "image", data = data, mimeType = "image/png")
}

#' @keywords internal
#' @noRd
.mcp_clean_log <- function(lines) {
  lines <- unlist(strsplit(paste(lines, collapse = "\n"), "\n", fixed = TRUE))
  lines <- trimws(gsub("\r", "", lines, fixed = TRUE), which = "right")
  lines[nzchar(trimws(lines))]
}

# WORKSPACE AND SESSION SCRIPT ---------------------------------------------------

#' Evaluate expressions in the workspace and record them in the session script.
#' @keywords internal
#' @noRd
.mcp_eval <- function(srv, exprs, title, record = TRUE) {
  if (is.language(exprs)) exprs <- list(exprs)
  values <- vector("list", length(exprs))
  for (i in seq_along(exprs)) {
    values[[i]] <- eval(exprs[[i]], envir = srv$ws)
  }
  code <- paste(vapply(exprs, .mcp_format_code, character(1)), collapse = "\n")
  if (record) .mcp_record(srv, title, code)
  list(values = values, value = values[[length(values)]], code = code)
}

#' @keywords internal
#' @noRd
.mcp_record <- function(srv, title, code) {
  srv$steps[[length(srv$steps) + 1L]] <- list(title = title, code = code)
  .mcp_save_script(srv)
}

#' @keywords internal
#' @noRd
.mcp_script_text <- function(srv) {
  header <- c(
    "# ==========================================================================",
    "# GTAPViz session script (generated by the GTAPViz MCP server)",
    sprintf("# Created: %s | R %s | GTAPViz %s",
            format(srv$started, "%Y-%m-%d %H:%M"), getRversion(), .mcp_pkg_version()),
    "# Run this file in R to recreate every figure and table from the session.",
    "# ==========================================================================",
    "",
    "library(GTAPViz)"
  )
  body <- unlist(lapply(seq_along(srv$steps), function(i) {
    c("", sprintf("# ---- Step %d: %s ----", i, srv$steps[[i]]$title), srv$steps[[i]]$code)
  }))
  paste(c(header, body, ""), collapse = "\n")
}

#' @keywords internal
#' @noRd
.mcp_script_path <- function(srv) {
  dir <- .mcp_current_output_dir(srv)
  if (is.null(dir)) return(NULL)
  file.path(dir, "gtapviz_session.R")
}

#' @keywords internal
#' @noRd
.mcp_save_script <- function(srv) {
  path <- .mcp_script_path(srv)
  if (is.null(path)) return(invisible(NULL))
  tryCatch({
    if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
    writeLines(.mcp_script_text(srv), path, useBytes = TRUE)
  }, error = function(e) NULL)
  invisible(path)
}

#' Output folder currently used by the session (value of `output_path` in the
#' workspace, set by the load tools).
#' @keywords internal
#' @noRd
.mcp_current_output_dir <- function(srv) {
  if (exists("output_path", envir = srv$ws, inherits = FALSE)) {
    p <- get("output_path", envir = srv$ws)
    if (is.character(p) && length(p) == 1L && nzchar(p)) return(p)
  }
  srv$output_dir
}

#' Expression for `output_path` in a tool call. Uses the session variable
#' `output_path` unless the AI asked for a specific folder. Creates the session
#' variable (recorded in the script) when nothing has been loaded yet.
#' @keywords internal
#' @noRd
.mcp_output_expr <- function(srv, output_folder = NULL) {
  if (!is.null(output_folder) && nzchar(output_folder)) {
    return(list(expr = .mcp_path(output_folder), dir = .mcp_path(output_folder)))
  }
  if (!exists("output_path", envir = srv$ws, inherits = FALSE)) {
    dir <- if (!is.null(srv$output_dir)) srv$output_dir else .mcp_default_output_dir()
    .mcp_eval(srv, call("<-", as.name("output_path"), dir), "Output folder")
  }
  list(expr = as.name("output_path"), dir = get("output_path", envir = srv$ws))
}

#' @keywords internal
#' @noRd
.mcp_default_output_dir <- function() {
  .mcp_path(file.path(path.expand("~"), "GTAPViz-output"))
}

#' @keywords internal
#' @noRd
.mcp_path <- function(path) {
  path <- path.expand(as.character(path))
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

# CODE FORMATTING ----------------------------------------------------------------

#' Format an expression as readable R code (one argument per line for long calls).
#' @keywords internal
#' @noRd
.mcp_format_code <- function(expr, indent = 0L) {
  one_line <- paste(deparse(expr, width.cutoff = 500L), collapse = " ")
  if (nchar(one_line) + indent <= 80L) return(one_line)

  pad <- strrep(" ", indent + 2L)
  # Long vectors (literal values or c() calls): wrap at commas
  if ((is.atomic(expr) && length(expr) > 1L) ||
      (is.call(expr) && identical(expr[[1]], as.name("c")))) {
    lines <- deparse(expr, width.cutoff = max(40L, 78L - indent))
    return(paste(trimws(lines), collapse = paste0("\n", pad)))
  }
  # Literal lists: format like list(...) calls
  if (is.list(expr) && !is.data.frame(expr) && !is.null(names(expr)) && all(nzchar(names(expr)))) {
    expr <- as.call(c(list(as.name("list")), expr))
  }
  if (!is.call(expr)) return(one_line)

  fn <- expr[[1]]
  if (identical(fn, as.name("<-")) || identical(fn, as.name("="))) {
    lhs <- paste(deparse(expr[[2]], width.cutoff = 500L), collapse = " ")
    return(paste0(lhs, " <- ", .mcp_format_code(expr[[3]], indent)))
  }
  if (!is.name(fn) || as.character(fn) %in% c("[[", "[", "$", "::", ":::")) return(one_line)

  args <- as.list(expr)[-1]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  parts <- vapply(seq_along(args), function(i) {
    value <- .mcp_format_code(args[[i]], indent + 2L)
    if (nzchar(nms[i])) paste0(pad, .mcp_format_name(nms[i]), " = ", value) else paste0(pad, value)
  }, character(1))
  paste0(as.character(fn), "(\n", paste(parts, collapse = ",\n"), "\n", strrep(" ", indent), ")")
}

#' @keywords internal
#' @noRd
.mcp_format_name <- function(name) {
  if (identical(make.names(name), name)) name else paste0("`", name, "`")
}

#' Build a call from a function name and a named list of arguments.
#' @keywords internal
#' @noRd
.mcp_call <- function(fun, args = list()) {
  as.call(c(list(as.name(fun)), args))
}
