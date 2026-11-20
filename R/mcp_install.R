# GTAPViz MCP SETUP ============================================================

#' @title Connect GTAPViz to Your AI App (One-Time Setup)
#' @md
#'
#' @description
#' Registers the GTAPViz MCP server with an AI app so you can ask the AI, in
#' plain language, to load your GTAP results and create figures and tables.
#' Run it once, then fully quit and reopen the AI app.
#'
#' After that you can ask, for example: *"Use GTAPViz to load the example data
#' and plot welfare (EV) and real GDP by region."*
#'
#' @param client Character. The AI app to set up:
#'   - `"claude_desktop"` (default): writes the Claude Desktop configuration file.
#'   - `"claude_code"`: registers the server with the Claude Code CLI (user scope).
#'   - `"cursor"`: writes `~/.cursor/mcp.json`.
#'   - `"other"`: prints the settings to paste into any other MCP-capable app
#'     (VS Code, Gemini CLI, Codex, Windsurf, ...). Same as [mcp_config()].
#' @param allow_r_code Logical. If `TRUE`, the AI may also run arbitrary R code in
#'   the GTAPViz session for custom data manipulation. Default `FALSE`.
#' @param output_dir Character or `NULL`. Default folder for figures, tables and
#'   the session script. If `NULL`, outputs go next to the input data.
#' @param config_path Character or `NULL`. Path to the app's configuration file
#'   if it is not in the standard location.
#'
#' @details
#' The configuration starts `Rscript` from this R installation and runs
#' [mcp_serve()]. Your R, GTAPViz and data stay on your computer; no account,
#' API key or server of the package author is involved. An existing
#' configuration file is backed up (`.backup`) before it is changed, and other
#' servers in it are kept.
#'
#' Requirements: R and GTAPViz installed on the same computer as the AI app.
#' Users do not need to know R; the AI writes and runs the code and saves it
#' as a reproducible script (`gtapviz_session.R`).
#'
#' @return Invisibly, the path(s) of the configuration file(s) written (or the
#'   server settings for `client = "other"`).
#'
#' @author Pattawee Puangchit
#' @seealso [mcp_config()], [mcp_serve()]
#' @export
#'
#' @examples
#' \dontrun{
#' # Claude Desktop
#' GTAPViz::mcp_install()
#'
#' # Claude Code or Cursor
#' GTAPViz::mcp_install("claude_code")
#' GTAPViz::mcp_install("cursor")
#'
#' # Any other MCP app: print the settings to paste
#' GTAPViz::mcp_install("other")
#' }
mcp_install <- function(client = c("claude_desktop", "claude_code", "cursor", "other"),
                        allow_r_code = FALSE, output_dir = NULL, config_path = NULL) {
  client <- match.arg(client)
  server <- .mcp_server_spec(allow_r_code = allow_r_code, output_dir = output_dir)

  if (client == "other") return(mcp_config(allow_r_code = allow_r_code, output_dir = output_dir))

  if (client == "claude_code") {
    cmd_args <- c("mcp", "add", "--scope", "user", "gtapviz", "--",
                  shQuote(server$command), unlist(lapply(server$args, shQuote)))
    if (!.mcp_confirm("Add GTAPViz to Claude Code (runs `claude mcp add`)?")) {
      return(invisible(NULL))
    }
    claude <- Sys.which("claude")
    status <- if (nzchar(claude)) {
      tryCatch(suppressWarnings(system2(claude, cmd_args, stdout = TRUE, stderr = TRUE)),
               error = function(e) structure("", status = 1L))
    } else {
      structure("", status = 1L)
    }
    ok <- nzchar(claude) && is.null(attr(status, "status"))
    if (ok) {
      message("GTAPViz was added to Claude Code (all projects). Start a new `claude` session and ",
              "ask it to use GTAPViz.")
    } else {
      message("Run this command in a terminal to add GTAPViz to Claude Code:\n\n  claude ",
              paste(cmd_args, collapse = " "), "\n")
    }
    return(invisible(NULL))
  }

  paths <- if (!is.null(config_path)) {
    .mcp_path(config_path)
  } else if (client == "cursor") {
    .mcp_path(file.path(path.expand("~"), ".cursor", "mcp.json"))
  } else {
    .mcp_claude_desktop_paths()
  }
  app <- if (client == "cursor") "Cursor" else "Claude Desktop"
  if (!.mcp_confirm(sprintf("Add GTAPViz to the %s settings in\n  %s\n(a .backup copy is kept)?",
                            app, paste(paths, collapse = "\n  ")))) {
    return(invisible(NULL))
  }
  for (p in paths) .mcp_write_config(p, server)

  message(
    "GTAPViz is now connected to ", app, ".\n",
    "Settings written to: ", paste(paths, collapse = "\n  "), "\n\n",
    "Next steps:\n",
    "  1. Fully quit ", app, " (also from the system tray / menu bar) and open it again.\n",
    "  2. Ask: \"Use GTAPViz to load the example data and plot welfare (EV) by region.\"\n",
    "  3. To use your own results, give the AI the folder with your .sl4/.har files.\n"
  )
  .mcp_check_installed_version()
  invisible(paths)
}

#' @title Show the GTAPViz MCP Server Settings
#' @md
#'
#' @description
#' Prints the settings that start the GTAPViz MCP server, ready to paste into
#' any MCP-capable AI app. Use [mcp_install()] to write them automatically for
#' Claude Desktop, Claude Code or Cursor.
#'
#' @inheritParams mcp_install
#'
#' @return Invisibly, a list with `command` and `args`.
#'
#' @author Pattawee Puangchit
#' @seealso [mcp_install()], [mcp_serve()]
#' @export
#'
#' @examples
#' \dontrun{
#' mcp_config()
#' }
mcp_config <- function(allow_r_code = FALSE, output_dir = NULL) {
  server <- .mcp_server_spec(allow_r_code = allow_r_code, output_dir = output_dir)
  json <- jsonlite::toJSON(list(mcpServers = list(gtapviz = server)),
                           auto_unbox = TRUE, pretty = TRUE)
  cat(
    "GTAPViz MCP server settings (the \"mcpServers\" format used by most AI apps):\n\n",
    json, "\n\n",
    "VS Code: put the same entry under \"servers\" in mcp.json and add \"type\": \"stdio\".\n",
    "Command line: ", shQuote(server$command), " ",
    paste(vapply(server$args, shQuote, character(1)), collapse = " "), "\n",
    sep = ""
  )
  invisible(server)
}

# HELPERS ----------------------------------------------------------------------

#' Command that starts the server with this R installation. Uses single quotes
#' inside the R expression so it survives every app's argument quoting.
#' @keywords internal
#' @noRd
.mcp_server_spec <- function(allow_r_code = FALSE, output_dir = NULL) {
  exe <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  rscript <- normalizePath(file.path(R.home("bin"), exe), winslash = "/", mustWork = FALSE)
  quote1 <- function(x) gsub("'", "\\\\'", x)

  serve_args <- character()
  if (isTRUE(allow_r_code)) serve_args <- c(serve_args, "allow_r_code = TRUE")
  if (!is.null(output_dir)) serve_args <- c(serve_args, sprintf("output_dir = '%s'", quote1(.mcp_path(output_dir))))
  expr <- sprintf("GTAPViz::mcp_serve(%s)", paste(serve_args, collapse = ", "))

  pkg <- find.package("GTAPViz", lib.loc = .libPaths(), quiet = TRUE)
  if (length(pkg)) {
    lib <- normalizePath(dirname(pkg[1]), winslash = "/")
    expr <- sprintf(".libPaths(c('%s', .libPaths())); %s", quote1(lib), expr)
  }
  # --no-init-file: a user .Rprofile that prints text would break the protocol
  list(command = rscript, args = list("--no-init-file", "-e", expr))
}

#' @keywords internal
#' @noRd
.mcp_claude_desktop_paths <- function() {
  if (.Platform$OS.type == "windows") {
    appdata <- gsub("\\\\", "/", Sys.getenv("APPDATA"))
    local <- gsub("\\\\", "/", Sys.getenv("LOCALAPPDATA"))
    paths <- file.path(appdata, "Claude", "claude_desktop_config.json")
    # Microsoft Store version keeps its settings in a virtualized folder
    store <- Sys.glob(file.path(local, "Packages", "Claude_*", "LocalCache", "Roaming", "Claude"))
    if (length(store)) paths <- c(paths, file.path(store, "claude_desktop_config.json"))
  } else if (identical(Sys.info()[["sysname"]], "Darwin")) {
    paths <- file.path(path.expand("~"), "Library", "Application Support", "Claude", "claude_desktop_config.json")
  } else {
    paths <- file.path(path.expand("~"), ".config", "Claude", "claude_desktop_config.json")
  }
  .mcp_path(paths)
}

#' Add or replace the "gtapviz" entry in an MCP JSON config, keeping everything
#' else and backing up the original file.
#' @keywords internal
#' @noRd
.mcp_write_config <- function(path, server, key = "mcpServers", name = "gtapviz") {
  cfg <- .mcp_empty_object()
  if (file.exists(path) && isTRUE(file.size(path) > 0)) {
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    if (nzchar(trimws(txt))) {
      cfg <- tryCatch(
        jsonlite::fromJSON(txt, simplifyVector = FALSE),
        error = function(e) {
          stop("Could not read ", path, " because it is not valid JSON. Fix or delete it, then run ",
               "mcp_install() again.\nDetails: ", conditionMessage(e), call. = FALSE)
        }
      )
      if (!is.list(cfg) || (length(cfg) && is.null(names(cfg)))) {
        stop(path, " does not contain a JSON object; not changing it.", call. = FALSE)
      }
    }
    file.copy(path, paste0(path, ".backup"), overwrite = TRUE)
  } else if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE)
  }
  servers <- cfg[[key]]
  if (!is.list(servers)) servers <- .mcp_empty_object()
  servers[[name]] <- server
  cfg[[key]] <- servers
  json <- jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  writeLines(enc2utf8(as.character(json)), path, useBytes = TRUE)
  invisible(path)
}

#' Ask before changing the user's files (interactive sessions only; a call in
#' a script is taken as the user's explicit choice).
#' @keywords internal
#' @noRd
.mcp_confirm <- function(question) {
  if (!interactive()) return(TRUE)
  ok <- utils::askYesNo(question, default = TRUE)
  if (!isTRUE(ok)) message("Cancelled; nothing was changed.")
  isTRUE(ok)
}

#' Warn when the AI app would start an older installed GTAPViz without the
#' MCP server (e.g. while developing with devtools::load_all()).
#' @keywords internal
#' @noRd
.mcp_check_installed_version <- function() {
  pkg <- find.package("GTAPViz", lib.loc = .libPaths(), quiet = TRUE)
  if (!length(pkg)) {
    warning("GTAPViz is not installed in an R library, so the AI app cannot start it. ",
            "Install it first: remotes::install_github(\"Bodysbobb/GTAPViz\").", call. = FALSE)
    return(invisible(FALSE))
  }
  ns <- readLines(file.path(pkg[1], "NAMESPACE"), warn = FALSE)
  if (!any(grepl("mcp_serve", ns, fixed = TRUE))) {
    warning("The installed GTAPViz (", utils::packageDescription("GTAPViz", lib.loc = dirname(pkg[1]))$Version,
            ") has no MCP server. Update it with remotes::install_github(\"Bodysbobb/GTAPViz\"), then restart the AI app.", call. = FALSE)
    return(invisible(FALSE))
  }
  invisible(TRUE)
}
