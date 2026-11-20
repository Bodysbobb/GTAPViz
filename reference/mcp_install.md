# Connect GTAPViz to Your AI App (One-Time Setup)

Registers the GTAPViz MCP server with an AI app so you can ask the AI,
in plain language, to load your GTAP results and create figures and
tables. Run it once, then fully quit and reopen the AI app.

After that you can ask, for example: *"Use GTAPViz to load the example
data and plot welfare (EV) and real GDP by region."*

## Usage

``` r
mcp_install(
  client = c("claude_desktop", "claude_code", "cursor", "other"),
  allow_r_code = FALSE,
  output_dir = NULL,
  config_path = NULL
)
```

## Arguments

- client:

  Character. The AI app to set up:

  - `"claude_desktop"` (default): writes the Claude Desktop
    configuration file.

  - `"claude_code"`: registers the server with the Claude Code CLI (user
    scope).

  - `"cursor"`: writes `~/.cursor/mcp.json`.

  - `"other"`: prints the settings to paste into any other MCP-capable
    app (VS Code, Gemini CLI, Codex, Windsurf, ...). Same as
    [`mcp_config()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_config.md).

- allow_r_code:

  Logical. If `TRUE`, the AI may also run arbitrary R code in the
  GTAPViz session for custom data manipulation. Default `FALSE`.

- output_dir:

  Character or `NULL`. Default folder for figures, tables and the
  session script. If `NULL`, outputs go next to the input data.

- config_path:

  Character or `NULL`. Path to the app's configuration file if it is not
  in the standard location.

## Value

Invisibly, the path(s) of the configuration file(s) written (or the
server settings for `client = "other"`).

## Details

The configuration starts `Rscript` from this R installation and runs
[`mcp_serve()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_serve.md).
Your R, GTAPViz and data stay on your computer; no account, API key or
server of the package author is involved. An existing configuration file
is backed up (`.backup`) before it is changed, and other servers in it
are kept.

Requirements: R and GTAPViz installed on the same computer as the AI
app. Users do not need to know R; the AI writes and runs the code and
saves it as a reproducible script (`gtapviz_session.R`).

## See also

[`mcp_config()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_config.md),
[`mcp_serve()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_serve.md)

## Author

Pattawee Puangchit

## Examples

``` r
if (FALSE) { # \dontrun{
# Claude Desktop
GTAPViz::mcp_install()

# Claude Code or Cursor
GTAPViz::mcp_install("claude_code")
GTAPViz::mcp_install("cursor")

# Any other MCP app: print the settings to paste
GTAPViz::mcp_install("other")
} # }
```
