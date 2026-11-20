# Start the GTAPViz MCP Server for AI Assistants

Starts a [Model Context Protocol](https://modelcontextprotocol.io) (MCP)
server that lets AI assistants (Claude Desktop, Claude Code, Cursor, VS
Code, Gemini CLI and other MCP clients) use GTAPViz on your computer:
load `.sl4`/`.har` results, inspect them, and create figures and tables
from plain-language requests.

You normally never call this function yourself. Run
[`mcp_install()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_install.md)
once and your AI app starts the server whenever it needs it.

## Usage

``` r
mcp_serve(output_dir = NULL, allow_r_code = FALSE)
```

## Arguments

- output_dir:

  Character or `NULL`. Default folder for figures, tables and the
  session script when the AI does not choose one. If `NULL` (default),
  outputs go to an `out` folder next to the input folder (or to
  `~/GTAPViz-output` for the example data).

- allow_r_code:

  Logical. If `TRUE`, the AI may also run arbitrary R code in the
  session (tool `run_r_code`) for custom data manipulation. Default is
  `FALSE`, which limits the AI to GTAPViz functions.

## Value

Invisibly `NULL`, once the AI app closes the connection.

## Details

**Reproducibility.** Every action is executed as ordinary GTAPViz R code
and appended to a session script (`gtapviz_session.R`, saved in the
output folder). Running that script in R recreates every figure and
table.

**Privacy.** The server runs locally and only talks to the AI app that
started it. Your files are read on your computer. Results that a tool
returns (summaries, numbers, preview images) become part of your
conversation with the AI provider you chose, just like anything else you
type there.

## See also

[`mcp_install()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_install.md),
[`mcp_config()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_config.md)

## Author

Pattawee Puangchit

## Examples

``` r
if (FALSE) { # \dontrun{
# Started by the AI app, e.g. from a terminal:
# Rscript -e "GTAPViz::mcp_serve()"
} # }
```
