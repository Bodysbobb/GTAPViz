# Show the GTAPViz MCP Server Settings

Prints the settings that start the GTAPViz MCP server, ready to paste
into any MCP-capable AI app. Use
[`mcp_install()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_install.md)
to write them automatically for Claude Desktop, Claude Code or Cursor.

## Usage

``` r
mcp_config(allow_r_code = FALSE, output_dir = NULL)
```

## Arguments

- allow_r_code:

  Logical. If `TRUE`, the AI may also run arbitrary R code in the
  GTAPViz session for custom data manipulation. Default `FALSE`.

- output_dir:

  Character or `NULL`. Default folder for figures, tables and the
  session script. If `NULL`, outputs go next to the input data.

## Value

Invisibly, a list with `command` and `args`.

## See also

[`mcp_install()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_install.md),
[`mcp_serve()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_serve.md)

## Author

Pattawee Puangchit

## Examples

``` r
if (FALSE) { # \dontrun{
mcp_config()
} # }
```
