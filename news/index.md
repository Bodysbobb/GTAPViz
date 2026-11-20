# Changelog

## GTAPViz 1.3.0

- New: use GTAPViz from AI assistants (Claude Desktop, Claude Code,
  Cursor, …) through a built-in MCP server. Set it up once with
  [`mcp_install()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_install.md);
  see
  [`vignette("GTAPViz_AI")`](https://www.pattawee-pp.com/GTAPViz/articles/GTAPViz_AI.md).
  Every step the AI takes is saved as a reproducible R script
  (`gtapviz_session.R`).
- New functions:
  [`mcp_install()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_install.md),
  [`mcp_config()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_config.md)
  and
  [`mcp_serve()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_serve.md).
- Fixed
  [`auto_gtap()`](https://www.pattawee-pp.com/GTAPViz/reference/auto_gtap.md),
  which failed with the default settings. With `rename_columns = TRUE`
  it now renames `REG`, `COMM`, `ACTS` and `Experiment` to `Region`,
  `Commodity`, `Activity` and `Scenario`, as documented.
- Updated the help pages of
  [`create_export_config()`](https://www.pattawee-pp.com/GTAPViz/reference/create_export_config.md),
  [`create_trend_style()`](https://www.pattawee-pp.com/GTAPViz/reference/create_trend_style.md)
  and
  [`sort_plot_data()`](https://www.pattawee-pp.com/GTAPViz/reference/sort_plot_data.md).

## GTAPViz 1.2.0

- Previous CRAN release.
