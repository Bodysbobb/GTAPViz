# GTAPViz 1.3.0

* New: use GTAPViz from AI assistants (Claude Desktop, Claude Code, Cursor, ...)
  through a built-in MCP server. Set it up once with `mcp_install()`; see
  `vignette("GTAPViz_AI")`. Every step the AI takes is saved as a reproducible
  R script (`gtapviz_session.R`).
* New functions: `mcp_install()`, `mcp_config()` and `mcp_serve()`.
* Fixed `auto_gtap()`, which failed with the default settings. With
  `rename_columns = TRUE` it now renames `REG`, `COMM`, `ACTS` and `Experiment`
  to `Region`, `Commodity`, `Activity` and `Scenario`, as documented.
* Updated the help pages of `create_export_config()`, `create_trend_style()`
  and `sort_plot_data()`.

# GTAPViz 1.2.0

* Previous CRAN release.
