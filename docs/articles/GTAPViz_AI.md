# Use GTAPViz with AI Assistants (MCP)

GTAPViz includes an **MCP server** (Model Context Protocol), so an AI
assistant such as Claude, Cursor, VS Code Copilot or Gemini CLI can use
GTAPViz on your computer. You describe what you want in plain language;
the AI loads your `.sl4`/`.har` results, makes the figures and tables,
and saves **the R code it ran** as a script you can rerun at any time.

> *“Load my results in D:/project/in, plot welfare (EV) and real GDP by
> region, and make a welfare decomposition chart. Put Thailand first.”*

## What you need

- **R** (free, <https://cran.r-project.org>) and **GTAPViz**. You do not
  need to know R: the AI writes and runs the code.
- An AI app that supports MCP, for example **Claude Desktop**, **Claude
  Code**, **Cursor**, **VS Code** or **Gemini CLI**.

## One-time setup

Open R (or RStudio) and run:

``` r
install.packages("remotes")
remotes::install_github("Bodysbobb/GTAPViz")   # the AI feature needs GTAPViz 1.3.0 or later
GTAPViz::mcp_install()                         # Claude Desktop
```

The AI feature is currently available from GitHub. No extra tools (such
as Rtools) are needed; the other packages are installed from CRAN
automatically.

Then **fully quit** Claude Desktop (including from the system tray or
menu bar) and open it again. GTAPViz now appears in the app’s tools
menu.

For other apps:

``` r
GTAPViz::mcp_install("claude_code")   # Claude Code (all projects)
GTAPViz::mcp_install("cursor")        # Cursor
GTAPViz::mcp_install("other")         # prints settings to paste into any MCP app
```

**After updating R:** run
[`GTAPViz::mcp_install()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_install.md)
again, because the settings point to the R installation that ran it.

## Try it

Start with the bundled example data:

> *“Use GTAPViz to load the example data and show me the welfare (EV)
> results by region.”*

Then use your own results by giving the folder that holds your `.sl4`
and `.har` files:

> *“Load the GTAP results in D:/TradeWar/in. Which experiments are
> there?”*
>
> *“Plot qgdp, EV and tot by region, with the regions on the vertical
> axis.”*
>
> *“Show the welfare decomposition as stacked bars and rename alloc_A1
> to ‘Allocative efficiency’.”*
>
> *“Make an Excel table of all regional results with experiments as
> columns.”*
>
> *“Which sectors in SEAsia have the largest output changes? Plot the
> top 10.”*

The AI checks the variables and values before plotting, looks at a
preview of each figure, and adjusts the style when labels overlap.
Figures (PNG, and PDF on request) and Excel tables are saved in an `out`
folder next to your input folder.

Experiments, the `-WEL` file suffix and an `OutputMapping.xlsx` workbook
in a `map` folder are detected automatically, following the project
layout described in the [Data
vignette](https://www.pattawee-pp.com/GTAPViz/articles/GTAPViz_Data.md).

## Reproducible R script

Every action is run as ordinary GTAPViz code and added to
**`gtapviz_session.R`** in the output folder. Running that script in R
recreates every figure and table without the AI, so the results can be
checked, shared and cited. Ask *“show me the R script”* to see it in the
chat. An excerpt:

``` r
library(GTAPViz)

# ---- Step 1: Load GTAP results (EXP1, EXP2) ----
input_path <- "D:/TradeWar/in"
output_path <- "D:/TradeWar/out"
auto_gtap_data(
  experiment = c("EXP1", "EXP2"),
  input_path = input_path,
  har_suffix = "-WEL",
  mapping_info = "GTAPv7",
  plot_data = TRUE
)

# ---- Step 2: comparison_plot of sl4.plot.data[["REG"]] (EV, qgdp) ----
comparison_plot(
  data = sl4.plot.data[["REG"]],
  filter_var = list(Variable = c("EV", "qgdp")),
  x_axis_from = "Region",
  output_path = output_path,
  export_picture = TRUE,
  plot_style_config = create_plot_style(color_tone = "purdue")
)
```

## Privacy

- The MCP server runs **on your computer** and only talks to the AI app
  that started it. No account, key or server of the package author is
  involved.
- Your `.sl4`/`.har` files are read locally. What a tool returns
  (summaries, numbers you ask about, figure previews) becomes part of
  your conversation with the AI provider you chose, like anything you
  type there.
- By default the AI can only use GTAPViz functions.
  `mcp_install(allow_r_code = TRUE)` additionally lets it run any R
  code, for custom data work.

## What the AI can do

| Task | Tools |
|----|----|
| Find and load results | `find_gtap_files`, `load_gtap_results`, `load_gtap_dynamic` (recursive dynamic) |
| Inspect data | `describe_data`, `view_data` |
| Figures | `comparison_plot`, `detail_plot`, `stack_plot`, `map_heatmap`, `trend_plot`, `decomp_area_plot` |
| Tables | `report_table` (Excel), `pivot_table` (Excel pivot table) |
| Prepare data | `rename_values`, `sort_data`, `convert_units` |
| Style and script | `style_options`, `get_r_script`, `gtapviz_help` |

## Troubleshooting

- **GTAPViz does not show up in the app:** quit the app completely and
  reopen it. In Claude Desktop, check *Settings \> Developer* for the
  server status and log.
- **“There is no package called GTAPViz”:** install GTAPViz in the R
  version used by
  [`mcp_install()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_install.md),
  then run
  [`mcp_install()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_install.md)
  again.
- **Other MCP apps:**
  [`GTAPViz::mcp_config()`](https://www.pattawee-pp.com/GTAPViz/reference/mcp_config.md)
  prints the command and settings to paste.
