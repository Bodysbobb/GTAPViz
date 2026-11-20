# GTAPViz: guide for AI assistants

GTAPViz turns GTAP / CGE model results (`.sl4` solution files and `.har` header-array
files) into publication-ready figures and Excel tables. Every tool runs real GTAPViz R
code on the user's own computer and appends that code to a reproducible R script
(`get_r_script`), so the user can rerun everything later without AI.

## Workflow

1. **Load data.**
   - `find_gtap_files(folder)` shows what a folder contains (optional, cheap).
   - `load_gtap_results(folder)` loads standard (comparative-static) runs. Experiments are
     detected from file names: `EXP1.sl4` + `EXP1-WEL.har` gives experiment `EXP1` with
     `har_suffix = "-WEL"`. Use `folder = "example"` for the bundled demo data.
   - `load_gtap_dynamic(...)` loads recursive-dynamic runs (one file per period, e.g.
     `base-2020.sl4 ... pol-2030.sl4`).
2. **Inspect before plotting.** `describe_data()` lists datasets; `describe_data(data = ref)`
   shows columns, variables (with description and unit) and the values of each dimension.
   Use `view_data` to read actual numbers. Never guess numbers or column values.
3. **Visualize / tabulate** with the plot and table tools. Pass dataset references exactly
   as `describe_data` prints them, e.g. `sl4.plot.data[["REG"]]`.
4. **Check the preview images** returned by plot tools. Fix overlapping labels, crowded
   bars or unreadable text (style, export size, filter) before presenting results.
5. **Hand over.** Tell the user where files were saved. The session script
   (`gtapviz_session.R`) is saved next to the figures; `get_r_script` returns it.

## Datasets created by load_gtap_results

- `sl4.plot.data`: list of tables grouped by dimension. Typical tables: `"REG"`
  (region-level: EV, qgdp, u, tot, ppriv ...), `"COMM*REG"` (commodity x region: qxw, qo
  ...), `"ACTS*REG"`, `"COMM*REG*REG"`.
- `har.plot.data`: one table per HAR header, e.g. `"A"` (welfare decomposition; components
  in column `COLUMN`) and `"E1"` (terms-of-trade decomposition; components in `PRICES`).
- `GTAPMacro[["macros"]]`: global macro variables (pgdpwld, qgdpwld, vgdpwld ...).
- `bilateral_data[["qxs"]]`: bilateral export volumes (columns `Source`, `Destination`,
  `Commodity`).

Common columns: `Experiment`, `Variable`, `Value`, `Unit`, `Description`, `Region`,
`Commodity`, `Activity`, `Dimension`, `Subtotal`. Recursive-dynamic data also has `Case`
and `Period`.

## Choosing a plot

- `comparison_plot`: a few observations compared across experiments. Regional results:
  `data = sl4.plot.data[["REG"]], x_axis_from = "Region", split_by = "Variable"`.
  Global macro results: `data = GTAPMacro[["macros"]], x_axis_from = "Variable",
  split_by = false`.
- `detail_plot`: many sectors or regions per experiment, e.g.
  `data = sl4.plot.data[["COMM*REG"]], x_axis_from = "Commodity", split_by = "Region"`.
  `top_impact = N` keeps the N largest gains and losses. Bilateral trade:
  `split_by = ["Source", "Destination"]`.
- `stack_plot`: decompositions. Welfare: `data = har.plot.data[["A"]],
  x_axis_from = "Region", stack_value_from = "COLUMN"`. Terms of trade: `"E1"` with
  `stack_value_from = "PRICES"`. `unstack_plot = true` gives one panel per component.
- `map_heatmap`: values on a world map. Works only when the region column holds ISO3
  country codes (USA, CHN, THA ...). Aggregated GTAP regions such as "EastAsia" cannot be
  mapped; say so instead of forcing it.
- `trend_plot` and `decomp_area_plot`: time paths from `load_gtap_dynamic` (column `Period`).

## Rules of thumb

- Every unique value of `split_by` becomes a separate figure. Keep it small with
  `filter_var`, e.g. `{"Variable": ["EV", "qgdp"], "Region": ["Oceania", "SEAsia"]}`.
  A plain array filters the `Variable` column. Plot tools refuse to create more than
  `max_figures` (default 12) figures at once.
- Units: welfare `EV` is in million USD; most quantity and price variables are percentage
  changes. Use `convert_units` (`preset = "mil2bil"`) if the user wants billions.
- Styling: pass a `style` object whose keys are arguments of `create_plot_style` (or
  `create_map_style`, `create_trend_style`, `create_decomp_style`). Call `style_options` to
  see every key with its default and the available color tones. Useful keys: `color_tone`
  (e.g. "gtap", "purdue"), `panel_rows`, `show_value_labels`,
  `value_label_decimal_places`, `title_format` (`{"type": "prefix", "text": "Impact on"}`),
  `all_font_size`, `positive_color`, `negative_color`, `x_axis_text_angle`.
- Size: `export = {"width": 20, "height": 12}` (inches). Without it, the size is chosen
  automatically.
- Labels: `rename_values` renames values (e.g. Region "EastAsia" to "East Asia");
  `sort_data` sets the display order (e.g. experiments or regions).
- Files: figures are saved as PNG (plus PDF if `export_as_pdf`) in the output folder;
  tables as Excel workbooks. Loading new results replaces the datasets with the same names.
