# GTAPViz MCP TOOLS ============================================================
# Tool definitions (name, description, JSON schema) and their handlers. Each
# handler returns list(text, code, files, images, show_log); see
# .mcp_tool_result().

# SCHEMA HELPERS -----------------------------------------------------------------

#' @keywords internal
#' @noRd
.s_str <- function(description, enum = NULL) {
  p <- list(type = "string", description = description)
  if (!is.null(enum)) p$enum <- as.list(enum)
  p
}

#' @keywords internal
#' @noRd
.s_strs <- function(description) {
  list(type = "array", items = list(type = "string"), description = description)
}

#' @keywords internal
#' @noRd
.s_nums <- function(description) {
  list(type = "array", items = list(type = "number"), description = description)
}

#' @keywords internal
#' @noRd
.s_num <- function(description) list(type = "number", description = description)

#' @keywords internal
#' @noRd
.s_int <- function(description) list(type = "integer", description = description)

#' @keywords internal
#' @noRd
.s_bool <- function(description) list(type = "boolean", description = description)

#' @keywords internal
#' @noRd
.s_obj <- function(description, properties = NULL, values = NULL) {
  p <- list(type = "object", description = description)
  if (!is.null(properties)) p$properties <- properties
  p$additionalProperties <- if (!is.null(values)) values else is.null(properties)
  p
}

#' @keywords internal
#' @noRd
.mcp_tool <- function(name, title, description, properties = list(), required = NULL,
                      handler, read_only = FALSE) {
  schema <- list(
    type = "object",
    properties = if (length(properties)) properties else .mcp_empty_object()
  )
  if (length(required)) schema$required <- as.list(required)
  list(
    name = name,
    title = title,
    description = description,
    inputSchema = schema,
    annotations = list(
      title = title,
      readOnlyHint = read_only,
      destructiveHint = FALSE,
      idempotentHint = read_only,
      openWorldHint = FALSE
    ),
    handler = handler
  )
}

# Shared parameter definitions ----------------------------------------------------

#' @keywords internal
#' @noRd
.p <- function() {
  list(
    data = .s_str('Dataset reference exactly as listed by describe_data, e.g. sl4.plot.data[["REG"]].'),
    filter_var = .s_obj(
      paste0('Keep only these values, as {column: [values]}, e.g. {"Variable": ["EV", "qgdp"], ',
             '"Region": ["Oceania", "SEAsia"]}. Strongly recommended: every split_by value ',
             'becomes a separate figure.'),
      values = list(type = "array", items = list(type = "string"))
    ),
    split_by = .s_strs(paste0(
      'Column(s) whose values each get a separate figure, e.g. ["Variable"] or ',
      '["Source", "Destination"]. Use [] for one combined figure.'
    )),
    panel_var = .s_str('Column used for side-by-side panels (facets), usually "Experiment". Use "none" for no panels.'),
    invert_axis = .s_bool("Horizontal bars (categories on the vertical axis)."),
    separate_figure = .s_bool("One figure per panel_var value instead of facets."),
    top_impact = .s_int("Keep only the N largest positive and negative values per group."),
    var_name_by_description = .s_bool("Use variable descriptions instead of codes in titles."),
    add_var_info = .s_bool("Append the variable code in parentheses after the description."),
    export = .s_obj(
      "Figure file settings. Width/height in inches; automatic when omitted.",
      properties = list(
        width = .s_num("Width in inches."),
        height = .s_num("Height in inches."),
        dpi = .s_int("Resolution (default 300)."),
        file_name = .s_str("Base file name (used for merged PDFs)."),
        bg = .s_str('Background color (default "white").')
      )
    ),
    export_as_pdf = .s_str(
      'Also save PDF: "no" (default), "yes" (one PDF per figure) or "merged" (all figures in one PDF).',
      enum = c("no", "yes", "merged")
    ),
    output_folder = .s_str("Folder for the output files. Default: the session output folder."),
    max_figures = .s_int("Safety limit on the number of figures created at once (default 12)."),
    preview_images = .s_int("How many preview images to return (default 3, max 6, 0 = none)."),
    title_format = .s_obj(
      "Title format, e.g. {\"type\": \"prefix\", \"text\": \"Impact on\"}.",
      properties = list(
        type = .s_str("Title mode.", enum = c("standard", "prefix", "suffix", "full", "dynamic")),
        text = .s_str('Text to add, the full title, or a template such as "Impact on {Variable} in {Region}".'),
        sep = .s_str("Separator between text and the automatic title.")
      )
    )
  )
}

#' @keywords internal
#' @noRd
.p_style <- function(style_fun, examples) {
  .s_obj(paste0(
    "Styling options passed to ", style_fun, "(): {option: value}. ", examples,
    " Call style_options for every option, its default and the color tones."
  ))
}

# TOOL REGISTRY ------------------------------------------------------------------

#' @keywords internal
#' @noRd
.mcp_tool_registry <- function(allow_r_code = FALSE) {
  p <- .p()
  plot_common <- p[c("export", "export_as_pdf", "output_folder", "max_figures", "preview_images")]
  style_bar <- .p_style("create_plot_style", paste0(
    'E.g. {"color_tone": "purdue", "panel_rows": 2, "show_value_labels": false, ',
    '"title_format": {"type": "prefix", "text": "Impact on"}, "all_font_size": 1.2}.'
  ))

  tools <- list(
    .mcp_tool(
      "gtapviz_help", "GTAPViz guide",
      "Workflow, datasets, plot choice and styling tips for GTAPViz. Read this first if unsure.",
      handler = .mcp_h_help, read_only = TRUE
    ),
    .mcp_tool(
      "find_gtap_files", "Find GTAP files",
      paste0("List the .sl4/.har result files and mapping workbooks in a folder and detect ",
             "experiment names and file suffixes. Cheap; use before loading when unsure."),
      list(folder = .s_str("Folder to scan (a project folder or its 'in' subfolder).")),
      required = "folder", handler = .mcp_h_find_files, read_only = TRUE
    ),
    .mcp_tool(
      "load_gtap_results", "Load GTAP results",
      paste0(
        "Load comparative-static GTAP results (.sl4 and .har files) with auto_gtap_data(). ",
        "Experiments, the HAR suffix (e.g. -WEL) and an OutputMapping.xlsx in a sibling 'map' ",
        "folder are detected automatically. Creates sl4.plot.data, har.plot.data, GTAPMacro ",
        "and bilateral_data. Use folder = \"example\" for the bundled demo data."
      ),
      list(
        folder = .s_str('Folder with the .sl4/.har files, a project folder containing "in", or "example".'),
        experiments = .s_strs("Experiment names (file names without extension). Default: all found."),
        har_suffix = .s_str('Suffix of the HAR files, e.g. "-WEL". Default: detected.'),
        sl4_suffix = .s_str("Suffix of the SL4 files. Default: none."),
        sl4_variables = .s_strs(paste0('SL4 variables to load, e.g. ["qgdp", "EV", "qo"]. Default: all. ',
                                       'Add "macros" to also load the GTAP macro variables.')),
        har_headers = .s_strs('HAR headers to load, e.g. ["A", "E1"]. Default: all.'),
        load_sl4 = .s_bool("Load .sl4 files (default true)."),
        load_har = .s_bool("Load .har files (default true)."),
        mapping_file = .s_str(paste0('Excel mapping workbook (sheets SL4File, HARFile, FilterData). ',
                                     'Default "auto" (map/OutputMapping.xlsx next to the input folder); "none" to skip.')),
        mapping_info = .s_str("How descriptions and units are added.", enum = c("GTAPv7", "Mix", "Yes", "No")),
        regions = .s_strs("Keep only these regions (REG)."),
        sectors = .s_strs("Keep only these sectors (COMM/ACTS)."),
        sl4_convert_unit = .s_str("Unit conversion for SL4 data.", enum = c("mil2bil", "bil2mil", "pct2frac", "frac2pct")),
        har_convert_unit = .s_str("Unit conversion for HAR data.", enum = c("mil2bil", "bil2mil", "pct2frac", "frac2pct")),
        decimals = .s_int("Decimal places to keep (default 4)."),
        export_formats = .s_strs('Also export the processed data: any of "csv", "stata", "rds", "txt".'),
        output_folder = .s_str("Folder for figures, tables and the R script. Default: 'out' next to the input folder.")
      ),
      required = "folder", handler = .mcp_h_load_results
    ),
    .mcp_tool(
      "load_gtap_dynamic", "Load recursive-dynamic results",
      paste0(
        "Load recursive-dynamic GTAP results (one file per period) with dynamic_input_name() + ",
        "auto_gtap_rd(). File names are built as <prefix><year>, e.g. input_format = ",
        '{"base": "base-", "policy": "pol-"} with pattern "2020:2030" reads base-2020.sl4 ... pol-2030.sl4. ',
        "Use find_gtap_files first to see the names."
      ),
      list(
        folder = .s_str("Folder with the period files."),
        case_name = .s_strs('Case (scenario) name(s), e.g. ["TAR"].'),
        input_format = .s_obj(paste0(
          'Simulation types and their file-name parts, e.g. {"base": "base-", "policy": "pol-"}. ',
          'For several cases with different names: {"CaseA": {"base": "...", "policy": "..."}, ...}.'
        )),
        pattern = .s_str('Periods, e.g. "2020:2030" or "2020,2025,2030".'),
        increment = .s_int("Step between periods for a range pattern (default 1)."),
        type = .s_str('Whether the file-name part is a "prefix" (default) or "suffix" to the period.',
                      enum = c("prefix", "suffix")),
        separator = .s_str('Separator between the file-name part and the period (default "-").'),
        file_format = .s_str("File type to read (default .sl4).", enum = c(".sl4", ".har")),
        input_suffix = .s_str('Suffix after the period in the file name, e.g. "-WEL".'),
        variables = .s_strs("Variables to load. Default: all."),
        comparison = .s_str('Difference to compute, "minuend-subtrahend", e.g. "policy-base". Omit for raw values.'),
        result_format = .s_str("Period-by-period or cumulative differences.", enum = c("single", "cumulative")),
        mapping_info = .s_str("How descriptions and units are added.", enum = c("GTAPv7", "Mix", "Yes", "No")),
        regions = .s_strs("Keep only these regions."),
        sectors = .s_strs("Keep only these sectors."),
        convert_unit = .s_str("Unit conversion.", enum = c("mil2bil", "bil2mil", "pct2frac", "frac2pct")),
        output_name = .s_str('Name of the dataset to create (default "comparison.data").'),
        output_folder = .s_str("Folder for figures, tables and the R script.")
      ),
      required = c("folder", "case_name", "input_format", "pattern"), handler = .mcp_h_load_dynamic
    ),
    .mcp_tool(
      "describe_data", "Describe loaded data",
      paste0("Without `data`: list all loaded datasets with their references. With `data`: columns, ",
             "the values of every dimension, and all variables with description and unit. ",
             "Use before plotting so column names and values are exact."),
      list(data = .s_str('Optional dataset reference, e.g. sl4.plot.data[["REG"]] or a whole list such as har.plot.data.')),
      handler = .mcp_h_describe, read_only = TRUE
    ),
    .mcp_tool(
      "view_data", "View numbers",
      paste0("Show the actual numbers of a dataset as a table, filtered and optionally pivoted ",
             "(e.g. experiments side by side). Use it before describing results in words."),
      list(
        data = p$data,
        filter = .s_obj('Keep only these values, {column: [values]}, e.g. {"Variable": ["EV"]}.',
                        values = list(type = "array", items = list(type = "string"))),
        columns = .s_strs("Columns to show (default: all informative columns)."),
        wide_by = .s_str('Column to spread into separate value columns, e.g. "Experiment".'),
        sort_by = .s_str('Column to sort by (default none), e.g. "Value".'),
        descending = .s_bool("Sort descending."),
        max_rows = .s_int("Maximum rows to return (default 50, max 500).")
      ),
      required = "data", handler = .mcp_h_view, read_only = TRUE
    ),
    .mcp_tool(
      "comparison_plot", "Comparison plot",
      paste0(
        "Bar chart comparing a few observations across experiments (comparison_plot). Typical: ",
        'regional results data = sl4.plot.data[["REG"]], x_axis_from = "Region", split_by = ["Variable"] ',
        '(one figure per variable); global macros data = GTAPMacro[["macros"]], x_axis_from = "Variable", ',
        "split_by = []. Saves PNG files and returns previews."
      ),
      c(list(
        data = p$data,
        x_axis_from = .s_str('Column for the bars, e.g. "Region" or "Variable".'),
        filter_var = p$filter_var, split_by = p$split_by, panel_var = p$panel_var,
        invert_axis = p$invert_axis, separate_figure = p$separate_figure,
        var_name_by_description = p$var_name_by_description, add_var_info = p$add_var_info,
        style = style_bar
      ), plot_common),
      required = c("data", "x_axis_from"), handler = .mcp_plot_handler("comparison_plot")
    ),
    .mcp_tool(
      "detail_plot", "Detail plot",
      paste0(
        "Bar chart for many sectors or regions per experiment (detail_plot). Typical: ",
        'data = sl4.plot.data[["COMM*REG"]], x_axis_from = "Commodity", split_by = ["Region"], ',
        'filter_var = {"Variable": ["qo"], "Region": ["SEAsia"]}. top_impact = 10 keeps the largest ',
        'changes. Bilateral trade: data = bilateral_data[["qxs"]], split_by = ["Source", "Destination"].'
      ),
      c(list(
        data = p$data,
        x_axis_from = .s_str('Column for the bars, e.g. "Commodity", "Activity" or "Region".'),
        filter_var = p$filter_var, split_by = p$split_by, panel_var = p$panel_var,
        invert_axis = .s_bool("Horizontal bars (default true for detail plots)."),
        separate_figure = p$separate_figure, top_impact = p$top_impact,
        var_name_by_description = p$var_name_by_description, add_var_info = p$add_var_info,
        style = style_bar
      ), plot_common),
      required = c("data", "x_axis_from"), handler = .mcp_plot_handler("detail_plot")
    ),
    .mcp_tool(
      "stack_plot", "Stacked decomposition plot",
      paste0(
        "Stacked bars showing how components add up to a total (stack_plot). Welfare decomposition: ",
        'data = har.plot.data[["A"]], x_axis_from = "Region", stack_value_from = "COLUMN". ',
        'Terms of trade: data = har.plot.data[["E1"]], x_axis_from = "Commodity", stack_value_from = "PRICES", ',
        'split_by = ["Region"]. unstack_plot = true shows each component in its own panel. ',
        "Use rename_values first for readable component names."
      ),
      c(list(
        data = p$data,
        x_axis_from = .s_str('Column for the bars, e.g. "Region".'),
        stack_value_from = .s_str('Column holding the components, e.g. "COLUMN" or "PRICES".'),
        filter_var = p$filter_var, split_by = p$split_by, panel_var = p$panel_var,
        invert_axis = p$invert_axis, separate_figure = p$separate_figure,
        show_total = .s_bool("Show the total (default true)."),
        unstack_plot = .s_bool("One panel per component instead of stacking."),
        top_impact = p$top_impact,
        var_name_by_description = p$var_name_by_description, add_var_info = p$add_var_info,
        style = style_bar
      ), plot_common),
      required = c("data", "x_axis_from", "stack_value_from"), handler = .mcp_plot_handler("stack_plot")
    ),
    .mcp_tool(
      "map_heatmap", "Map",
      paste0(
        "World or regional map colored by a variable (map_heatmap). Needs ISO3 country codes ",
        "(USA, CHN, THA ...) in the region column; aggregated regions such as 'EastAsia' cannot be ",
        "mapped. Filter to one variable or use map_value_var; label_box_vars adds value boxes."
      ),
      c(list(
        data = p$data,
        filter_var = p$filter_var,
        iso_col = .s_str('Column with ISO3 codes (default "Region" when present).'),
        value_col = .s_str('Column with values (default "Value").'),
        split_by = .s_strs('Column(s) giving one map per value, e.g. ["Experiment"].'),
        map_region = .s_strs(paste0(
          'Area to show: ["world"] (default), a region name such as ["asean"], ["europe"], ["asia"], ',
          '["eu"], ["latin_america"], ["sub_saharan_africa"], or ISO3 codes ["USA", "CAN", "MEX"].'
        )),
        map_value_var = .s_str("Variable used for the colors when several are loaded."),
        label_box_vars = list(
          type = "array",
          description = 'Value boxes next to countries, e.g. [{"variable": "EV", "label": "Welfare", "decimal_places": 1}].',
          items = list(type = "object", properties = list(
            variable = .s_str("Variable code."),
            label = .s_str("Label shown in the box."),
            decimal_places = .s_int("Decimal places.")
          ), required = list("variable"))
        ),
        show_flags = .s_bool("Show country flags next to the label boxes."),
        common_scale = .s_bool("Same color scale for all maps."),
        aggregate_by = .s_strs("Columns to aggregate by before mapping."),
        aggregate_fun = .s_str("Aggregation function.", enum = c("mean", "sum", "median")),
        xlim = .s_nums("Longitude limits [min, max]."),
        ylim = .s_nums("Latitude limits [min, max]."),
        var_name_by_description = p$var_name_by_description, add_var_info = p$add_var_info,
        title_format = p$title_format,
        style = .p_style("create_map_style", paste0(
          'E.g. {"color_low": "#D9B17D", "color_high": "#1C3E5D", "color_mid": "white", ',
          '"color_palette": "viridis", "legend_position": "bottom"}.'
        ))
      ), plot_common),
      required = "data", handler = .mcp_plot_handler("map_heatmap")
    ),
    .mcp_tool(
      "trend_plot", "Trend plot",
      paste0(
        "Line chart over time for recursive-dynamic data (trend_plot; needs a Period column, ",
        'e.g. data = comparison.data[["qgdp"]]). line_group_by sets the lines, e.g. ["Region"] or ["Case"].'
      ),
      c(list(
        data = p$data,
        filter_var = p$filter_var,
        period_col = .s_str('Column with periods (default "Period").'),
        split_by = .s_strs('Column(s) giving one figure per value, e.g. ["Variable"].'),
        line_group_by = .s_strs('Column(s) defining the lines (default ["Case"]).'),
        panel_var = .s_str('Column for panels, e.g. "Region". Default: none.'),
        separate_figure = p$separate_figure,
        var_name_by_description = p$var_name_by_description, add_var_info = p$add_var_info,
        title_format = p$title_format,
        style = .p_style("create_trend_style", paste0(
          'E.g. {"color_palette": "gtap", "show_points": false, "smooth_line": true, ',
          '"vertical_lines": [{"period": 2025, "label": "Policy starts"}]}.'
        ))
      ), plot_common),
      required = "data", handler = .mcp_plot_handler("trend_plot")
    ),
    .mcp_tool(
      "decomp_area_plot", "Decomposition area plot",
      paste0(
        "Stacked area chart of components over time for recursive-dynamic data (decomp_area_plot). ",
        "Needs a Period column and a component column."
      ),
      c(list(
        data = p$data,
        component_col = .s_str('Column holding the components, e.g. "COLUMN".'),
        filter_var = p$filter_var,
        period_col = .s_str('Column with periods (default "Period").'),
        split_by = .s_strs("Column(s) giving one figure per value."),
        panel_var = .s_str('Column for panels (default "Experiment"; use "none" for none).'),
        separate_figure = p$separate_figure,
        show_total_line = .s_bool("Overlay the total (default true)."),
        normalize = .s_bool("Show percentage shares instead of values."),
        var_name_by_description = p$var_name_by_description, add_var_info = p$add_var_info,
        title_format = p$title_format,
        style = .p_style("create_decomp_style", 'E.g. {"color_palette": "gtap", "area_alpha": 0.8}.')
      ), plot_common),
      required = c("data", "component_col"), handler = .mcp_plot_handler("decomp_area_plot")
    ),
    .mcp_tool(
      "report_table", "Report table (Excel)",
      paste0(
        "Publication-style wide tables saved to Excel (report_table). Example: data = ",
        '"sl4.plot.data", tables = ["REG"], pivot_col = "Variable", group_by = ["Experiment", "Region"]. ',
        'Decomposition: data = "har.plot.data", tables = ["A"], pivot_col = "COLUMN", total_column = true. ',
        "Returns a preview of the table."
      ),
      list(
        data = .s_str('A data list such as "sl4.plot.data", or one table reference.'),
        tables = .s_strs('Tables of the list to include, e.g. ["REG", "COMM*REG"]. Default: all.'),
        pivot_col = .s_str('Column spread into table columns, e.g. "Variable", "Commodity", "COLUMN" (same for every table).'),
        pivot_col_by_table = .s_obj('Different pivot column per table, e.g. {"REG": "Variable", "COMM*REG": "Commodity"}.'),
        group_by = .s_strs('Row grouping columns in order, e.g. ["Experiment", "Region"] (same for every table).'),
        group_by_table = .s_obj('Different grouping per table, e.g. {"A": ["Experiment", "Region"]}.'),
        rename_cols = .s_obj('Rename columns in the output, e.g. {"Experiment": "Scenario"}.'),
        total_column = .s_bool("Add a Total column (useful for decompositions)."),
        decimal = .s_int("Decimal places (default 2)."),
        include_units = .s_bool("Include units."),
        var_name_by_description = .s_bool("Use variable descriptions instead of codes (default true)."),
        add_var_info = .s_bool("Append variable codes to descriptions."),
        separate_sheet_by = .s_str('Column that splits the output into sheets, e.g. "Unit" or "Region".'),
        unit_select = .s_str("Keep only this unit."),
        component_exclude = .s_strs("Pivoted values to drop."),
        subtotal_level = .s_bool("Include subtotal rows."),
        repeat_label = .s_bool("Repeat group labels on every row."),
        add_group_line = .s_bool("Add a line after each group."),
        separate_file = .s_bool("One Excel file per table."),
        workbook_name = .s_str('Excel file name without extension (default "GTAPViz_report").'),
        output_folder = p$output_folder
      ),
      required = "data", handler = .mcp_h_report_table
    ),
    .mcp_tool(
      "pivot_table", "Excel pivot table",
      paste0("Interactive Excel pivot table with filters, plus the raw data (pivot_table_with_filter). ",
             "Good for sharing with Excel users."),
      list(
        data = p$data,
        rows = .s_strs('Row fields, e.g. ["Region", "Commodity"].'),
        cols = .s_strs('Column fields, e.g. ["Experiment"].'),
        filter = .s_strs('Filter fields, e.g. ["Variable", "Unit"].'),
        data_fields = .s_str('Value field (default "Value").'),
        workbook_name = .s_str('Excel file name (default "GTAPViz_pivot.xlsx").'),
        output_folder = p$output_folder
      ),
      required = "data", handler = .mcp_h_pivot_table
    ),
    .mcp_tool(
      "rename_values", "Rename values",
      paste0("Rename values in a column for nicer labels (rename_value), e.g. column = \"Region\", ",
             "mapping = {\"EastAsia\": \"East Asia\"}, or welfare components in COLUMN. ",
             "Applies to every loaded dataset with that column unless `data` is given."),
      list(
        column = .s_str('Column to change, e.g. "Region", "Experiment", "COLUMN".'),
        mapping = .s_obj('Old value -> new value, e.g. {"EXP1": "Tariff cut", "EXP2": "Subsidy"}.',
                         values = list(type = "string")),
        data = .s_strs("Dataset references to change (default: all with the column).")
      ),
      required = c("column", "mapping"), handler = .mcp_h_rename
    ),
    .mcp_tool(
      "sort_data", "Set display order",
      paste0("Set the order of experiments, regions, sectors, ... in figures and tables (sort_plot_data), ",
             'e.g. order = {"Experiment": ["EXP2", "EXP1"], "Region": ["SEAsia", "Oceania"]}. ',
             "Values not listed follow in their current order."),
      list(
        order = .s_obj("Column -> values in the desired order.",
                       values = list(type = "array", items = list(type = "string"))),
        sort_by_value = .s_str("Then sort by Value.", enum = c("descending", "ascending")),
        data = .s_strs("Dataset references to change (default: all).")
      ),
      handler = .mcp_h_sort
    ),
    .mcp_tool(
      "convert_units", "Convert units",
      paste0("Convert values and unit labels (convert_units), either with a preset (mil2bil: million ",
             "USD to billion USD, bil2mil, pct2frac, frac2pct) or a custom rule such as from ",
             '["million USD"], to ["billion USD"], adjustment ["/1000"].'),
      list(
        preset = .s_str("Predefined conversion.", enum = c("mil2bil", "bil2mil", "pct2frac", "frac2pct")),
        from = .s_strs("Unit labels to convert."),
        to = .s_strs("New unit labels (same length as from)."),
        adjustment = .s_strs('Operation per unit, e.g. ["/1000"] or ["*100"].'),
        variables = .s_strs("Only convert these variables."),
        data = .s_strs("Dataset references to change (default: all).")
      ),
      handler = .mcp_h_convert
    ),
    .mcp_tool(
      "style_options", "Style options",
      "All options of a style function with defaults and descriptions, or the available color tones.",
      list(kind = .s_str("Which options to list.",
                         enum = c("plot", "map", "trend", "decomp", "export", "title", "color_tones"))),
      required = "kind", handler = .mcp_h_style_options, read_only = TRUE
    ),
    .mcp_tool(
      "get_r_script", "Get the R script",
      paste0("Return the reproducible R script of everything done in this session. It is also ",
             "saved automatically as gtapviz_session.R in the output folder."),
      list(save_as = .s_str("Optional extra path to save a copy, e.g. D:/project/figures.R.")),
      handler = .mcp_h_script, read_only = TRUE
    )
  )

  if (isTRUE(allow_r_code)) {
    tools[[length(tools) + 1L]] <- .mcp_tool(
      "run_r_code", "Run R code",
      paste0("Run R code in the GTAPViz session (datasets such as sl4.plot.data are available). ",
             "For custom data manipulation the other tools cannot do. The code is added to the ",
             "session script. Printed output is returned."),
      list(
        code = .s_str("R code to run."),
        title = .s_str("Short description for the script comment.")
      ),
      required = "code", handler = .mcp_h_run_r
    )
  }

  stats::setNames(tools, vapply(tools, `[[`, "", "name"))
}

# PROMPTS ------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.mcp_prompt_list <- function() {
  list(
    list(
      name = "try_example", title = "Try GTAPViz with example data",
      description = "Load the bundled example results and make a few figures.",
      arguments = list()
    ),
    list(
      name = "analyze_results", title = "Analyze my GTAP results",
      description = "Load your results and create the key figures for a policy report.",
      arguments = list(list(name = "folder", description = "Folder with your .sl4/.har files", required = TRUE))
    )
  )
}

#' @keywords internal
#' @noRd
.mcp_prompt_get <- function(params) {
  name <- params$name
  folder <- params$arguments$folder
  text <- switch(
    if (is.null(name)) "" else name,
    try_example = paste(
      "Use the GTAPViz tools to load the example data (folder = \"example\").",
      "Then: (1) describe the experiments and regions, (2) make a comparison plot of welfare (EV)",
      "and real GDP (qgdp) by region, (3) make a welfare decomposition stack plot, and (4) summarize",
      "the main results in a short paragraph using the actual numbers. Finally tell me where the",
      "files and the reproducible R script were saved."
    ),
    analyze_results = paste0(
      "Use the GTAPViz tools to load the GTAP results in \"", if (is.null(folder)) "" else folder, "\". ",
      "Describe the experiments, regions and sectors. Then create the key figures for a policy report: ",
      "welfare (EV), real GDP (qgdp) and terms of trade (tot) by region, the welfare decomposition, and ",
      "the sectors with the largest output changes. Summarize the findings using the actual numbers and ",
      "tell me where the files and the reproducible R script were saved."
    ),
    stop(.mcp_rpc_condition(-32602L, paste0("Unknown prompt: ", format(name))))
  )
  list(
    description = "GTAPViz prompt",
    messages = list(list(role = "user", content = list(type = "text", text = text)))
  )
}

# HANDLERS: HELP, FILES, LOADING -------------------------------------------------

#' @keywords internal
#' @noRd
.mcp_h_help <- function(srv, args) {
  list(text = c(.mcp_guide_text(), "", .mcp_overview(srv)))
}

#' Resolve a user folder: "example", a folder with result files, or a project
#' folder containing an "in" subfolder.
#' @keywords internal
#' @noRd
.mcp_resolve_input <- function(folder) {
  folder <- .mcp_chr(folder)
  if (!length(folder) || !nzchar(folder[1])) stop("Please give a folder.", call. = FALSE)
  folder <- folder[1]
  if (tolower(folder) %in% c("example", "examples", "demo", "sample", "sample data", "example data")) {
    return(list(
      dir = .mcp_path(system.file("extdata", "in", package = "GTAPViz")),
      expr = quote(system.file("extdata/in", package = "GTAPViz")),
      example = TRUE, note = NULL
    ))
  }
  dir <- .mcp_path(folder)
  if (!dir.exists(dir)) {
    if (file.exists(dir)) dir <- dirname(dir) else stop("Folder not found: ", dir, call. = FALSE)
  }
  note <- NULL
  has_results <- function(d) any(grepl("\\.(sl4|har)$", list.files(d), ignore.case = TRUE))
  if (!has_results(dir)) {
    sub <- file.path(dir, "in")
    if (dir.exists(sub) && has_results(sub)) {
      note <- sprintf("Using the 'in' subfolder: %s", .mcp_path(sub))
      dir <- .mcp_path(sub)
    }
  }
  list(dir = dir, expr = dir, example = FALSE, note = note)
}

#' Detect experiments and the HAR suffix from file names.
#' @keywords internal
#' @noRd
.mcp_detect_experiments <- function(files) {
  sl4 <- sub("\\.sl4$", "", files[grepl("\\.sl4$", files, ignore.case = TRUE)], ignore.case = TRUE)
  har <- sub("\\.har$", "", files[grepl("\\.har$", files, ignore.case = TRUE)], ignore.case = TRUE)
  suffixes <- character()
  for (h in har) {
    cand <- sl4[startsWith(tolower(h), tolower(sl4))]
    if (length(cand)) {
      e <- cand[which.max(nchar(cand))]
      suffixes <- c(suffixes, substring(h, nchar(e) + 1L))
    }
  }
  har_suffix <- ""
  if (length(suffixes)) {
    tab <- sort(table(suffixes), decreasing = TRUE)
    har_suffix <- if ("-WEL" %in% names(tab)) "-WEL" else names(tab)[1]
  }
  experiments <- if (length(sl4)) sl4 else har
  years <- grepl("(19|20)[0-9]{2}", experiments)
  list(experiments = experiments, sl4 = sl4, har = har, har_suffix = har_suffix,
       har_suffixes = unique(suffixes), dynamic = sum(years) >= 4L)
}

#' @keywords internal
#' @noRd
.mcp_find_mapping <- function(input_dir) {
  candidates <- c(
    file.path(dirname(input_dir), "map", "OutputMapping.xlsx"),
    file.path(input_dir, "OutputMapping.xlsx"),
    file.path(dirname(input_dir), "OutputMapping.xlsx")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) .mcp_path(hit[1]) else NULL
}

#' @keywords internal
#' @noRd
.mcp_h_find_files <- function(srv, args) {
  input <- .mcp_resolve_input(args$folder)
  files <- list.files(input$dir)
  det <- .mcp_detect_experiments(files)
  xlsx <- files[grepl("\\.xlsx?$", files, ignore.case = TRUE)]
  mapping <- if (input$example) NULL else .mcp_find_mapping(input$dir)
  lines <- c(
    sprintf("Folder: %s", input$dir), input$note,
    sprintf(".sl4 files (%d): %s", length(det$sl4), if (length(det$sl4)) .mcp_values_text(det$sl4, 60) else "none"),
    sprintf(".har files (%d): %s", length(det$har), if (length(det$har)) .mcp_values_text(det$har, 60) else "none"),
    if (length(xlsx)) sprintf("Excel files: %s", paste(xlsx, collapse = ", ")),
    sprintf("Mapping workbook: %s", if (is.null(mapping)) "none found (GTAPv7 defaults will be used)" else mapping),
    "",
    sprintf("Detected experiments: %s", if (length(det$experiments)) .mcp_values_text(det$experiments, 60) else "none"),
    sprintf("Detected HAR suffix: %s", if (nzchar(det$har_suffix)) shQuote(det$har_suffix, "cmd") else "none"),
    if (length(det$har_suffixes) > 1) sprintf("Other HAR suffixes present: %s", paste(det$har_suffixes, collapse = ", "))
  )
  if (det$dynamic) {
    lines <- c(lines, "", paste(
      "File names contain years: this looks like recursive-dynamic output.",
      "Use load_gtap_dynamic with the file-name parts before the year as input_format."
    ))
  } else if (length(det$experiments)) {
    lines <- c(lines, "", "Next: load_gtap_results with this folder (experiments and suffix are detected automatically).")
  }
  list(text = lines[!is.na(lines) & !is.null(lines)])
}

#' Output folder for a load call.
#' @keywords internal
#' @noRd
.mcp_pick_output_dir <- function(srv, args, input) {
  chosen <- .mcp_arg(args, "output_folder")
  if (!is.null(chosen)) return(.mcp_path(chosen))
  if (!is.null(srv$output_dir)) return(srv$output_dir)
  if (input$example) return(.mcp_default_output_dir())
  if (tolower(basename(input$dir)) == "in") return(.mcp_path(file.path(dirname(input$dir), "out")))
  .mcp_path(file.path(input$dir, "GTAPViz-output"))
}

#' @keywords internal
#' @noRd
.mcp_h_load_results <- function(srv, args) {
  input <- .mcp_resolve_input(args$folder)
  files <- list.files(input$dir)
  det <- .mcp_detect_experiments(files)
  if (!length(det$experiments)) {
    stop("No .sl4 or .har files found in ", input$dir, ". Files there: ",
         if (length(files)) .mcp_values_text(files, 30) else "(empty)", call. = FALSE)
  }

  experiments <- .mcp_arg(args, "experiments", "strings", det$experiments)
  har_suffix <- .mcp_arg(args, "har_suffix", "string", det$har_suffix)
  har_bases <- det$har
  if (nzchar(har_suffix)) {
    has_sfx <- endsWith(tolower(har_bases), tolower(har_suffix))
    har_bases[has_sfx] <- substr(har_bases[has_sfx], 1L, nchar(har_bases[has_sfx]) - nchar(har_suffix))
  }
  unknown <- experiments[!tolower(experiments) %in% tolower(c(det$sl4, har_bases))]
  if (length(unknown)) {
    stop("Experiment(s) not found: ", paste(unknown, collapse = ", "),
         ". Detected experiments: ", .mcp_values_text(det$experiments, 40), call. = FALSE)
  }
  sl4_suffix <- .mcp_arg(args, "sl4_suffix", "string", "")
  load_sl4 <- .mcp_arg(args, "load_sl4", "boolean", length(det$sl4) > 0)
  load_har <- .mcp_arg(args, "load_har", "boolean", length(det$har) > 0)
  if (!load_sl4 && !load_har) stop("Nothing to load: both load_sl4 and load_har are false.", call. = FALSE)

  # Mapping workbook
  mapping_arg <- .mcp_arg(args, "mapping_file", "string", "auto")
  mapping_file <- if (tolower(mapping_arg) %in% c("none", "no", "")) {
    NULL
  } else if (tolower(mapping_arg) == "auto") {
    if (input$example) NULL else .mcp_find_mapping(input$dir)
  } else {
    if (!file.exists(mapping_arg)) stop("Mapping file not found: ", mapping_arg, call. = FALSE)
    .mcp_path(mapping_arg)
  }
  sheets <- if (!is.null(mapping_file)) openxlsx::getSheetNames(mapping_file) else character()
  mapping_info <- .mcp_arg(args, "mapping_info", "string", if (length(sheets)) "Mix" else "GTAPv7")

  regions <- .mcp_arg(args, "regions", "strings")
  sectors <- .mcp_arg(args, "sectors", "strings")
  notes <- c(input$note)
  if ("FilterData" %in% sheets && is.null(regions) && is.null(sectors)) {
    fd <- openxlsx::read.xlsx(mapping_file, sheet = "FilterData")
    pick <- function(col) {
      if (is.null(fd) || !col %in% names(fd)) return(NULL)
      v <- trimws(as.character(fd[[col]]))
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v)) v else NULL
    }
    regions <- pick("Region")
    sectors <- pick("Sector")
    if (!is.null(regions) || !is.null(sectors)) {
      notes <- c(notes, "Region/sector filters were taken from the FilterData sheet of the mapping workbook.")
    }
  }

  output_dir <- .mcp_pick_output_dir(srv, args, input)

  # Build the code
  exprs <- list(
    call("<-", as.name("input_path"), input$expr),
    call("<-", as.name("output_path"), output_dir)
  )
  call_args <- list(experiment = experiments, input_path = as.name("input_path"))
  if (nzchar(sl4_suffix)) call_args$sl4_suffix <- sl4_suffix
  if (nzchar(har_suffix)) call_args$har_suffix <- har_suffix
  if (load_sl4) {
    vars <- .mcp_arg(args, "sl4_variables", "strings")
    if (!is.null(vars)) call_args$process_sl4_vars <- vars
  } else {
    call_args$process_sl4_vars <- FALSE
  }
  if (load_har) {
    heads <- .mcp_arg(args, "har_headers", "strings")
    if (!is.null(heads)) call_args$process_har_vars <- heads
  } else {
    call_args$process_har_vars <- FALSE
  }
  call_args$mapping_info <- mapping_info
  if (!is.null(mapping_file)) {
    exprs <- c(exprs, list(call("<-", as.name("mapping_file"), mapping_file)))
    # As in the package workflow, the mapping sheets also choose the variables
    # to extract unless the AI named them explicitly.
    if ("SL4File" %in% sheets) {
      exprs <- c(exprs, list(call("<-", as.name("sl4map"), quote(openxlsx::read.xlsx(mapping_file, sheet = "SL4File")))))
      call_args$sl4_mapping_info <- as.name("sl4map")
      if (load_sl4 && is.null(call_args$process_sl4_vars)) {
        call_args$process_sl4_vars <- as.name("sl4map")
        notes <- c(notes, "SL4 variables were taken from the SL4File sheet (use sl4_variables or mapping_file = \"none\" to change).")
      }
    }
    if ("HARFile" %in% sheets) {
      exprs <- c(exprs, list(call("<-", as.name("harmap"), quote(openxlsx::read.xlsx(mapping_file, sheet = "HARFile")))))
      call_args$har_mapping_info <- as.name("harmap")
      if (load_har && is.null(call_args$process_har_vars)) {
        call_args$process_har_vars <- as.name("harmap")
        notes <- c(notes, "HAR headers were taken from the HARFile sheet.")
      }
    }
  }
  if (!is.null(args$sl4_convert_unit)) call_args$sl4_convert_unit <- .mcp_arg(args, "sl4_convert_unit")
  if (!is.null(args$har_convert_unit)) call_args$har_convert_unit <- .mcp_arg(args, "har_convert_unit")
  if (!is.null(args$decimals)) call_args$decimals <- .mcp_arg(args, "decimals", "integer")
  if (!is.null(regions)) call_args$region_select <- regions
  if (!is.null(sectors)) call_args$sector_select <- sectors
  formats <- .mcp_arg(args, "export_formats", "strings")
  if (length(formats)) {
    call_args$output_path <- as.name("output_path")
    call_args$output_formats <- tolower(formats)
  }
  call_args$plot_data <- TRUE
  call_args <- call_args[order(match(names(call_args), names(formals(auto_gtap_data))))]
  exprs <- c(exprs, list(.mcp_call("auto_gtap_data", call_args)))

  before <- .mcp_snapshot(output_dir)
  res <- .mcp_eval(srv, exprs, sprintf("Load GTAP results (%s)", paste(experiments, collapse = ", ")))

  created <- intersect(c("sl4.plot.data", "har.plot.data", "GTAPMacro", "bilateral_data"), ls(srv$ws))
  if (!length(created)) {
    stop("auto_gtap_data() finished but produced no datasets. Check the file names and suffixes.", call. = FALSE)
  }
  files <- if (length(formats)) .mcp_changed_files(before, output_dir) else character()
  list(
    text = c(
      sprintf("Loaded %d experiment%s: %s", length(experiments), if (length(experiments) == 1) "" else "s",
              paste(experiments, collapse = ", ")),
      sprintf("Input folder: %s", input$dir),
      sprintf("Output folder (figures, tables, gtapviz_session.R): %s", output_dir),
      if (!is.null(mapping_file)) sprintf("Mapping workbook: %s (mapping_info = \"%s\")", mapping_file, mapping_info),
      notes, "", .mcp_overview(srv), "",
      "Next: describe_data(data = ...) for columns, values and variable descriptions."
    ),
    code = res$code, files = files, show_log = TRUE
  )
}

#' @keywords internal
#' @noRd
.mcp_h_load_dynamic <- function(srv, args) {
  input <- .mcp_resolve_input(args$folder)
  cases <- .mcp_arg(args, "case_name", "strings")
  if (!length(cases)) stop("case_name is required, e.g. [\"TAR\"].", call. = FALSE)
  fmt <- args$input_format
  if (!is.list(fmt) || is.null(names(fmt))) {
    stop('input_format must be an object such as {"base": "base-", "policy": "pol-"}.', call. = FALSE)
  }
  pattern_raw <- .mcp_chr(args$pattern)
  pattern <- if (length(pattern_raw) == 1L && grepl(",", pattern_raw)) {
    as.numeric(trimws(strsplit(pattern_raw, ",")[[1]]))
  } else if (length(pattern_raw) > 1L) {
    as.numeric(pattern_raw)
  } else {
    pattern_raw
  }
  file_format <- .mcp_arg(args, "file_format", "string", ".sl4")
  input_suffix <- .mcp_arg(args, "input_suffix", "string", "")
  output_name <- .mcp_arg(args, "output_name", "string", "comparison.data")
  if (!identical(make.names(output_name), output_name)) stop("output_name must be a valid R name.", call. = FALSE)

  dyn_args <- list(
    case_name = cases,
    type = .mcp_arg(args, "type", "string", "prefix"),
    input_format = .mcp_value(fmt),
    pattern = pattern
  )
  if (!is.null(args$increment)) dyn_args$increment <- .mcp_arg(args, "increment", "integer")
  if (!is.null(args$separator)) dyn_args$separator <- .mcp_arg(args, "separator")
  dyn_call <- call("<-", as.name("input_map"), .mcp_call("dynamic_input_name", dyn_args))

  # Check the file names before loading
  probe <- new.env(parent = srv$ws)
  eval(dyn_call, envir = probe)
  input_map <- get("input_map", envir = probe)
  expected <- paste0(input_map$Input, input_suffix, file_format)
  found <- tolower(expected) %in% tolower(list.files(input$dir))
  if (!any(found)) {
    stop("None of the expected files exist, e.g. ", paste(utils::head(expected, 4), collapse = ", "),
         ". Files in the folder: ", .mcp_values_text(list.files(input$dir), 20),
         ". Adjust input_format, type, separator or pattern.", call. = FALSE)
  }
  notes <- c(input$note)
  if (!all(found)) {
    notes <- c(notes, sprintf("Missing files (skipped): %s", .mcp_values_text(expected[!found], 15)))
  }

  rd_args <- list(input_detail = as.name("input_map"), input_path = as.name("input_path"))
  if (file_format != ".sl4") rd_args$input_format <- file_format
  if (nzchar(input_suffix)) rd_args$input_suffix <- input_suffix
  vars <- .mcp_arg(args, "variables", "strings")
  if (!is.null(vars)) rd_args$process_vars <- vars
  comparison <- .mcp_arg(args, "comparison")
  rd_args["comparison"] <- list(comparison)
  if (!is.null(comparison)) rd_args$result_format <- .mcp_arg(args, "result_format", "string", "single")
  if (!is.null(args$mapping_info)) rd_args$mapping_info <- .mcp_arg(args, "mapping_info")
  if (!is.null(args$regions)) rd_args$region_select <- .mcp_arg(args, "regions", "strings")
  if (!is.null(args$sectors)) rd_args$sector_select <- .mcp_arg(args, "sectors", "strings")
  if (!is.null(args$convert_unit)) rd_args$convert_unit <- .mcp_arg(args, "convert_unit")
  rd_args$main_output_name <- output_name

  output_dir <- .mcp_pick_output_dir(srv, args, input)
  exprs <- list(
    call("<-", as.name("input_path"), input$expr),
    call("<-", as.name("output_path"), output_dir),
    dyn_call,
    .mcp_call("auto_gtap_rd", rd_args)
  )
  res <- .mcp_eval(srv, exprs, sprintf("Load recursive-dynamic results (%s)", paste(cases, collapse = ", ")))
  if (!exists(output_name, envir = srv$ws, inherits = FALSE)) {
    stop("auto_gtap_rd() produced no data. Check variables and file names.", call. = FALSE)
  }
  list(
    text = c(
      sprintf("Loaded %d period files for case(s) %s.", sum(found), paste(cases, collapse = ", ")),
      sprintf("Output folder: %s", output_dir), notes, "", .mcp_overview(srv)
    ),
    code = res$code, show_log = TRUE
  )
}

# HANDLERS: INSPECTION -----------------------------------------------------------

#' @keywords internal
#' @noRd
.mcp_h_describe <- function(srv, args) {
  if (is.null(args$data) || !nzchar(.mcp_chr(args$data)[1])) return(list(text = .mcp_overview(srv)))
  ref <- .mcp_ref(srv, args$data, want = "any")
  if (is.data.frame(ref$value)) {
    text <- .mcp_describe_table(ref$label, ref$value)
    if ("Period" %in% names(ref$value)) {
      text <- c(text, "", "Has a Period column: use trend_plot / decomp_area_plot.")
    }
    return(list(text = text))
  }
  index <- Filter(function(d) d$object == ref$object, .mcp_dataset_index(srv))
  list(text = vapply(index, function(d) .mcp_describe_table(d$ref, d$df, max_values = 15L, max_vars = 25L),
                     character(1)))
}

#' @keywords internal
#' @noRd
.mcp_h_view <- function(srv, args) {
  ref <- .mcp_ref(srv, args$data, want = "table")
  df <- .mcp_apply_filter(ref$value, .mcp_filter_value(args$filter))
  noise <- c("Subtotal", "Dimension", "Description")
  wide_by <- .mcp_arg(args, "wide_by")
  cols <- .mcp_arg(args, "columns", "strings")
  if (is.null(cols)) cols <- setdiff(names(df), noise)
  .mcp_check_columns(df, list(columns = cols, wide_by = wide_by, sort_by = .mcp_arg(args, "sort_by")),
                     c("columns", "wide_by", "sort_by"))
  if (!is.null(wide_by)) {
    if (!"Value" %in% names(df)) stop("wide_by needs a Value column.", call. = FALSE)
    id_cols <- setdiff(cols, c(wide_by, "Value"))
    df <- as.data.frame(tidyr::pivot_wider(
      df[unique(c(id_cols, wide_by, "Value"))],
      names_from = tidyr::all_of(wide_by), values_from = "Value", values_fn = function(x) x[1]
    ))
  } else {
    df <- df[cols]
  }
  sort_by <- .mcp_arg(args, "sort_by")
  if (!is.null(sort_by) && sort_by %in% names(df)) {
    df <- df[order(df[[sort_by]], decreasing = isTRUE(.mcp_arg(args, "descending", "boolean", FALSE))), , drop = FALSE]
  }
  max_rows <- min(max(.mcp_arg(args, "max_rows", "integer", 50), 1), 500)
  total <- nrow(df)
  list(text = c(
    sprintf("%s: %d row%s%s", ref$label, total, if (total == 1) "" else "s",
            if (total > max_rows) sprintf(" (showing the first %d)", max_rows) else ""),
    "",
    .mcp_md_table(utils::head(df, max_rows))
  ))
}

# HANDLERS: PLOTS ----------------------------------------------------------------

#' Argument types of each plot function exposed to the AI.
#' @keywords internal
#' @noRd
.mcp_plot_specs <- function(fun) {
  common <- list(filter_var = "filter", var_name_by_description = "boolean", add_var_info = "boolean")
  switch(
    fun,
    comparison_plot = c(list(x_axis_from = "string", split_by = "split", panel_var = "nullable_string",
                             invert_axis = "boolean", separate_figure = "boolean"), common),
    detail_plot = c(list(x_axis_from = "string", split_by = "split", panel_var = "nullable_string",
                         invert_axis = "boolean", separate_figure = "boolean", top_impact = "integer"), common),
    stack_plot = c(list(x_axis_from = "string", stack_value_from = "string", split_by = "split",
                        panel_var = "nullable_string", invert_axis = "boolean", separate_figure = "boolean",
                        show_total = "boolean", unstack_plot = "boolean", top_impact = "integer"), common),
    map_heatmap = c(list(iso_col = "string", value_col = "string", split_by = "split",
                         map_region = "strings", map_value_var = "string", show_flags = "boolean",
                         common_scale = "boolean", aggregate_by = "strings", aggregate_fun = "string",
                         xlim = "numbers", ylim = "numbers"), common),
    trend_plot = c(list(period_col = "string", split_by = "split", line_group_by = "strings",
                        panel_var = "nullable_string", separate_figure = "boolean"), common),
    decomp_area_plot = c(list(component_col = "string", period_col = "string", split_by = "split",
                              panel_var = "nullable_string", separate_figure = "boolean",
                              show_total_line = "boolean", normalize = "boolean"), common)
  )
}

#' @keywords internal
#' @noRd
.mcp_style_fun <- function(fun) {
  switch(fun,
         map_heatmap = c("create_map_style", "map_style_config"),
         trend_plot = c("create_trend_style", "trend_style_config"),
         decomp_area_plot = c("create_decomp_style", "decomp_style_config"),
         c("create_plot_style", "plot_style_config"))
}

#' Build a style call such as create_plot_style(color_tone = "gtap"), checking
#' option names.
#' @keywords internal
#' @noRd
.mcp_style_call <- function(style_fun, style) {
  if (!is.list(style) || (length(style) && is.null(names(style)))) {
    stop("style must be an object of option: value pairs.", call. = FALSE)
  }
  if (!length(style)) return(NULL)
  valid <- names(formals(get(style_fun, envir = asNamespace("GTAPViz"))))
  bad <- setdiff(names(style), valid)
  if (length(bad)) {
    hints <- vapply(bad, function(b) {
      d <- utils::adist(tolower(b), tolower(valid))[1, ]
      if (min(d) <= 4) sprintf("'%s' (did you mean '%s'?)", b, valid[which.min(d)]) else sprintf("'%s'", b)
    }, character(1))
    stop(sprintf("Unknown %s option(s): %s. Call style_options to list valid options.",
                 style_fun, paste(hints, collapse = ", ")), call. = FALSE)
  }
  vals <- lapply(names(style), function(k) {
    x <- style[[k]]
    if (k == "title_format" && is.list(x) && !is.null(names(x))) return(.mcp_title_call(x))
    if (k == "vertical_lines" && is.list(x)) {
      lines <- if (!is.null(names(x))) list(x) else x
      return(as.call(c(list(as.name("list")), lapply(lines, function(v) .mcp_call("create_vline", lapply(v, .mcp_value))))))
    }
    .mcp_value(x)
  })
  names(vals) <- names(style)
  .mcp_call(style_fun, vals)
}

#' @keywords internal
#' @noRd
.mcp_title_call <- function(x) {
  x <- x[intersect(names(x), c("type", "text", "sep"))]
  .mcp_call("create_title_format", lapply(x, .mcp_value))
}

#' @keywords internal
#' @noRd
.mcp_export_call <- function(x) {
  if (!is.list(x) || is.null(names(x))) stop("export must be an object, e.g. {\"width\": 20, \"height\": 12}.", call. = FALSE)
  valid <- names(formals(create_export_config))
  bad <- setdiff(names(x), valid)
  if (length(bad)) {
    stop("Unknown export option(s): ", paste(bad, collapse = ", "), ". Valid: ",
         paste(valid, collapse = ", "), call. = FALSE)
  }
  .mcp_call("create_export_config", lapply(x, .mcp_value))
}

#' Map-specific preparation: default ISO column, label boxes, ISO3 check.
#' @keywords internal
#' @noRd
.mcp_prepare_map <- function(call_args, args, df) {
  if (is.null(call_args$iso_col)) {
    if (!"REG" %in% names(df) && "Region" %in% names(df)) call_args$iso_col <- "Region"
  }
  iso_col <- if (is.null(call_args$iso_col)) "REG" else call_args$iso_col
  if (!iso_col %in% names(df)) {
    stop("iso_col '", iso_col, "' not found. Columns: ", paste(names(df), collapse = ", "), call. = FALSE)
  }
  if (length(call_args$map_region) == 1L) call_args$map_region <- call_args$map_region[1]
  codes <- unique(as.character(df[[iso_col]]))
  iso3 <- countrycode::codelist$iso3c
  matched <- toupper(codes) %in% iso3
  if (!any(matched)) {
    stop("Column '", iso_col, "' holds ", .mcp_values_text(codes, 12),
         ", which are not ISO3 country codes, so they cannot be placed on a map. ",
         "map_heatmap needs country-level regions such as USA, CHN, THA. If these are aggregated ",
         "regions, tell the user; if they are country names, use rename_values to map them to ISO3 codes.",
         call. = FALSE)
  }
  if (!all(matched)) {
    warning("Not drawn on the map (not ISO3 codes): ", .mcp_values_text(codes[!matched], 20), call. = FALSE)
  }
  lb <- args$label_box_vars
  if (is.list(lb) && length(lb)) {
    if (!is.null(names(lb))) lb <- list(lb)
    get_field <- function(f, default) vapply(lb, function(v) {
      if (is.null(v[[f]])) default else as.character(v[[f]])
    }, character(1))
    lb_args <- list(variable = get_field("variable", NA_character_))
    if (anyNA(lb_args$variable)) stop("Every label_box_vars entry needs a variable.", call. = FALSE)
    labels <- get_field("label", "")
    if (any(nzchar(labels))) lb_args$label <- ifelse(nzchar(labels), labels, lb_args$variable)
    dec <- vapply(lb, function(v) if (is.null(v$decimal_places)) 1 else as.numeric(v$decimal_places), numeric(1))
    if (any(dec != 1)) lb_args$decimal_places <- dec
    call_args$label_box_vars <- .mcp_call("create_label_box_vars", lb_args)
  }
  call_args
}

#' Factory for the plot tool handlers.
#' @keywords internal
#' @noRd
.mcp_plot_handler <- function(fun) {
  force(fun)
  function(srv, args) {
    ref <- .mcp_ref(srv, args$data, want = "table")
    df <- ref$value
    call_args <- c(list(data = ref$expr), .mcp_build_args(args, .mcp_plot_specs(fun)))
    if (fun == "map_heatmap") call_args <- .mcp_prepare_map(call_args, args, df)

    .mcp_check_columns(df, call_args, c(
      "x_axis_from", "stack_value_from", "split_by", "panel_var", "component_col",
      "period_col", "line_group_by", "iso_col", "value_col", "aggregate_by"
    ))
    filtered <- .mcp_apply_filter(df, call_args$filter_var)

    # Guard against hundreds of figures
    split_by <- if ("split_by" %in% names(call_args)) call_args$split_by else .mcp_formal_default(fun, "split_by")
    panel_var <- if ("panel_var" %in% names(call_args)) call_args$panel_var else .mcp_formal_default(fun, "panel_var")
    x_axis <- switch(fun, trend_plot = , decomp_area_plot = "Period", map_heatmap = NULL, call_args$x_axis_from)
    if (fun == "map_heatmap" && !is.null(call_args$map_value_var)) {
      filtered <- filtered[filtered$Variable %in% call_args$map_value_var, , drop = FALSE]
    }
    n_fig <- .mcp_count_figures(filtered, split_by, panel_var, call_args$separate_figure,
                                x_axis_from = x_axis, unstack = call_args$unstack_plot)
    max_fig <- .mcp_arg(args, "max_figures", "integer", 12)
    if (n_fig > max_fig) {
      n_vars <- if ("Variable" %in% names(filtered)) length(unique(filtered$Variable)) else 1L
      stop(sprintf(paste0(
        "This would create about %d figures (%d variable%s%s). Narrow it with filter_var, e.g. ",
        "{\"Variable\": [\"...\"]}, or raise max_figures if the user really wants all of them."
      ), n_fig, n_vars, if (n_vars == 1) "" else "s",
      if (is.character(split_by)) paste0(" x ", paste(split_by, collapse = " x ")) else ""), call. = FALSE)
    }

    style <- .mcp_style_fun(fun)
    if (!is.null(args$style)) call_args[[style[2]]] <- .mcp_style_call(style[1], args$style)
    if (!is.null(args$title_format) && "title_format" %in% names(formals(get(fun)))) {
      call_args$title_format <- .mcp_title_call(args$title_format)
    }
    if (!is.null(args$export)) call_args$export_config <- .mcp_export_call(args$export)

    out <- .mcp_output_expr(srv, .mcp_arg(args, "output_folder"))
    call_args$output_path <- out$expr
    call_args$export_picture <- TRUE
    pdf <- .mcp_arg(args, "export_as_pdf", "pdf", FALSE)
    if (!isFALSE(pdf)) call_args$export_as_pdf <- pdf

    # Arguments in the order of the function signature
    call_args <- call_args[order(match(names(call_args), names(formals(get(fun)))))]

    title <- sprintf("%s of %s", fun, ref$label)
    if (is.list(call_args$filter_var) && !is.null(call_args$filter_var$Variable)) {
      title <- sprintf("%s (%s)", title, paste(call_args$filter_var$Variable, collapse = ", "))
    }
    before <- .mcp_snapshot(out$dir)
    res <- .mcp_eval(srv, .mcp_call(fun, call_args), title)
    files <- .mcp_changed_files(before, out$dir)
    pngs <- files[grepl("\\.png$", files, ignore.case = TRUE)]
    n_prev <- min(max(.mcp_arg(args, "preview_images", "integer", 3), 0), 6)

    if (!length(files)) {
      return(list(
        text = sprintf("%s ran but created no figure. Check filter_var and the columns (describe_data).", fun),
        code = res$code, show_log = TRUE
      ))
    }
    text <- sprintf("%s created %d figure file%s in %s.", fun, length(files),
                    if (length(files) == 1) "" else "s", out$dir)
    if (length(pngs) > n_prev && n_prev > 0) {
      text <- c(text, sprintf("Previews of the first %d PNG files are attached.", n_prev))
    }
    text <- c(text, "Look at the previews and adjust style/export if labels overlap or text is hard to read.")
    list(text = text, code = res$code, files = files, images = utils::head(pngs, n_prev))
  }
}

# HANDLERS: TABLES ---------------------------------------------------------------

#' Data list expression for report_table: whole list, selected tables, or a
#' single table wrapped as a one-element list.
#' @keywords internal
#' @noRd
.mcp_table_selection <- function(srv, data, tables = NULL) {
  ref <- .mcp_ref(srv, data, want = "any")
  if (is.data.frame(ref$value)) {
    if (is.null(ref$table)) {
      expr <- call("list", ref$expr)
      names(expr)[2] <- ref$object
      return(list(expr = expr, frames = stats::setNames(list(ref$value), ref$object)))
    }
    parent <- get(ref$object, envir = srv$ws)
    return(list(expr = call("[", as.name(ref$object), ref$table), frames = parent[ref$table]))
  }
  frames <- Filter(is.data.frame, ref$value)
  if (length(tables)) {
    missing <- setdiff(tables, names(frames))
    if (length(missing)) {
      stop("Table(s) not in ", ref$object, ": ", paste(missing, collapse = ", "),
           ". Available: ", paste(names(frames), collapse = ", "), call. = FALSE)
    }
    return(list(expr = call("[", ref$expr, tables), frames = frames[tables]))
  }
  list(expr = ref$expr, frames = frames)
}

#' @keywords internal
#' @noRd
.mcp_h_report_table <- function(srv, args) {
  sel <- .mcp_table_selection(srv, args$data, .mcp_arg(args, "tables", "strings"))
  keys <- names(sel$frames)

  per_table <- function(single_name, by_table_name, type) {
    by_table <- args[[by_table_name]]
    if (is.list(by_table) && length(by_table) && !is.null(names(by_table))) {
      return(lapply(by_table, function(v) .mcp_convert(v, type)))
    }
    single <- .mcp_arg(args, single_name, type)
    if (is.null(single)) return(NULL)
    stats::setNames(rep(list(single), length(keys)), keys)
  }
  pivot_col <- per_table("pivot_col", "pivot_col_by_table", "string")
  if (is.null(pivot_col)) {
    stop('pivot_col is required, e.g. "Variable" (or pivot_col_by_table).', call. = FALSE)
  }
  group_by <- per_table("group_by", "group_by_table", "strings")

  for (k in names(pivot_col)) {
    if (!k %in% keys) stop("pivot_col_by_table names unknown table '", k, "'. Tables: ", paste(keys, collapse = ", "), call. = FALSE)
    cols <- c(pivot_col[[k]], if (!is.null(group_by)) group_by[[k]])
    missing <- setdiff(cols, names(sel$frames[[k]]))
    if (length(missing)) {
      stop(sprintf("Table '%s' has no column %s. Columns: %s", k, paste(missing, collapse = ", "),
                   paste(names(sel$frames[[k]]), collapse = ", ")), call. = FALSE)
    }
  }

  spec <- list(total_column = "boolean", decimal = "integer", include_units = "boolean",
               var_name_by_description = "boolean", add_var_info = "boolean",
               separate_sheet_by = "string", unit_select = "string", component_exclude = "strings",
               subtotal_level = "boolean", repeat_label = "boolean", add_group_line = "boolean",
               separate_file = "boolean")
  call_args <- c(list(data_list = sel$expr, pivot_col = pivot_col),
                 if (!is.null(group_by)) list(group_by = group_by))
  if (is.list(args$rename_cols) && length(args$rename_cols)) call_args$rename_cols <- lapply(args$rename_cols, .mcp_value)
  call_args <- c(call_args, .mcp_build_args(args, spec))
  out <- .mcp_output_expr(srv, .mcp_arg(args, "output_folder"))
  call_args$export_table <- TRUE
  call_args$output_path <- out$expr
  call_args$workbook_name <- .mcp_arg(args, "workbook_name", "string", "GTAPViz_report")

  before <- .mcp_snapshot(out$dir)
  res <- tryCatch(
    .mcp_eval(srv, .mcp_call("report_table", call_args), sprintf("report_table of %s", paste(keys, collapse = ", "))),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("duplicate", msg, ignore.case = TRUE)) {
        msg <- paste0(msg, "\nHint: some variables share the same description. Set ",
                      "var_name_by_description = false, or filter with unit_select / fewer tables.")
      }
      stop(msg, call. = FALSE)
    }
  )
  files <- .mcp_changed_files(before, out$dir)
  tables_out <- res$value
  preview <- character()
  if (is.list(tables_out) && length(tables_out)) {
    for (nm in utils::head(names(tables_out), 2)) {
      preview <- c(preview, "", sprintf("Preview of '%s' (first rows):", nm),
                   .mcp_md_table(utils::head(tables_out[[nm]], 12)))
    }
  }
  list(
    text = c(sprintf("report_table created %d table%s: %s", length(tables_out),
                     if (length(tables_out) == 1) "" else "s", paste(names(tables_out), collapse = ", ")),
             preview),
    code = res$code, files = files
  )
}

#' @keywords internal
#' @noRd
.mcp_h_pivot_table <- function(srv, args) {
  ref <- .mcp_ref(srv, args$data, want = "table")
  call_args <- c(list(data = ref$expr), .mcp_build_args(args, list(
    filter = "strings", rows = "strings", cols = "strings", data_fields = "string"
  )))
  .mcp_check_columns(ref$value, call_args, c("filter", "rows", "cols", "data_fields"))
  out <- .mcp_output_expr(srv, .mcp_arg(args, "output_folder"))
  workbook <- .mcp_arg(args, "workbook_name", "string", "GTAPViz_pivot.xlsx")
  if (!grepl("\\.xlsx$", workbook, ignore.case = TRUE)) workbook <- paste0(workbook, ".xlsx")
  call_args$export_table <- TRUE
  call_args$output_path <- out$expr
  call_args$workbook_name <- workbook
  before <- .mcp_snapshot(out$dir)
  res <- .mcp_eval(srv, .mcp_call("pivot_table_with_filter", call_args), sprintf("Pivot table of %s", ref$label))
  list(text = "Excel pivot table created (open it in Excel to use the filters).",
       code = res$code, files = .mcp_changed_files(before, out$dir))
}

# HANDLERS: DATA CHANGES ---------------------------------------------------------

#' Apply fun(target, ...) to each target and assign the result back.
#' @keywords internal
#' @noRd
.mcp_modify <- function(srv, targets, fun, fun_args, title) {
  exprs <- lapply(targets, function(t) {
    call("<-", t$expr, .mcp_call(fun, c(list(t$expr), fun_args)))
  })
  res <- .mcp_eval(srv, exprs, title)
  list(
    text = sprintf("Updated: %s", paste(vapply(targets, `[[`, "", "label"), collapse = ", ")),
    code = res$code
  )
}

#' @keywords internal
#' @noRd
.mcp_h_rename <- function(srv, args) {
  column <- .mcp_arg(args, "column")
  mapping <- args$mapping
  if (is.null(column)) stop("column is required.", call. = FALSE)
  if (!is.list(mapping) || is.null(names(mapping)) || !length(mapping)) {
    stop('mapping must be an object such as {"EastAsia": "East Asia"}.', call. = FALSE)
  }
  targets <- .mcp_targets(srv, args$data, column = column)
  old <- names(mapping)
  new <- vapply(mapping, function(v) as.character(.mcp_vec(v))[1], character(1))
  present <- unique(unlist(lapply(targets, function(t) {
    unlist(lapply(.mcp_frames(t$value), function(df) if (column %in% names(df)) as.character(df[[column]])))
  })))
  missing <- setdiff(old, present)
  if (length(missing) == length(old)) {
    stop(sprintf("None of %s found in column '%s'. Values: %s", paste(old, collapse = ", "),
                 column, .mcp_values_text(present)), call. = FALSE)
  }
  if (length(missing)) warning("Not found (ignored): ", paste(missing, collapse = ", "), call. = FALSE)
  mapping_df <- .mcp_call("data.frame", list(OldName = unname(old), NewName = unname(new)))
  .mcp_modify(srv, targets, "rename_value", list(column_name = column, mapping.file = mapping_df),
              sprintf("Rename values in %s", column))
}

#' @keywords internal
#' @noRd
.mcp_h_sort <- function(srv, args) {
  order_spec <- args$order
  sort_value <- .mcp_arg(args, "sort_by_value")
  if ((!is.list(order_spec) || !length(order_spec)) && is.null(sort_value)) {
    stop('Give order, e.g. {"Experiment": ["EXP2", "EXP1"]}, and/or sort_by_value.', call. = FALSE)
  }
  fun_args <- list()
  if (is.list(order_spec) && length(order_spec)) {
    fun_args$sort_columns <- lapply(order_spec, function(v) as.character(.mcp_vec(v)))
  }
  if (!is.null(sort_value)) fun_args$sort_by_value_desc <- identical(sort_value, "descending")
  targets <- .mcp_targets(srv, args$data)
  .mcp_modify(srv, targets, "sort_plot_data", fun_args, "Set display order")
}

#' @keywords internal
#' @noRd
.mcp_h_convert <- function(srv, args) {
  preset <- .mcp_arg(args, "preset")
  fun_args <- list()
  if (!is.null(preset)) {
    fun_args$scale_auto <- preset
  } else {
    from <- .mcp_arg(args, "from", "strings")
    to <- .mcp_arg(args, "to", "strings")
    adj <- .mcp_arg(args, "adjustment", "strings")
    if (!length(from) || length(from) != length(to) || length(from) != length(adj)) {
      stop("Give a preset, or from/to/adjustment arrays of the same length.", call. = FALSE)
    }
    fun_args <- list(change_unit_from = from, change_unit_to = to, adjustment = adj)
  }
  vars <- .mcp_arg(args, "variables", "strings")
  if (!is.null(vars)) fun_args$variable_select <- vars
  targets <- .mcp_targets(srv, args$data)
  .mcp_modify(srv, targets, "convert_units", fun_args, "Convert units")
}

# HANDLERS: OPTIONS, SCRIPT, R CODE ------------------------------------------------

#' @keywords internal
#' @noRd
.mcp_h_style_options <- function(srv, args) {
  kind <- .mcp_arg(args, "kind", "string", "plot")
  if (kind == "color_tones") {
    tones <- suppressMessages(get_color_palette("list"))
    return(list(text = c(
      "Color tones for style.color_tone (bar plots):", paste0("- ", tones, collapse = "\n"),
      "",
      "Trend/decomposition plots use style.color_palette (e.g. \"gtap\", \"viridis\", RColorBrewer names or a list of colors).",
      "Maps use color_low / color_mid / color_high or color_palette (e.g. \"viridis\", \"magma\")."
    )))
  }
  fun <- switch(kind, plot = "create_plot_style", map = "create_map_style", trend = "create_trend_style",
                decomp = "create_decomp_style", export = "create_export_config",
                title = "create_title_format", stop("Unknown kind: ", kind, call. = FALSE))
  f <- formals(get(fun, envir = asNamespace("GTAPViz")))
  descriptions <- .mcp_rd_arguments(fun)
  rows <- vapply(names(f), function(nm) {
    def <- paste(deparse(f[[nm]], width.cutoff = 500L), collapse = " ")
    if (!nzchar(def)) def <- "(required)"
    desc <- if (nm %in% names(descriptions)) descriptions[[nm]] else ""
    sprintf("- %s = %s%s", nm, def, if (nzchar(desc)) paste0(": ", desc) else "")
  }, character(1))
  list(text = c(
    sprintf("Options of %s() (pass them in the `style` object; JSON values, arrays for vectors):", fun),
    rows
  ))
}

#' @keywords internal
#' @noRd
.mcp_h_script <- function(srv, args) {
  script <- .mcp_script_text(srv)
  path <- .mcp_save_script(srv)
  extra <- .mcp_arg(args, "save_as")
  if (!is.null(extra)) {
    extra <- .mcp_path(extra)
    if (!dir.exists(dirname(extra))) dir.create(dirname(extra), recursive = TRUE)
    writeLines(script, extra, useBytes = TRUE)
  }
  saved <- c(path, extra)
  list(text = c(
    if (length(saved)) sprintf("Saved to: %s", paste(saved, collapse = " and ")) else
      "Nothing has been loaded yet, so the script is not saved to a file.",
    "", "```r", script, "```"
  ))
}

#' @keywords internal
#' @noRd
.mcp_h_run_r <- function(srv, args) {
  code <- .mcp_arg(args, "code")
  if (is.null(code) || !nzchar(trimws(code))) stop("code is empty.", call. = FALSE)
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) {
    stop("R could not parse the code: ", conditionMessage(e), call. = FALSE)
  })
  out_dir <- .mcp_current_output_dir(srv)
  before <- .mcp_snapshot(out_dir)
  printed <- character()
  last <- NULL
  for (e in exprs) {
    printed <- c(printed, utils::capture.output({
      res <- withVisible(eval(e, envir = srv$ws))
      if (res$visible && !inherits(res$value, "gg")) print(res$value)
    }))
    last <- res
  }
  .mcp_record(srv, .mcp_arg(args, "title", "string", "Custom R code"), trimws(code))
  images <- character()
  if (!is.null(last) && last$visible && inherits(last$value, "gg")) {
    tmp <- tempfile(fileext = ".png")
    ggplot2::ggsave(tmp, last$value, width = 10, height = 6, dpi = 110)
    images <- tmp
  }
  files <- if (!is.null(out_dir)) .mcp_changed_files(before, out_dir) else character()
  list(
    text = c("Code ran successfully.",
             if (length(printed)) c("", "Output:", paste(utils::tail(printed, 80), collapse = "\n"))),
    code = trimws(code), files = files,
    images = c(images, utils::head(files[grepl("\\.png$", files, ignore.case = TRUE)], 3))
  )
}
