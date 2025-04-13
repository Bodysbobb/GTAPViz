## ----include = FALSE, eval = FALSE--------------------------------------------
# knitr::opts_chunk$set(
#   collapse = TRUE,
#   comment = "#>",
#   warning = FALSE,
#   message = FALSE,
#   eval = requireNamespace("GTAPViz", quietly = TRUE)
# )

## ----Dev Period, include = FALSE, eval = TRUE---------------------------------
rm(list=ls())

try(devtools::load_all(".."), silent = TRUE)  # go up one level from /vignettes/

input_path <- system.file("extdata/in", package = "GTAPViz")
sl4.plot.data <- readRDS(file.path(input_path, "sl4.plot.data.rds"))
bilateral_data <- readRDS(file.path(input_path, "bilateral_data.rds"))
har.plot.data <- readRDS(file.path(input_path, "har.plot.data.rds"))
macro.data    <- readRDS(file.path(input_path, "macro.data.rds"))

## ----eval = TRUE, message = FALSE, warning = FALSE, results = 'hide'----------
reg_data = sl4.plot.data[["REG"]]

plotA <- comparison_plot(
          # === Input Data ===
          data         = reg_data,
          filter_var   = NULL,
          x_axis_from  = "Region",
          split_by     = "Variable",
          panel_var    = "Experiment",
          variable_col = "Variable",
          unit_col     = "Unit",
          desc_col     = "Description",
        
          # === Plot Behavior ===
          invert_axis     = FALSE,
          separate_figure = FALSE,
        
          # === Variable Display ===
          var_name_by_description = FALSE,
          add_var_info            = FALSE,
        
          # === Export Settings ===
          output_path     = NULL,
          export_picture  = FALSE,
          export_as_pdf   = FALSE,
          export_config   = create_export_config(
            width  = 20,
            height = 12
          ),
        
          # === Styling ===
          plot_style_config = create_plot_style(
            color_tone         = "purdue",
            add_unit_to_title  = TRUE,
            title_format       = create_title_format(
              type = "prefix",
              text = "Impact on"
            ),
            panel_rows = 2
          )
        )

## ----echo=FALSE, fig.align='center', fig.width=12, fig.height=10--------------
print(plotA[[1]])

## ----eval = TRUE, message = FALSE, warning = FALSE, results = 'hide'----------
macro.plot <- comparison_plot(
  # === Input Data ===
  data         = macro.data[["macros"]],
  filter_var   = c("pgdpwld", "qgdpwld", "vgdpwld"),
  x_axis_from  = "Variable",
  split_by     = FALSE,     # THIS IS THE MOST IMPORTANT PART FOR THIS PLOT

  # === Export Settings ===
  output_path     = NULL,
  export_picture  = FALSE,
  export_as_pdf   = FALSE,
  export_config   = create_export_config(
    width  = 20,
    height = 15
  ),

  # === Styling ===
  plot_style_config = create_plot_style(
    color_tone = "gtap",
    title_format = create_title_format(
      type = "full",
      text = "Global Economic Impacts"
    )
  )
)

## ----echo=FALSE, fig.align='center', fig.width=12, fig.height=10--------------
print(macro.plot[[1]])

## ----eval = TRUE, message = FALSE, warning = FALSE, results = 'hide'----------
sector_data <- sl4.plot.data[["COMM*REG"]]

plotB <- detail_plot(
  # === Input Data ===
  data        = sector_data,
  filter_var  = list(Region = c("Oceania")),
  x_axis_from = "Commodity",
  split_by    = "Region",
  panel_var   = "Experiment",
  variable_col = "Variable",
  unit_col     = "Unit",
  desc_col     = "Description",
  
  # === Plot Behavior ===
  invert_axis      = TRUE,
  separate_figure  = FALSE,
  top_impact       = NULL,
  
  # === Variable Display ===
  var_name_by_description = TRUE,
  add_var_info            = FALSE,
  
  # === Export Settings ===
  output_path     = NULL,
  export_picture  = FALSE,
  export_as_pdf   = FALSE,
  export_config   = create_export_config(
    width  = 45,
    height = 20
  ),
  
  # === Styling ===
  plot_style_config = create_plot_style(
    positive_color = "#2E8B57",
    negative_color = "#CD5C5C",
    panel_rows = 1,
    panel_cols = NULL,
    show_axis_titles_on_all_facets = FALSE,
    y_axis_text_size = 25,
    bar_width = 0.6,
    all_font_size = 1.1
  )
)

## ----echo=FALSE, fig.align='center', fig.width=12, fig.height=10--------------
print(plotB[[1]])

## ----echo=FALSE, message = FALSE, warning = FALSE, fig.align='center', fig.width=12, fig.height=10----
plotB2 <- detail_plot(
  # === Input Data ===
  data        = sector_data,
  filter_var  = list(Region = c("Oceania")),
  x_axis_from = "Commodity",
  split_by    = "Region",
  panel_var   = "Experiment",
  variable_col = "Variable",
  unit_col     = "Unit",
  desc_col     = "Description",
  
  # === Plot Behavior ===
  invert_axis      = TRUE,
  separate_figure  = FALSE,
  top_impact       = 10,
  
  # === Variable Display ===
  var_name_by_description = FALSE,
  add_var_info            = FALSE,
  
  # === Export Settings ===
  output_path     = NULL,
  export_picture  = FALSE,
  export_as_pdf   = FALSE,
  export_config   = create_export_config(
    width  = 25,
    height = 12
  ),
  
  # === Styling ===
  plot_style_config = create_plot_style(
    positive_color = "#2E8B57",
    negative_color = "#CD5C5C",
    panel_rows = 1,
    panel_cols = NULL,
    show_axis_titles_on_all_facets = FALSE,
    y_axis_text_size = 25,
    bar_width = 0.6,
    all_font_size = 1.1
  )
)

print(plotB2[[1]])

## ----eval = TRUE, message = FALSE, warning = FALSE, results = 'hide'----------
bilateral_data <- bilateral_data[["qxs"]]

plotB3 <- detail_plot(
  # === Input Data ===
  data        = bilateral_data,
  filter_var  = list(Source = c("Oceania"),
                     Destination = c("SEAsia", "EastAsia")),
  x_axis_from = "Commodity",
  split_by    = c("Source", "Destination"),    # The most important part!
  
  # === Plot Behavior ===
  invert_axis      = TRUE,
  separate_figure  = FALSE,
  top_impact       = NULL,
  
  # === Variable Display ===
  var_name_by_description = FALSE,
  add_var_info            = FALSE,
  
  # === Export Settings ===
  output_path     = NULL,
  export_picture  = FALSE,
  export_as_pdf   = FALSE,
  export_config   = create_export_config(
    width  = 45,
    height = 20
  ),
  
  # === Styling ===
  plot_style_config = create_plot_style(
    positive_color = "#2E8B57",
    negative_color = "#CD5C5C",
    panel_rows = 1,
    panel_cols = NULL,
    show_axis_titles_on_all_facets = FALSE,
    y_axis_text_size = 25,
    bar_width = 0.6,
    all_font_size = 1.1
  )
)

## ----echo=FALSE, fig.align='center', fig.width=12, fig.height=10--------------
print(plotB3[[1]])

## ----eval = TRUE, message = FALSE, warning = FALSE, results = 'hide'----------
# Rename Value if needed
wefare.decomp.rename <- data.frame(
  ColumnName = "COLUMN",
  OldName = c("alloc_A1", "ENDWB1", "tech_C1", "pop_D1", "pref_G1", "tot_E1", "IS_F1"),
  NewName = c("Alloc Eff.", "Endwb", "Tech Chg.", "Pop", "Perf", "ToT", "I-S"),
  stringsAsFactors = FALSE
)

har.plot.data <- rename_value(har.plot.data, mapping.file = wefare.decomp.rename)

## ----eval = TRUE, message = FALSE, warning = FALSE, results = 'hide'----------
plotC <- stack_plot(
  # === Input Data ===
  data              = har.plot.data[["A"]],
  filter_var        = NULL,
  x_axis_from       = "Region",
  stack_value_from  = "COLUMN",
  split_by          = FALSE,
  panel_var         = "Experiment",
  variable_col      = "Variable",
  unit_col          = "Unit",
  desc_col          = "Description",

  # === Plot Behavior ===
  invert_axis     = FALSE,
  separate_figure = FALSE,
  show_total      = TRUE,
  unstack_plot    = FALSE,
  top_impact      = NULL,

  # === Variable Display ===
  var_name_by_description = TRUE,
  add_var_info            = FALSE,

  # === Export Settings ===
  output_path     = NULL,
  export_picture  = FALSE,
  export_as_pdf   = FALSE,
  export_config   = create_export_config(
    width  = 28,
    height = 15
  ),

  # === Styling ===
  plot_style_config = create_plot_style(
    color_tone                   = "gtap",
    panel_rows                   = 2,
    panel_cols                   = NULL,
    show_legend                  = TRUE,
    show_axis_titles_on_all_facets = FALSE
  )
)

## ----echo=FALSE, fig.align='center', fig.width=12, fig.height=10--------------
print(plotC[[1]])

## ----echo=FALSE, message = FALSE, warning = FALSE, fig.align='center', fig.width=12, fig.height=10----
plotD <- stack_plot(
  # === Input Data ===
  data              = har.plot.data[["A"]],
  filter_var        = NULL,
  x_axis_from       = "Region",
  stack_value_from  = "COLUMN",
  split_by          = FALSE,

  # === Plot Behavior ===
  invert_axis     = TRUE,
  separate_figure = FALSE,
  show_total      = TRUE,
  unstack_plot    = TRUE,
  top_impact      = NULL,

  # === Variable Display ===
  var_name_by_description = TRUE,
  add_var_info            = FALSE,

  # === Export Settings ===
  output_path     = NULL,
  export_picture  = FALSE,
  export_as_pdf   = FALSE,
  export_config   = create_export_config(
    width  = 28,
    height = 15
  ),

  # === Styling ===
  plot_style_config = create_plot_style(
    color_tone                   = "gtap",
    panel_rows                   = 1,
    panel_cols                   = NULL,
    show_legend                  = TRUE,
    show_axis_titles_on_all_facets = FALSE
  )
)

print(plotD[[1]])

## ----eval = TRUE, message = FALSE, warning = FALSE, results = 'hide'----------
plotE <- stack_plot(
  # === Input Data ===
  data              = har.plot.data[["E1"]],
  filter_var        = list(Region = c("Oceania", "SEAsia")),
  x_axis_from       = "Commodity",
  stack_value_from  = "PRICES",
  split_by          = "Region",
  panel_var         = "Experiment",
  variable_col      = "Variable",
  unit_col          = "Unit",
  desc_col          = "Description",

  # === Plot Behavior ===
  invert_axis     = TRUE,
  separate_figure = FALSE,
  show_total      = TRUE,
  unstack_plot    = FALSE,
  top_impact      = NULL,

  # === Export Settings ===
  output_path     = NULL,
  export_picture  = FALSE,
  export_as_pdf   = FALSE,
  export_config   = create_export_config(
    width  = 50,
    height = 30
  ),

  # === Styling ===
  plot_style_config = create_plot_style(
    title_format = create_title_format(
      type = "prefix",
      text = "Terms of Trade Decomposition",
      sep  = ": "
    ),
    color_tone                   = "winter", 
    panel_rows                   = 1,
    show_axis_titles_on_all_facets = FALSE,
    bar_width                    = 0.5,
    show_legend                  = TRUE
  )
)

## ----echo=FALSE, fig.align='center', fig.width=12, fig.height=10--------------
print(plotE[[1]])

## ----eval=FALSE---------------------------------------------------------------
# get_all_config()

## ----eval=FALSE---------------------------------------------------------------
# comparison_plot <- (...,
#                     export_config = my_export_config,
#                     plot_style_config = my_style_config
#                     )

## ----Sorting Data Function, eval = TRUE---------------------------------------
# Using the function
sort_sl4_plot <- sort_plot_data(
  sl4.plot.data,
  sort_columns = list(
    Experiment = c("EXP2", "EXP1"), # Column Name = Sorting Order
    Region = c("SEAsia", "Oceania")
    ),
  sort_by_value_desc = NULL
)

## ----echo=FALSE, message = FALSE, warning = FALSE, fig.align='center', fig.width=12, fig.height=10----
sort_sl4_plot = sort_sl4_plot[["REG"]]

plotE <- comparison_plot(
          # === Input Data ===
          data         = sort_sl4_plot,
          filter_var   = NULL,
          x_axis_from  = "Region",
          split_by     = "Variable",
          panel_var    = "Experiment",
          variable_col = "Variable",
          unit_col     = "Unit",
          desc_col     = "Description",
        
          # === Plot Behavior ===
          invert_axis     = FALSE,
          separate_figure = FALSE,
        
          # === Variable Display ===
          var_name_by_description = FALSE,
          add_var_info            = FALSE,
        
          # === Export Settings ===
          output_path     = NULL,
          export_picture  = FALSE,
          export_as_pdf   = FALSE,
          export_config   = create_export_config(
            width  = 20,
            height = 12
          ),
        
          # === Styling ===
          plot_style_config = create_plot_style(
            color_tone         = "purdue",
            add_unit_to_title  = TRUE,
            title_format       = create_title_format(
              type = "prefix",
              text = "Impact on"
            ),
            panel_rows = 2
          )
        )

print(plotE[[1]])

## ----Rename Value, eval = FALSE-----------------------------------------------
# # Creating Mapping File
# rename.region <- data.frame(
#   ColumnName = "REG",
#   OldName = c("USA", "CHN"),
#   NewName = c("United States", "China"),
#   stringsAsFactors = FALSE
# )
# 
# sl4.plot.data_rename <- rename_value(sl4.plot.data, mapping.file = rename.region)

## ----eval = FALSE-------------------------------------------------------------
# # Rename Value if needed
# wefare.decomp.rename <- data.frame(
#   ColumnName = "COLUMN",
#   OldName = c("alloc_A1", "ENDWB1", "tech_C1", "pop_D1", "pref_G1", "tot_E1", "IS_F1"),
#   NewName = c("Alloc Eff.", "Endwb", "Tech Chg.", "Pop", "Perf", "ToT", "I-S"),
#   stringsAsFactors = FALSE
# )
# 
# welfare_decomp <- rename_value(har.plot.data, mapping.file = wefare.decomp.rename)

