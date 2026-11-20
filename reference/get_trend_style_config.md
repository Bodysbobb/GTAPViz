# Get Trend Plot Style Configuration Helper

Prints a complete trend plot style configuration template to the console
that can be copied, modified, and used with trend_plot().

## Usage

``` r
get_trend_style_config()
```

## Value

Invisibly returns NULL after printing configuration template

## Author

Pattawee Puangchit

## Examples

``` r
get_trend_style_config()
#> my_trend_style <- create_trend_style(
#> 
#>   # Line and Point settings
#>   show_points = TRUE,
#>   point_size = 2,
#>   point_shape = 19, # 0-25 or 'circle', 'square', etc.
#>   point_alpha = 1,
#> 
#>   line_size = 1,
#>   line_type = "solid", # 'solid', 'dashed', 'dotted'
#>   line_alpha = 1,
#> 
#>   # Smoothing settings
#>   smooth_line = FALSE,
#>   smooth_se = FALSE,
#>   smooth_span = 0.75,
#> 
#>   # Color settings
#>   color_palette = NULL, # 'gtap', 'viridis', 'Set1', or c('#FF0000', '#00FF00')
#>   legend_title = NULL,
#> 
#>   # Period axis settings
#>   period_breaks = "auto", # 'all', 'auto', or numeric vector
#> 
#>   # Vertical reference lines
#>   vertical_lines = NULL, # list(list(period=2019, label='Shock 1', color='black', linetype='dashed'))
#>   vline_label_size = 3.5,
#>   vline_label_angle = 90,
#>   vline_label_vjust = -0.2,
#> 
#>   # Title settings
#>   show_title = TRUE,
#>   title_size = 20,
#>   add_unit_to_title = TRUE,
#> 
#>   # Legend settings
#>   show_legend = TRUE,
#>   legend_position = "right", # 'right', 'left', 'top', 'bottom'
#>   show_legend_title = TRUE,
#>   legend_key_width = 3, # Width of legend symbols
#>   legend_key_height = 1, # Height of legend symbols
#>   legend_line_size = 2, # Line thickness in legend
#>   legend_point_size = 3, # Point size in legend
#>   legend_text_size = 14,
#> 
#>   # Grid settings
#>   show_grid_major_x = TRUE,
#>   show_grid_major_y = TRUE,
#>   grid_color = "grey90",
#> 
#>   # Zero line settings
#>   show_zero_line = TRUE,
#>   zero_line_type = "dashed",
#> 
#>   # Font settings
#>   all_font_size = 1 # Multiplier for all text sizes
#> )
#> 
#> # Example usage:
#> # trend_plot(data, period_col = 'Period', line_group_by = 'Case', 
#> #            split_by = c('Variable', 'AREG'), trend_style_config = my_trend_style)
```
