# teal.goshawk 0.1.5.9000

* Add option to add rug plot in `tm_g_gh_density_distribution_plot`.
* Move `code` argument to `cdisc_dataset` (from `cdisc_data`) in examples.
* Changing slider titles from Transparency to Alpha for `tm_g_gh_boxplot` and `tm_g_gh_spaghettiplot`.
* Replace `plot_with_height` module with new `plot_with_settings` module. 
* Add drop down selector for treatment ARM `trt_group`.
* Introduce `facet_var` drop down in `tm_g_gh_scatterplot` and `tm_g_gh_correlationplot`.
* Replace `plot_with_height` module with new `plot_with_settings` module.
* Allow to set a minimum number of observations to render a node in `tm_g_gh_lineplot` and allow to change the font size in the table. Also the previously named argument `font_size` is now `plot_font_size` to distinguish it from the table font size argument.
* Add option to customize symbol type, line type and color in `tm_g_gh_lineplot`.
* issues with no news:

# teal.goshawk 0.1.5

* `templ_ui_params_vars` now uses `optionalSelectInput` from `teal`.
* `shape_choices` argument to `tm_g_gh_lineplot` can be either a character vector or `choices_selected`.

# teal.goshawk 0.1.4

* bug fix in correlation plot module related to axis ranges
* reflect changes in data filter panel re-factoring
* modification to correlation module to pass data for data driven LLOQ and ULOQ footnote

# teal.goshawk 0.1.3

* Added `.data` to PARAMCD in new functions related to sliders and reactivity.
* Fixing doc and other small fixes.
* Added toggable slider to all modules.
* Added data driven data constraints UI rendering.

# teal.goshawk 0.1.2

* Added checkbox input to control the following:
  - Box: Toggle LoQ legend on/off.
  - Correlation: Toggle LoQ legend on/off, toggle visit facetting on/off.
  - Density: Toggle combined treatment line on/off.
* Modified lineplot vertical axis range to match parameter value and CI range.

# teal.goshawk 0.1.1

* First release.