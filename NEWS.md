# teal.goshawk 0.1.7
### Enhancements
* `tm_g_gh_lineplot`
  - Added a table to display summary statistics.
  
### Bug fixes
* `tm_g_gh_lineplot`
  - Fixed displaying the number of messages, warnings and errors on the Debug Info button.
  - Fixed treatment variable having values with with symbols (e.g. ':').
  - Allow treatment variables with different arm levels.

# teal.goshawk 0.1.6

### Enhancements
* `tm_g_gh_boxplot` 
   - Changed slider title from "Transparency" to "Alpha". 
* `tm_g_gh_correlationplot`
    - Added `facet_var` argument and UI drop down.
* `tm_g_gh_density_distribution_plot`
    - Rug plot option added.
* `tm_g_gh_lineplot`
    - Argument changes: `font_size` --> `plot_font_size`.
    - Line and symbol type can now be configured. especially useful if line splitting is used.
    - Can set minimum records threshold for rendering data point in plot.
    - Table font size can now be controlled.
* `tm_g_gh_scatterplot`
    - Added `facet_var` argument and UI drop down.
* `tm_g_gh_spaghettiplot`
   - Changed slider title from "Transparency" to "Alpha".
    
### General
* Moved `code` argument to `cdisc_dataset` (from `cdisc_data`) in examples.
* Implemented new `plot_with_settings` functionality to all modules with support for plot resizing, zooming, and downloading functionality.
* Added drop down selector for treatment ARM.

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