# teal.goshawk 0.1.13.9012

### Enhancements
* Added `teal.reporter` reporting into all the package modules.
* Added `plot_relative_height_value` argument to `tm_g_gh_lineplot` to control initial value of the relative plot height slider. 
* Implemented `nestcolor` with slight refactoring to `tm_g_gh_lineplot`. Added `nestcolor` in examples with no custom color manuals.

### Miscellaneous
* Fixed minor type coercion warning in `srv_arbitrary_lines`.
* Updated modules to not use datasets with suffix `_FILTERED` so the package works with the breaking changes in `teal.slice`. 

# teal.goshawk 0.1.13

### Miscellaneous
* Added a template to the `pkgdown` site.
* Updated package authors.

# teal.goshawk 0.1.12

### Breaking Changes
* Converted the `hline` parameter of `tm_g_gh_lineplot` to three parameters: `hline_arb`, `hline_arb_color` and `hline_arb_label`.

### Miscellaneous
* Added basic logging to the modules.
* Rewrote modules to use `moduleServer` and updated calls to `teal.devel` modules which have also been written to use `moduleServer`.
* Replaced calls to `teal::root_modules` with `teal::modules` following deprecation of `teal::root_modules`.
* Adjusted package imports to take into account changes to the `teal` framework.

# teal.goshawk 0.1.11

### Enhancements
* Added a UI input component to add additional arbitrary horizontal lines to `tm_g_gh_spaghettiplot`, `tm_g_gh_boxplot`, `tm_g_gh_density_distribution_plot` as well as two additional UI input components to add an additional horizontal and an additional vertical line to `tm_g_gh_correlationplot`.

### Bug Fixes
* Fixed an error in `tm_g_gh_boxplot` when no facet variable is selected.

### Miscellaneous
* Updated R version requirement to `R >= 3.6`.
* Removed dependency on `test.nest` package.
* Removed dependency on `utils.nest` package and replaced its functions with equivalents from the `checkmate` package.

# teal.goshawk 0.1.10

### New Features
* Lab normal range and `LOQs` horizontal line feature in `tm_g_gh_spaghettiplot`, `tm_g_gh_boxplot` and `tm_g_gh_correlationplot`.

### Breaking Changes
* Allow arbitrary horizontal line arguments in `tm_g_gh_spaghettiplot`, `tm_g_gh_boxplot`, `tm_g_gh_density_distribution_plot` and `tm_g_gh_correlationplot` and vertical line arguments in `tm_g_gh_correlationplot`. This functionality has changed the arguments required to use the modules:
  - `hline` replaced by `hline_arb`, `hline_arb_color` and `hline_arb_label` in the above modules.
  - `vline` replaced by `vline_arb_var`, `vline_arb_color` and `vline_arb_label` in `tm_g_gh_correlationplot`.

### Bug Fixes
* Fixed bug in `tm_g_gh_boxplot` module that always used the `AVISITCD` variable as the `Visit` Column of the table.

### Miscellaneous
* Updated `LICENCE` and `README` with new package references.
* Updated examples and documentation using `scda` synthetic data instead of `random.cdisc.data`.
* Added `error_on_lint: TRUE` to `.lintr`.
* Replaced `tidyr`'s `gather` and `spread` with `pivot_wider` and `pivot_longer` in package.

# teal.goshawk 0.1.9

### Enhancements
* Updated `tm_g_gh_correlationplot` and `tm_g_gh_scatterplot` encodings to have a checkbox to facet by the treatment variable instead of a drop down menu.
* Updated the starting line type to be solid instead of dashed in `tm_g_gh_lineplot`.

# teal.goshawk 0.1.8

### Enhancements
* `g_lineplot`
  - Updated the plot to remove x-axis label when x-axis is numeric and has no data in the corresponding y-axis variable.
  - Added slider to control the relative size of the plot and tables.
* Replaced function `brushedPoints` with `clean_brushedPoints` in `tm_g_gh_boxplot`,
`tm_g_gh_correlationplot`, `tm_g_gh_scatterplot` and `tm_g_gh_spaghettiplot`.

### Bug fixes
* Fixed infinite reactive loop inside of `toggle_slider_server`.

### Miscellaneous
* Renamed `toggle.R` file to `toggleable.R` file to be consistent with the accepted correct spelling of the word.

# teal.goshawk 0.1.7

### Enhancements
* `tm_g_gh_lineplot`
  - Added a table to display summary statistics.

### Bug fixes
* `tm_g_gh_lineplot`
  - Fixed displaying the number of messages, warnings and errors on the Debug Info button.
  - Fixed treatment variable having values with with symbols (e.g. ':').
  - Allow treatment variables with different arm levels.

### Miscellaneous
* Reduced minimum number of records required in dataset to either 1 or 2 in all modules.

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
