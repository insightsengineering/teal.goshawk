# teal.goshawk
The teal.goshawk package renders the UI and calls the respective longitudinal visualization functions in the goshawk package.

# Modules
- tm_g_gh_boxplot
- tm_g_gh_correlationplot
- tm_g_gh_density_distribution_plot
- tm_g_gh_lineplot
- tm_g_gh_scatterplot
- tm_g_gh_spaghettiplot

# Sample App
Please refer to agile-r/Teal/Sample Teal Apps/Goshawk [here](https://go.roche.com/agile-R) for a ready to use sample app that uses randomly generated CDISC data.

# General Usage
Please refer to the quick start section in agile-R [here](https://go.roche.com/agile-R).

# Installation
The latest version of `teal.goshawk` can be installed locally with:
```r
devtools::install_github(
  repo = "NEST/teal.goshawk",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)
```
