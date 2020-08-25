# teal.goshawk
The teal.goshawk package renders the UI and calls the respective longitudinal visualization functions.

The modules included are:

- tm_g_gh_boxplot
- tm_g_gh_boxplot_av (undergoing update)
- tm_g_gh_correlationplot
- tm_g_gh_correlationplot_av (undergoing update)
- tm_g_gh_density_distribution_plot
- tm_g_gh_lineplot
- tm_g_gh_scatterplot (undergoing update)
- tm_g_gh_spaghettiplot

# Sample App
Please refer to agile-r/Teal/Sample Teal Apps/Goshawk [here](https://pages.github.roche.com/NEST/docs/hugo/NEST/agile-R/master/teal/sample_apps/sample-app-goshawk)] for a ready to go sample app that uses randomly generated CDISC data.
# Deploy Repository
Please refer to the deploy repository [here](https://github.roche.com/STATSSPA/statsspa_384) for full details on data prepocessing and configuration of this package for deployment.

# Installation

[BEE](https://r.roche.com/) users should check out [Agile-R installation page](https://pages.github.roche.com/NEST/docs/hugo/NEST/agile-R/master/quick_start/install-nest-environment/).

You can install locally last released version of `teal.goshawk` with:
```
devtools::install_github(
  repo = "NEST/teal.goshawk",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)
```
