# teal.goshawk
The teal.goshawk package provides `teal` modules of the longitudinal visualization functions from the [goshawk](https://github.com/insightsengineering/goshawk) R package.
This enables `teal` app developers to easily create applications to explore longitudinal clinical trial data.

## Modules
- `tm_g_gh_boxplot`
- `tm_g_gh_correlationplot`
- `tm_g_gh_density_distribution_plot`
- `tm_g_gh_lineplot`
- `tm_g_gh_scatterplot`
- `tm_g_gh_spaghettiplot`

## Installation

This repository requires a personal access token to install see here [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token). Once this is set up, to install the latest released version of the package run:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("insightsengineering/teal.goshawk@*release", dependencies = FALSE)
```

You might need to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to `install_github` it may produce an error.

In order to run many of the examples you will also need to install the [scda](https://github.com/insightsengineering/scda) package.
