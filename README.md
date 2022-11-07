# teal.goshawk

<!-- start badges -->
[![Code Coverage](https://raw.githubusercontent.com/insightsengineering/teal.goshawk/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering/teal.goshawk/_xml_coverage_reports/data/main/coverage.xml)
<!-- end badges -->

The teal.goshawk package provides `teal` modules of the longitudinal visualization functions from the [goshawk](https://insightsengineering.github.io/goshawk/) R package.
This enables `teal` app developers to easily create applications to explore longitudinal clinical trial data.

## Modules

<!-- markdownlint-disable MD007 MD030 -->
-   `tm_g_gh_boxplot`
-   `tm_g_gh_correlationplot`
-   `tm_g_gh_density_distribution_plot`
-   `tm_g_gh_lineplot`
-   `tm_g_gh_scatterplot`
-   `tm_g_gh_spaghettiplot`
<!-- markdownlint-enable MD007 MD030 -->

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.goshawk@*release")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

In order to run many of the examples you will also need to install the [`scda`](https://insightsengineering.github.io/scda/) package.

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.goshawk.svg)](https://starchart.cc/insightsengineering/teal.goshawk)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.goshawk](https://reporoster.com/stars/insightsengineering/teal.goshawk)](https://github.com/insightsengineering/teal.goshawk/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.goshawk](https://reporoster.com/forks/insightsengineering/teal.goshawk)](https://github.com/insightsengineering/teal.goshawk/network/members)
