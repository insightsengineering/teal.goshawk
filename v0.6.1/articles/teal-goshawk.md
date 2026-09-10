# Getting started with teal.goshawk

### Introduction

`teal.goshawk` is a package implementing a number of `teal` modules
helpful for exploring clinical trials data, specifically targeted at
data following the
[`ADaM`](https://www.cdisc.org/standards/foundational/adam) standards.
`teal.goshawk` modules can be used with data other than `ADaM` standard
clinical data but some features of the package will likely not be
applicable.

The concepts presented here require knowledge about the core features of
`teal`, specifically on how to launch a `teal` application and how to
pass data into it. Therefore, it is highly recommended to refer to the
[`README`](https://insightsengineering.github.io/teal/latest-tag/) file
and the introductory
[`vignette`](https://insightsengineering.github.io/teal/latest-tag/articles/index.html)
of the `teal` package.

### Main features

The package provides ready-to-use `teal` modules you can embed in your
`teal` application. The modules generate highly customizable plots and
outputs often used in exploratory data analysis, e.g.:

- box plots -
  [`tm_g_gh_boxplot()`](https://insightsengineering.github.io/teal.goshawk/reference/tm_g_gh_boxplot.md)
- correlation and scatter plots -
  [`tm_g_gh_correlationplot()`](https://insightsengineering.github.io/teal.goshawk/reference/tm_g_gh_correlationplot.md)
  and `tm_g_gh_scatterplot()`
- density distribution plots -
  [`tm_g_gh_density_distribution_plot()`](https://insightsengineering.github.io/teal.goshawk/reference/tm_g_gh_density_distribution_plot.md)
- line plots -
  [`tm_g_gh_lineplot()`](https://insightsengineering.github.io/teal.goshawk/reference/tm_g_gh_lineplot.md)
- spaghetti plots - `tm_g_spaghettiplot()`

See
[`package functions / modules`](https://insightsengineering.github.io/teal.goshawk/latest-tag/reference/).

### A simple application

A `teal.goshawk` module needs to be embedded inside a `shiny` / `teal`
application to interact with it.

There is no need to load `teal` as `teal.goshawk` already depends on it.
`nestcolor` is an optional package that can be loaded in to apply the
standardized NEST color palette to all module plots.

A simple application including the box plot module could look like this:

[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.goshawk`](https://insightsengineering.github.io/teal.goshawk/)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`nestcolor`](https://insightsengineering.github.io/nestcolor/)`)`` `` ``data`` ``<-`` ``teal_data``(``)`` ``data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``data``, ``{`` `` ``ADSL`` ``<-`` ``goshawk``::`[`rADSL`](https://insightsengineering.github.io/goshawk/latest-tag/reference/rADSL.html)` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``TRTORD ``=`` `[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`` `` ``TRT01P`` ``==`` ``"A: Drug X"`` ``~`` ``1``,`` `` ``TRT01P`` ``==`` ``"C: Combination"`` ``~`` ``2``,`` `` ``TRT01P`` ``==`` ``"B: Placebo"`` ``~`` ``3``,`` `` ``TRUE`` ``~`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``NA``)`` `` ``)`` `` ``)`` `` `` `` ``ADLB`` ``<-`` ``goshawk``::`[`rADLB`](https://insightsengineering.github.io/goshawk/latest-tag/reference/rADLB.html)` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVISITCD ``=`` ``AVISIT``,`` `` TRTORD ``=`` `[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(`` `` ``TRT01P`` ``==`` ``"A: Drug X"`` ``~`` ``1``,`` `` ``TRT01P`` ``==`` ``"C: Combination"`` ``~`` ``2``,`` `` ``TRT01P`` ``==`` ``"B: Placebo"`` ``~`` ``3``,`` `` ``TRUE`` ``~`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``NA``)`` `` ``)`` `` ``)`` ``}``)`` `` ``join_keys``(``data``)`` ``<-`` ``default_cdisc_join_keys``[`[`names`](https://rdrr.io/r/base/names.html)`(``data``)``]`` `` ``app`` ``<-`` ``teal``::`[`init`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)`(`` `` data ``=`` ``data``,`` `` modules ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` `[`tm_g_gh_boxplot`](https://insightsengineering.github.io/teal.goshawk/reference/tm_g_gh_boxplot.md)`(`` `` label ``=`` ``"Longitudinal Analysis"``,`` `` dataname ``=`` ``"ADLB"``,`` `` param_var ``=`` ``"PARAMCD"``,`` `` param ``=`` ``teal.transform``::`[`choices_selected`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)`(`` `` choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ALT"``, ``"CRP"``, ``"IGA"``)``,`` `` selected ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ALT"``)`` `` ``)``,`` `` trt_group ``=`` ``teal.transform``::`[`choices_selected`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)`(`` `` choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"TRT01P"``, ``"TRT01A"``)``,`` `` selected ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"TRT01P"``)`` `` ``)``,`` `` facet_var ``=`` ``teal.transform``::`[`choices_selected`](https://insightsengineering.github.io/teal.transform/latest-tag/reference/choices_selected.html)`(`` `` choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"TRT01P"``, ``"TRT01A"``)``,`` `` selected ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"TRT01P"``)`` `` ``)``,`` `` rotate_xlab ``=`` ``TRUE`` `` ``)`` `` ``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``shiny``::`[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``app``$``ui``, ``app``$``server``)`

Refer to the [Get
Started](https://insightsengineering.github.io/teal.modules.clinical/latest-tag/articles/teal-modules-clinical.html)
section of the teal.modules.clinical package that provides additional
detail on `teal` concepts as applied in another simple app example.

Please see additional information under
[Articles](https://insightsengineering.github.io/teal.goshawk/latest-tag/articles/index.html)
for data expectations, requirements and pre/post-processing rationale
