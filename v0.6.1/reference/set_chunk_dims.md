# Create a reactive that sets plot dimensions on a `teal_card`

This is a convenience function that creates a reactive expression that
automatically sets the `dev.width` and `dev.height` attributes on the
last chunk outputs of a `teal_card` based on plot dimensions from a plot
widget.

## Usage

``` r
set_chunk_dims(pws, q_r, inner_classes = NULL)
```

## Arguments

- pws:

  (`plot_widget`) plot widget that provides dimensions via
  [`dim()`](https://rdrr.io/r/base/dim.html) method

- q_r:

  (`reactive`) reactive expression that returns a `teal_reporter`

- inner_classes:

  (`character`) classes within `chunk_output` that should be modified.
  This can be used to only change `recordedplot`, `ggplot2` or other
  type of objects.

## Value

A reactive expression that returns the `teal_card` with updated
dimensions

## Examples

``` r
set_chunk_dims <- getFromNamespace("set_chunk_dims", "teal.goshawk")
td <- within(
  teal.reporter::teal_report(),
  ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point()
)
pws <- list(dim = reactive({
  list(width = 200, height = 100)
})) # mocking a teal.widget::plot_with_settings
set_chunk_dims(pws = pws, q_r = reactive(td))
#> reactive({
#>     pws_dim <- stats::setNames(as.list(req(pws$dim())), c("width", 
#>         "height"))
#>     if (identical(pws_dim$width, "auto")) {
#>         pws_dim$width <- NULL
#>     }
#>     if (identical(pws_dim$height, "auto")) {
#>         pws_dim$height <- NULL
#>     }
#>     q <- req(q_r())
#>     teal.reporter::teal_card(q) <- set_chunk_attrs(teal.reporter::teal_card(q), 
#>         list(dev.width = pws_dim$width, dev.height = pws_dim$height), 
#>         inner_classes = inner_classes)
#>     q
#> }) 
```
