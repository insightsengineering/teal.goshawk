# Set the attributes of the last chunk outputs

Set the attributes of the last chunk outputs

## Usage

``` r
set_chunk_attrs(
  teal_card,
  attributes,
  n = 1,
  inner_classes = NULL,
  quiet = FALSE
)
```

## Arguments

- teal_card:

  (`teal_card`) object to modify.

- attributes:

  (`list`) of attributes to set on the last chunk outputs.

- n:

  (`integer(1)`) number of the last element of `teal_card` to modify. it
  will only change `chunk_output` objects.

- inner_classes:

  (`character`) classes within `chunk_output` that should be modified.
  This can be used to only change `recordedplot`, `ggplot2` or other
  type of objects.

## Value

The modified `teal_card` object with updated attributes for the last
chunk outputs.

## Examples

``` r
set_chunk_attrs <- getFromNamespace("set_chunk_attrs", "teal.goshawk")
td <- within(
  teal.reporter::teal_report(),
  ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point()
)
card <- teal.reporter::teal_card(td)
set_chunk_attrs(card, list(dev.width = 200, dev.height = 100))

#> $`444fc0ed`
#> [1] "ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point()"
#> attr(,"params")
#> list()
#> attr(,"lang")
#> [1] "R"
#> attr(,"class")
#> [1] "code_chunk"
#> 
#> $d7bafcae
#> [[1]]
#> 
#> attr(,"class")
#> [1] "chunk_output"
#> attr(,"dev.width")
#> [1] 200
#> attr(,"dev.height")
#> [1] 100
#> 
#> attr(,"class")
#> [1] "teal_card"
#> attr(,"metadata")
#> list()
```
