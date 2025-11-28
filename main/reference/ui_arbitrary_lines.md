# UI module to arbitrary lines

UI module to input either horizontal or vertical lines to a plot via
comma separated values

## Usage

``` r
ui_arbitrary_lines(
  id,
  line_arb,
  line_arb_label,
  line_arb_color,
  title = "Arbitrary horizontal lines:"
)
```

## Arguments

- id:

  (`character(1)`)  
  defining namespace of the `shiny` module.

- line_arb:

  (`numeric`)  
  default values for the `textInput` defining values of arbitrary lines

- line_arb_label:

  (`character`)  
  default values for the `textInput` defining labels of arbitrary lines

- line_arb_color:

  (`character`)  
  default values for the `textInput` defining colors of arbitrary lines

- title:

  (`character(1)`)  
  title of the arbitrary lines input. The default is "Arbitrary
  Horizontal Lines".

## Value

(`shiny.tag`) an input to define values, colors and labels for arbitrary
straight lines.
