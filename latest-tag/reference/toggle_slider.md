# UI with a toggleable dichotomous slider to change between slider and numeric input fields

This is useful when a slider should be shown, but it is sometimes hard
to configure sliders, so one can toggle to one or two numeric input
fields to set slider instead. The toggle button will show two numeric
input field for selecting the from and to range.

## Usage

``` r
toggle_slider_ui(id, label)

toggle_slider_server(id, data_state, ...)

get_data_range_states(varname, paramname, ANL, trt_group = NULL, step = NULL)
```

## Arguments

- id:

  `character` module id

- label:

  `label` label for input field, e.g. slider or numeric inputs

- ...:

  additional parameters to pass to `sliderInput`

## Value

`toggle_slider_ui` returns a `shiny.tag` object with the UI for the
toggleable slider module. `toggle_slider_server` returns a shiny module
which itself returns a `reactiveValues` object with `min`, `max`, and
`value` elements.

## Examples

``` r
if (interactive()) {
  shinyApp(
    ui = fluidPage(toggle_slider_ui("slider", "Select range")),
    server = function(input, output, session) {
      data_state <- reactiveVal(
        list(range = c(0, 100), step = 1)
      )
      toggle_slider_server("slider", data_state)
    }
  )
}
```
