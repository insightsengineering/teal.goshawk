# UI and server module to arbitrary lines

UI module to input either horizontal or vertical lines to a plot via
comma separated values

Server to validate and transform the comma separated values into vectors
of values to be passed into goshawk functions.

## Usage

``` r
ui_arbitrary_lines(
  id,
  line_arb,
  line_arb_label,
  line_arb_color,
  title = "Arbitrary horizontal lines:"
)

srv_arbitrary_lines(id)
```

## Arguments

- id:

  (`character(1)`)\
  defining namespace of the `shiny` module.

- line_arb:

  (`numeric`)\
  default values for the `textInput` defining values of arbitrary lines

- line_arb_label:

  (`character`)\
  default values for the `textInput` defining labels of arbitrary lines

- line_arb_color:

  (`character`)\
  default values for the `textInput` defining colors of arbitrary lines

- title:

  (`character(1)`)\
  title of the arbitrary lines input. The default is "Arbitrary
  Horizontal Lines".

## Value

- `ui_arbitrary_lines`: (`shiny.tag`) an input to define values, colors
  and labels for arbitrary straight lines.

&nbsp;

- `srv_arbitrary_lines`: (`reactive`) returning a `list` containing
  `line_arb`, `line_arb_color`, `line_arb_label` which are validated and
  could be passed to `goshawk` plot functions.

## Examples

``` r
if (interactive()) {
  shinyApp(
    ui = fluidPage(
      ui_arbitrary_lines(
        id = "arbitrary_lines",
        line_arb = c(1, 2, 3),
        line_arb_color = c("red", "blue", "green"),
        line_arb_label = c("Line 1", "Line 2", "Line 3"),
        title = "Arbitrary horizontal lines:"
      ),
      verbatimTextOutput("result"),
    ),
    server = function(input, output, session) {
      result <- srv_arbitrary_lines("arbitrary_lines")
      output$result <- renderPrint({
        req(result())
        result()$iv()$validate()
        result()[c("line_arb", "line_arb_color", "line_arb_label")]
      })
    }
  )
}
```
