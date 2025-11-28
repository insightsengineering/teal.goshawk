# Server module to arbitrary lines

Server to validate and transform the comma separated values into vectors
of values to be passed into goshawk functions.

## Usage

``` r
srv_arbitrary_lines(id)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

## Value

(`reactive`) returning a `list` containing `line_arb`, `line_arb_color`,
`line_arb_label` which are validated and could be passed to `goshawk`
plot functions.
