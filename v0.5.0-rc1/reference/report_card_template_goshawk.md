# Template Function for `TealReportCard` Creation and Customization in `teal.goshawk`

This function generates a report card with a title, an optional
description, and the option to append the filter state list.
Additionally, it display selected constraint options.

## Usage

``` r
report_card_template_goshawk(
  title,
  label,
  with_filter,
  filter_panel_api,
  constraint_list,
  constraint_description = NULL,
  style = "default"
)
```

## Arguments

- title:

  (`character(1)`) title of the card (unless overwritten by label)

- label:

  (`character(1)`) label provided by the user when adding the card

- with_filter:

  (`logical(1)`) flag indicating to add filter state

- filter_panel_api:

  (`FilterPanelAPI`) object with API that allows the generation of the
  filter state in the report

- constraint_list:

  (`list`) a list containing constraint variables, including:

  - constraint_var (`character(1)`) the constraint variable name.

  - constraint_range_min (`numeric(1)`) the minimum constraint range
    value.

  - constraint_range_max (`numeric(1)`) the maximum constraint range
    value.

- constraint_description:

  (`character(1)`) description of the constraints.

- style:

  (`character(1)`) style of the constraint text block. options:
  `default`, `verbatim` (default is `default`).

## Value

(`TealReportCard`) populated with a title, description, and filter state
