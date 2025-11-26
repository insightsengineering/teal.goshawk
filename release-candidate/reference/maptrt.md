# helper for writing arm mapping and ordering code.

Provides lines of code for left hand side of arm mapping. user must
provide right hand side

## Usage

``` r
maptrt(df_armvar, code = c("M", "O"))
```

## Arguments

- df_armvar:

  the dataframe and column name containing treatment code. e.g.
  `ADSL$ARMCD`

- code:

  controls whether mapping or ordering code is written to console. Valid
  values: `"M"` and `"O"`.

## Details

SPA configure study specific pre-processing for deploying `goshawk`.
writing the code for `ARM` mapping and ordering is tedious. this
function helps to get that started by providing the left hand side of
the mapping and ordering syntax. call the function and then copy and
paste the resulting code from the console into the app.R file.

## Examples

``` r
ADSL <- rADSL

# get treatment mapping code
maptrt(df_armvar = ADSL$ARMCD, code = "M")
#>               
#>  "ARM A" = "",
#>  "ARM C" = "",
#>  "ARM B" = "",

# get treatment ordering code
maptrt(df_armvar = ADSL$ARMCD, code = "O")
#>                       
#>  ARMCD == "ARM A" ~  ,
#>  ARMCD == "ARM C" ~  ,
#>  ARMCD == "ARM B" ~  ,
```
