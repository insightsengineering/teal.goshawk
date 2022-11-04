#' helper for writing arm mapping and ordering code.
#'
#' Provides lines of code for left hand side of arm mapping. user must provide right hand side
#'
#' @details SPA configure study specific pre-processing for deploying goshawk. writing the code for ARM mapping and
#' ordering is tedious. this function helps to get that started by providing the left hand side of the
#' mapping and ordering syntax. call the function and then copy and paste the resulting code from the console
#' into the app.R file.
#'
#' @param df_armvar the dataframe and column name containing treatment code. e.g. ADSL$ARMCD
#' @param code controls whether mapping or ordering code is written to console. Valid values: "M" and "O".
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' # get treatment mapping code
#' maptrt(df_armvar = ADSL$ARMCD, code = "M")
#'
#' # get treatment ordering code
#' maptrt(df_armvar = ADSL$ARMCD, code = "O")
maptrt <- function(df_armvar, code = c("M", "O")) {
  code <- match.arg(code)

  # get arm variable
  trtvar <- strsplit(deparse(substitute(df_armvar)), "[$]")[[1]][2]

  dftrt <- data.frame(unique(df_armvar)) %>%
    dplyr::mutate(trt_mapping = paste0("\"", unique(df_armvar), "\"", " = \"\",")) %>%
    dplyr::mutate(trt_ordering = paste0(eval(trtvar), " == \"", unique(df_armvar), "\"", " ~  ,"))

  if (toupper(code) == "M") {
    print(unname(dftrt["trt_mapping"]), row.names = FALSE)
  } else if (toupper(code) == "O") {
    print(unname(dftrt["trt_ordering"]), row.names = FALSE)
  }
}