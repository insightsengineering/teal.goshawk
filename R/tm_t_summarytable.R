#' Teal module to generate a table of descriptive summary statistics table to accompany plots
#'
#' give more details about the method
#'
#' @param label label of teal module in app
#'
#' @author Balazs Toth
#' @author Nick Paszty
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(random.cdisc.data)
#' library(dplyr)
#' ASL <- rnorm(400)
#'
#' ASL$BWT <- rnorm(400)
#' ASL$VARNUM <- rnorm(400)
#'
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_g_lineplot(
#'        label = "Summary Table",
#'        dataname = "ASL",
#'        var = c("AGE", "BWT"),
#'        var_choices = which(sapply(ASL, is.numeric)) %>% names()
#'    )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#'
#' }
#'
#'

require(dplyr)
require(gridExtra)

# UPDATE: function needs to be tealified
tm_t_summarytable <- function(data,
                           biomarker,
                           trt_group,
                           time,
                           loq) {
  
  # Helper
  sum_data <- data %>% 
    group_by(eval(parse(text = time)),
             eval(parse(text = trt_group))) %>%
    summarise(n = sum(!is.na(eval(parse(text = biomarker)))),
              avg = round(mean(eval(parse(text = biomarker)),
                            na.rm = TRUE),1),
              med = round(median(eval(parse(text = biomarker)),
                              na.rm = TRUE),1),
              sd = round(sd(eval(parse(text = biomarker)),
                         na.rm = TRUE),1),
              pctLOQ = round(100 * sum(eval(parse(text = loq))=='YES')/
                            length(eval(parse(text = loq))=='YES'),2))
  
  tbl <- tableGrob(t(sum_data))
  grid.arrange(arrangeGrob(tbl, ncol=1, nrow=1))

}
