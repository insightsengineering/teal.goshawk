#' Teal Module: moduel output type (e.g. scatter plot)
#'
#' This shiny module displays a ...
#'
#' @param add as many param lines as there are params that this function expects
#' @param add as many param lines as there are params that this function expects
#'
#' @inheritParams teal::standard_layout
#'
#' @author Balazs Toth
#' @author first last
#'
#' @details provide additional information as needed. link to specification file \url{http://rstudio.com}
#'
#' @return an \code{\link[teal]{module}} object#'
#'
#' @export
#'
#' @examples
#' # Example using analysis dataset for example ASL or ADSL,
#' # ABM points to biomarker data stored in a custom file created to support goshawk. for example ADBIOM
#' library(dplyr)
#'
#' data("ASL")
#' data("ABM")
#'
#' ASL <- ASL
#' AAE <- ABM
#'
#' x1 <- teal::init(
#'   data = list(ASL = ASL, ABM = ABM),
#'   modules = root_modules(
#'     tm_g/t_module_name(
#'        label = "module label",
#'        dataname = "ABM",
#'        filter_var = NULL,
#'        filter_var_choices = c(NULL, "DTHFL", "flag1"),
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        etc. this list of parameters much match the list in the analogous function file
#'    )
#'   )
#' )
#'
#' shinyApp(x1$ui, x1$server)
#'
#'
tm_g/t_module_name <- function(label,
                    dataname,
                    filter_var = NULL,
                    filter_var_choices = NULL,
                    arm_var,
                    arm_var_choices,
                    pre_output = NULL,
                    post_output = NULL,
                    code_data_processing = NULL) {

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g/t_function_name,
    ui = ui_g/t_function_name,
    ui_args = args,
    server_args = list(dataname = dataname, code_data_processing = code_data_processing),
    filters = dataname
  )

}

ui_g/t_function_name <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = whiteSmallWell(uiOutput(ns("name_of_output"))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("filter_var"), "Preset Data Filters", a$filter_var_choices, a$filter_var, multiple = TRUE),
      optionalSelectInput(ns("arm_var"), "Arm Variable", a$arm_var_choices, a$arm_var, multiple = FALSE),
      # add additional input statements here
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}

srv_g/t_function_name <- function(input, output, session, datasets, dataname, code_data_processing) {

  # related to generating the R code via "Show R Code" button
  chunks <- list(
    vars = "# Not Calculated",
    data = "# Not Calculated",
    analysis = "# Not Calculated"
  )

  output$table <- renderUI({

    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    ABM_FILTERED <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    filter_var <- input$filter_var
    arm_var <- input$arm_var
    # add additional assignment statements here

    chunks$vars <<- bquote({
      arm_var <- .(arm_var)
      # add additional assignment statements here
      filter_var <- .(filter_var)
    })

    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var))
    abm_vars <- unique(c("USUBJID", "STUDYID", arm_var, ))

    chunks$data <<- bquote({
      ASL <- ASL_FILTERED[, .(asl_vars)] %>% as.data.frame()

      if(!("NULL" %in% .(filter_var)) && !is.null(.(filter_var))){
        ABM <- quick_filter(.(filter_var), ABM_FILTERED) %>% droplevels()
      } else{
        ABM <- ABM_FILTERED
      }

      ABM <- ABM[, .(abm_vars)] %>% as.data.frame()

      # may not need this block for the goshawk app
      #ANL  <- left_join(ASL, ABM, by = c("USUBJID", "STUDYID", .(arm_var))) %>%
      #  as.data.frame()

      #if(all_p == TRUE){
      #  total = "All Patients"
      #} else{
      #  total = NULL
      #}
    })
    eval(chunks$data)

    # may not need this block for the goshawk app
    #chunks$analysis <<- call(
    #  "g/t_function_name",
    #  class = bquote(ANL[, class_var]),
    #  term = bquote(ANL[, term_var]),
    #  id = bquote(ANL$USUBJID),
    #  col_by = bquote(as.factor(ANL[[.(arm_var)]])),
    #  total = bquote(total)
    #)

    tbl <- try(eval(chunks$analysis))

    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate the table:\n\n", tbl)))

    as_html(tbl)
  })


  observeEvent(input$show_rcode, {

    header <- get_rcode_header(
      title = "output title",
      datanames = dataname,
      datasets = datasets,
      code_data_processing
    )

    str_rcode <- paste(c(
      "",
      header,
      "",
      remove_enclosing_curly_braces(deparse(chunks$vars, width.cutoff = 60)),
      "",
      remove_enclosing_curly_braces(deparse(chunks$data, width.cutoff = 60)),
      "",
      remove_enclosing_curly_braces(deparse(chunks$analysis, width.cutoff = 60))
    ), collapse = "\n")

    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current < add output context here>",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })

}
