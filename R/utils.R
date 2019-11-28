

#' @export
goshawk_data <- function() {

  # original ARM value = dose value
  arm_mapping <- list("A: Drug X" = "150mg QD",
                      "B: Placebo" = "Placebo",
                      "C: Combination" = "Combination")
  
  ADSL <- radsl(N = 20, seed = 1)
  ADLB <- radlb(ADSL, visit_format = "WEEK", n_assessments = 7L, seed = 2)
  ADLB <- ADLB %>%
    mutate(AVISITCD = case_when(
      AVISIT == "SCREENING" ~ "SCR",
      AVISIT == "BASELINE" ~ "BL",
      grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
      TRUE ~ as.character(NA)),
      AVISITCDN = case_when(
        AVISITCD == "SCR" ~ -2,
        AVISITCD == "BL" ~ 0,
        grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
        TRUE ~ as.numeric(NA)),
      AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
      TRTORD = case_when(
        ARMCD == "ARM C" ~ 1,
        ARMCD == "ARM B" ~ 2,
        ARMCD == "ARM A" ~ 3),
      ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
      ARM = factor(ARM) %>% reorder(TRTORD))  
  
  list(ADSL = ADSL, ADLB = ADLB, code = '
 arm_mapping <- list("A: Drug X" = "150mg QD",
                      "B: Placebo" = "Placebo",
                      "C: Combination" = "Combination")
  
  ADSL <- radsl(N = 20, seed = 1)
  ADLB <- radlb(ADSL, visit_format = "WEEK", n_assessments = 7L, seed = 2)
  ADLB <- ADLB %>%
    mutate(AVISITCD = case_when(
      AVISIT == "SCREENING" ~ "SCR",
      AVISIT == "BASELINE" ~ "BL",
      grepl("WEEK", AVISIT) ~ paste("W", stringr::str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
      TRUE ~ as.character(NA)),
      AVISITCDN = case_when(
        AVISITCD == "SCR" ~ -2,
        AVISITCD == "BL" ~ 0,
        grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
        TRUE ~ as.numeric(NA)),
      AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
      TRTORD = case_when(
        ARMCD == "ARM C" ~ 1,
        ARMCD == "ARM B" ~ 2,
        ARMCD == "ARM A" ~ 3),
      ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
      ARM = factor(ARM) %>% reorder(TRTORD))         
       ' )
}



keep_range_slider_updated <- function(session, input, id_slider, id_var, reactive_ANL) {
  
  observe({
    ANL <- reactive_ANL()$ANL
    varname <- input[[id_var]]
    
    
    validate(need(varname, "Please select variable"))
    validate_has_variable(ANL, varname, paste("variable", varname, "does not exist"))
    
    vals <- ANL[[varname]]
    
    minmax <- c(
      floor(min(if_empty(na.omit(vals), 0))),
      ceiling(max(if_empty(na.omit(vals), 0)))
    )
    
    updateSliderInput(
      session = session,
      inputId = id_slider,
      min = minmax[1], 
      max = minmax[2], 
      value = minmax
    )
  })
}

constr_anl_chunks <- function(session, input, datasets, dataname, param_var, trt_group) {
  
  
  dataset_var <- paste0(dataname, "_FILTERED")
  
  ANL_param <- reactive({
    
    # resolve reactive values
    param <- input$param
    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE) # nolint
    
    # validate
    validate(need(param, "Please select a biomarker"))
    validate_has_data(ANL_FILTERED, 5)
    
    validate_has_variable(ANL_FILTERED, "AVISITCD") # should have BL and SCR levels
    validate_has_variable(ANL_FILTERED, param_var)
    validate_has_variable(ANL_FILTERED, "BASE")
    validate_has_variable(ANL_FILTERED, "BASE2")
    validate_has_variable(ANL_FILTERED, "ARM")
    
    
    # analysis
    private_chunks <- chunks$new()
    chunks_reset(as.environment(setNames(list(ANL_FILTERED), dataset_var)), private_chunks)
    
    # filter biomarker
    chunks_push(
      chunks = private_chunks,
      id = "filter_biomarker",
      expression = bquote({
        ANL <- .(as.name(dataset_var)) %>% # nolint
          dplyr::filter(.(as.name(param_var)) == .(param))
      }) 
    )
    
    ANL <- chunks_safe_eval(private_chunks) # to get ANL
    validate_has_data(ANL, 5)
    
    list(ANL = ANL, chunks = private_chunks)
  })
  
  
  observe({
    
    constraint_var <- input$constraint_var
    ANL <- ANL_param()$ANL
    
    validate(need(constraint_var, "select a constraint variable"))
    
    # get min max values
    args <- if (constraint_var == "NONE") {
      
      shinyjs::hide("constraint_range")
      
      list(
        min = list(label = "Min", min = 0, max = 0, value = 0),
        max = list(label = "Max", min = 0, max = 0, value = 0)
      )
      
    } else {  # BASE or BASE2
      
      shinyjs::show("constraint_range")
      
      rng <- switch(
        constraint_var,
        "BASE" = range(ANL$BASE[ANL$AVISITCD == "BL"], na.rm = TRUE),
        "BASE2" = range(ANL$BASE2[ANL$AVISITCD == "SCR"], na.rm = TRUE),
        stop(paste(constraint_var, "not allowed"))
      ) 
      
      minmax <- c( floor(rng[1] * 1000) / 1000,  ceiling(rng[2] * 1000) / 1000) 
      
      label_min <- sprintf("Min (%s)", minmax[1])
      label_max <- sprintf("Max (%s)", minmax[2])
      
      list(
        min = list(label = label_min, min = minmax[1], max = minmax[2], value = minmax[1]),
        max = list(label = label_max, min = minmax[1], max = minmax[2], value = minmax[2])
      )
    }
    
    do.call("updateNumericInput", c(list(session = session, inputId = "constraint_range_min"), args$min))
    do.call("updateNumericInput", c(list(session = session, inputId = "constraint_range_max"), args$max))
    
  })
  
  anl <- reactive({
    
    # it is assumed that constraint_var is triggering constraint_range which then trigger this clause
    # that's why it's not listed in triggers
    
    constraint_var <- isolate(input$constraint_var)
    constraint_range_min <- input$constraint_range_min
    constraint_range_max <- input$constraint_range_max
    
    ANL_param <- ANL_param()
    
    ANL <- ANL_param$ANL
    private_chunks <- ANL_param$chunks$clone(deep = TRUE)
    
    validate(need(constraint_range_min, "please select proper constraint minimum value"))
    validate(need(constraint_range_max, "please select proper constraint maximum value"))
    
    # filter constraint
    if (constraint_var != "NONE") {
      
      validate(need(constraint_range_min < constraint_range_max, "constraint min needs to be smaller than max"))
      
      chunks_push(
        chunks = private_chunks,
        id = "filter_constraint",
        expression = bquote({
          ANL <- ANL %>% # nolint
            dplyr::filter(
              (.(constraint_range_min) <= .(as.name(constraint_var)) &
                 .(as.name(constraint_var)) <= .(constraint_range_max)) |
                is.na(.(as.name(constraint_var)))
            )
        })
      )
      
      ANL <- chunks_safe_eval(private_chunks)
    }
    
    validate_has_data(ANL, 5)
    
    arm_label <- if (trt_group == "ARM") "Planned Arm" else "Actual Arm"
    
    chunks_push(
      chunks = private_chunks,
      id = "change_attr", 
      expression = bquote(attr(ANL$ARM, "label") <- .(arm_label))
    )
    
    chunks_push_new_line(private_chunks)
    chunks_safe_eval(private_chunks)
    
    list(ANL = chunks_get_var("ANL", private_chunks), chunks = private_chunks)
  })
  
  anl
  
}
