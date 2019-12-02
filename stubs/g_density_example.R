#' 1
# Example using ADaM structure analysis dataset.

library(random.cdisc.data)

# original ARM value = dose value
arm_mapping <- list("A: Drug X" = "150mg QD",
                    "B: Placebo" = "Placebo",
                    "C: Combination" = "Combination")

#ADSL <- radsl(N = 20, seed = 1)
#ADLB <- radlb(ADSL, visit_format = "WEEK", n_assessments = 7L, seed = 2)
ADSL <- radsl(cached = TRUE)
ADLB <- radlb(cached = TRUE)
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
    TRTORD = case_when(
      ARMCD == "ARM C" ~ 1,
      ARMCD == "ARM B" ~ 2,
      ARMCD == "ARM A" ~ 3),
    ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
    ARM = factor(ARM) %>% reorder(TRTORD))


devtools::load_all(); app <- teal::init(
  data = cdisc_data(
    cdisc_dataset("ADSL", ADSL),
    cdisc_dataset("ADLB", ADLB),
    code = {'
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
          TRTORD = case_when(
            ARMCD == "ARM C" ~ 1,
            ARMCD == "ARM B" ~ 2,
            ARMCD == "ARM A" ~ 3),
          ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
          ARM = factor(ARM) %>% reorder(TRTORD))
          '},
    check = FALSE
  ),
  modules = root_modules(
    tm_g_density_distribution_plot(
      label = "Density Distribution Plot",
      dataname = "ADLB",
      param_var = "PARAMCD",
      param = choices_selected(c("ALT", "CRP", "IGA"), "ALT"),
      xaxis_var = choices_selected(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
      trt_group = "ARM",
      color_manual = c("150mg QD" = "#000000",
                       "Placebo" = "#3498DB",
                       "Combination" = "#E74C3C"),
      color_comb = "#39ff14",
      plot_height = c(500, 200, 2000),
      font_size = c(12, 8, 20),
      line_size = c(1, .25, 3)
    )
  )
); shinyApp(app$ui, app$server)



#appWorking <- app
