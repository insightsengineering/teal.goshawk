# nolint start
get_test_data <- function() {
  data <- teal_data()
  data <- within(data, {
    library(dplyr)
    library(nestcolor)
    library(stringr)

    # use non-exported function from goshawk
    h_identify_loq_values <- getFromNamespace("h_identify_loq_values", "goshawk")

    # original ARM value = dose value
    arm_mapping <- list(
      "A: Drug X" = "150mg QD",
      "B: Placebo" = "Placebo",
      "C: Combination" = "Combination"
    )
    set.seed(1)
    ADSL <- rADSL
    ADLB <- rADLB
    var_labels <- lapply(ADLB, function(x) attributes(x)$label)
    ADLB <- ADLB %>%
      mutate(
        AVISITCD = case_when(
          AVISIT == "SCREENING" ~ "SCR",
          AVISIT == "BASELINE" ~ "BL",
          grepl("WEEK", AVISIT) ~ paste("W", str_extract(AVISIT, "(?<=(WEEK ))[0-9]+")),
          TRUE ~ as.character(NA)
        ),
        AVISITCDN = case_when(
          AVISITCD == "SCR" ~ -2,
          AVISITCD == "BL" ~ 0,
          grepl("W", AVISITCD) ~ as.numeric(gsub("[^0-9]*", "", AVISITCD)),
          TRUE ~ as.numeric(NA)
        ),
        AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN),
        TRTORD = case_when(
          ARMCD == "ARM C" ~ 1,
          ARMCD == "ARM B" ~ 2,
          ARMCD == "ARM A" ~ 3
        ),
        ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))]),
        ARM = factor(ARM) %>% reorder(TRTORD),
        ACTARM = as.character(arm_mapping[match(ACTARM, names(arm_mapping))]),
        ACTARM = factor(ACTARM) %>% reorder(TRTORD),
        ANRLO = 50,
        ANRHI = 75
      ) %>%
      rowwise() %>%
      group_by(PARAMCD) %>%
      mutate(LBSTRESC = ifelse(
        USUBJID %in% sample(USUBJID, 1, replace = TRUE),
        paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
      )) %>%
      mutate(LBSTRESC = ifelse(
        USUBJID %in% sample(USUBJID, 1, replace = TRUE),
        paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
      )) %>%
      ungroup()

    attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
    attr(ADLB[["ACTARM"]], "label") <- var_labels[["ACTARM"]]
    attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
    attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"

    # add LLOQ and ULOQ variables
    ALB_LOQS <- h_identify_loq_values(ADLB, "LOQFL")
    ADLB <- left_join(ADLB, ALB_LOQS, by = "PARAM")
  })
  datanames <- c("ADSL", "ADLB")
  datanames(data) <- datanames
  join_keys(data) <- default_cdisc_join_keys[datanames]
  data
}
# nolint end
