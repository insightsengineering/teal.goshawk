#.libPaths(c(file.path(getwd()), "~/goshawk", .libPaths()))
#goshawk:::g_lineplot
# required packages
library(DescTools) # for %% operators e.g. %like any%
library(dplyr)
library(ggplot2)
library(goshawk)
library(grid)
library(gridExtra)
library(stringr)
library(teal)
library(teal.goshawk)

#`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

################################################################################
# BEGIN: SPA Input Required
################################################################################

# study root path under qa branch - used for determining access
access_path <- "/opt/BIOSTAT/qa/cdt7738a"

# absolute path and data det name
ASL_path <- "/opt/BIOSTAT/qa/cdt7738a/s30044m/libraries/asl.sas7bdat"
ALB_path <- "/opt/BIOSTAT/qa/cdt7738a/s30044m/libraries/alb.sas7bdat"

# for development team testing
# ASL_path <- "/opt/bee/home_nas/npaszty/btk/lupus/dataadam/asl.sas7bdat"
# ALB_path <- "/opt/bee/home_nas/npaszty/btk/lupus/dataadam/alb3arm.sas7bdat"

# study information
MOLECULE <- "BTK"
INDICATION <- "Lupus"
STUDY <- "GA30044"

# protocol URL in IDM Direct system
protocol_url <- c('https://idmdirect.roche.com/rnc-webservices/document/0900323e8a3f702b/lifecycle')

# analysis type e.g. Interim Analysis, Final Analysis etc.
ATYPE <- "Interim Analysis"

# assign treatment colors to match those used in SPA study output
color_manual = c('Placebo' = "#000000", '150mg QD' = "#3498DB", '200mg BID' = "#E74C3C")

# assign LOQ flag symbols: circles and triangles respectively
shape_manual = c('N' = 1, 'Y' = 2, 'NA' = 0)

# list of biomarkers of interest. see ALB1 assignment below
param_choices <- c("ACIGG", "ACIGM", "ADIGG", "ANAPC", "ANAPDS", "ANAPH", "ANAPM", "ANAPND", "ANAPP", "ANAPR", "ANAPS", "ANAPSP", "ASSBABC", "AVCT_JMT",
                   "B2G1GAAB", "B2GLYIGG", "B2GLYIGM", "BASOSF", "BTKP", "BTKT",
                   "C3", "C4S", "CCL20", "CCL3", "CCL4", "CD19A", "CD19P", "CD3A", "CD3P", "CD4A", "CD4P", "CD8A", "CD8P", "CRP",
                   "DLCT_JCH", "DLCT_MZB", "DLCT_TXN",
                   "IGG", "IGM",
                   "NLBC_JCH", "NLBC_MZB", "NLBC_TXN",
                   "RNPAABC",  "RWCT_JCH", "RWCT_MZB", "RWCT_TME", "RWCT_TXN",
                   "SSAAABC")

# biomarkers of interest to exclude from performing log2 calculation: 
# NLBC are already log2 transformed: assigned AVAL to AVALL2
exclude_l2 <- c("NLBC_JCH", "NLBC_MZB", "NLBC_TXN")
# DLCT are CHG values, AVCT is average CHG: assigned NA
exclude_chg <- c("DLCT_JCH", "DLCT_MZB", "DLCT_TXN", "AVCT_JMT")

################################################################################
# END: SPA Input Required
################################################################################
################################################################################
# BEGIN: Generic Data Post Processing
################################################################################

# post process the ASL and ALB data to subset records per specification
ASL <- read_bce(ASL_path) %>%
  filter(ITTFL == 'Y' & IAFL == 'Y')

ALB_SUBSET <- read_bce(ALB_path) %>%
  filter(PARAMCD %in% c(param_choices) & ITTFL == 'Y' & IAFL == 'Y' & ANLFL == 'Y' & AVISIT %like any% c('SCREEN%', 'BASE%','%WEEK%')) %>%
  select(c('STUDYID', 'USUBJID',
           'ITTFL', 'ANLFL', 'ABLFL2', 
           'ARM', 'ARMCD', 
           'AVISIT', 'AVISITN', 
           'PARAM','PARAMCD', 
           'AVAL', 'AVALU', 'BASE', 'CHG', 'PCHG', 'BASE2', 'CHG2', 'PCHG2',
           'LBSTRESC', 'LBSTRESN',
           'LOQFL'))

# identify the minimum non-zero value for AVAL for each PARAMCD.
# non-zero minimum value used for log2 transformed analysis values
PARAM_MINS <- ALB_SUBSET %>%
  select(USUBJID, PARAMCD, AVAL) %>%
  filter(PARAMCD %in% param_choices) %>%
  group_by(PARAMCD) %>%
  summarise(AVAL_MIN=min(AVAL, na.rm=TRUE))

# post process the data to create several new variables and adjust existing record specific valules per specification
# - create a visit code variable - week records coded to "W NN"
# - adjust existing BASELINE record values where values are missing: According to SPA this is a STREAM artifact
ALB_SUPED1 <- ALB_SUBSET %>% 
  mutate(AVISITCD = case_when(
    AVISIT == 'SCREENING' ~ 'SCR',
    AVISIT == 'BASELINE' ~ 'BL',
    grepl("WEEK", AVISIT) ~ paste("W",trimws(substr(trimws(AVISIT), start=6, stop=7))),
    TRUE ~ as.character(NA)
  )) %>%
  mutate(AVISITCDN =  ifelse(trimws(AVISITCD) == "BL", 0, substr(AVISITCD, start=2, stop=4))) %>%
  mutate(AVISITCDN =  as.numeric(ifelse(trimws(AVISITCD) == "SCR", -1, AVISITCDN))) %>%
  
  mutate(BASE2 = ifelse(AVISIT == "SCREENING" & is.na(BASE2), AVAL, BASE2)) %>%
  mutate(CHG2 = ifelse(AVISIT == "SCREENING" & is.na(CHG2), 0, CHG2)) %>%
  mutate(PCHG2 = ifelse(AVISIT == "SCREENING" & is.na(PCHG2), 0, PCHG2)) %>%
  
  mutate(BASE = ifelse(AVISIT == "BASELINE" & is.na(BASE), AVAL, BASE)) %>%
  mutate(CHG = ifelse(AVISIT == "BASELINE" & is.na(CHG), 0, CHG)) %>%
  mutate(PCHG = ifelse(AVISIT == "BASELINE" & is.na(PCHG), 0, PCHG)) %>%
  
  mutate(TRTORD = ifelse(grepl("C", ARMCD), 3, ifelse(grepl("B", ARMCD), 2, ifelse(grepl("A", ARMCD), 1, NA))))

# merge minimum AVAL value onto the ALB data to calculate the log2 variables. preserve the variable order
ALB_SUPED2 <- merge(PARAM_MINS, ALB_SUPED1, by="PARAMCD", all=TRUE)[, union(names(ALB_SUPED1), names(PARAM_MINS))] %>%
  # visit values
  mutate(AVALL2 = ifelse(PARAMCD %in% exclude_l2, AVAL, # excludes biomarkers where log2 is not appropriate: for example assay value already log2
                         ifelse(PARAMCD %in% exclude_chg, NA, # excludes biomarkers where log2 is not appropriate: for example CHG type assay
                                ifelse(AVAL == 0 & AVAL_MIN > 0, log2(AVAL_MIN/2),
                                       ifelse(AVAL == 0 & AVAL_MIN <= 0, NA, # would be taking log2 of 0 or negative value so set to NA
                                              ifelse(AVAL > 0, log2(AVAL), NA)))))) %>%
  # baseline values
  mutate(BASEL2 = ifelse(PARAMCD %in% exclude_l2, BASE,
                         ifelse(PARAMCD %in% exclude_chg, NA,     
                                ifelse(BASE == 0 & AVAL_MIN > 0, log2(AVAL_MIN/2),
                                       ifelse(BASE == 0 & AVAL_MIN <= 0, NA,
                                              ifelse(BASE > 0, log2(BASE), NA)))))) %>%
  # screening
  mutate(BASE2L2 = ifelse(PARAMCD %in% exclude_l2, BASE2,
                          ifelse(PARAMCD %in% exclude_chg, NA,      
                                 ifelse(BASE2 == 0 & AVAL_MIN > 0, log2(AVAL_MIN/2),
                                        ifelse(BASE2 == 0 & AVAL_MIN <= 0, NA,
                                               ifelse(BASE2 > 0, log2(BASE2), NA))))))

# create final data set used by goshawk
ALB <- ALB_SUPED2 %>% 
  mutate(AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN)) %>%
  mutate(ARMORVAL = ARM) %>%
  mutate(ARM = case_when(
    ARMCD == "C" ~ "Placebo",
    ARMCD == "B" ~ "150mg QD",
    ARMCD == "A" ~ "200mg BID",
    TRUE ~ as.character(NA))) %>%
  mutate(ARM = factor(ARM) %>% reorder(TRTORD)) %>% # need explicit 'N' value for LOQFL
  mutate(LOQFL = ifelse(LOQFL == 'Y', LOQFL, 'N'))

################################################################################
# END: Generic Data Post Processing
################################################################################

# create ASL metadata for Source Data tab
adsl <- file.info(ASL_path)

# get number of variables
sdsl_nvars <- format(names(ASL), big.mark = ",")

# get number of subjects
sdsl_nsubjs <- ASL %>%
  pull(USUBJID) %>%
  unique()

# create ALB metadata for Source Data tab
adbm <- file.info(ALB_path)

# get number of variables
sdbm_nvars <- format(names(ALB), big.mark = ",")

# get number of subjects
sdbm_nsubjs <- ALB %>%
  pull(USUBJID) %>%
  unique()

# create biomarkers of interest dictionary table to display to user
paramDict <- unique(ALB_SUBSET[c("PARAM", "PARAMCD")])
paramDict <- paramDict[order(paramDict$PARAM), ]
param_list <- paramDict$PARAM
paramcd_list <- paramDict$PARAMCD

x <- teal::init(
  data =  list(ASL = ASL, ALB = ALB),
  modules = root_modules(
    
    module(
      "User Guide",
      server = function(input, output, session, datasets) {},
      ui = function(id) div(
        h5(
          strong("Analysis Variable Name Legend:"), actionLink("showAnlVarLegendModal", tags$img(height=15, width=15, src="img/qmark.png"))
        ),
        tags$ul(
          h6(
            p("BASE2 = Screening Visit Value"),
            p("BASE2L2 = Log2(BASE2)"),
            p("CHG2 = Change from Screening"),
            p("PCHG2 =  %Change from Screening"),
            p("BASE = Baseline Visit Value"),
            p("BASEL2 = Log2(BASE)"),
            p("CHG = Change from Baseline"),
            p("PCHG =  %Change from Baseline"),
            p("AVAL = Visit Values"),
            p("AVALL2 = Log2(AVAL)")
          )
        ),
        br(),
        h5(
          strong("Treatment Variable:")
        ),
        p(tags$ul(
          h6(
            "The 'Description of Planned Arm' (ARM) variable is used to reflect ITT treatment for these biomarker visualizations."
          )
        )),
        h5(
          strong("Log2 Variables:")
        ),
        p(tags$ul(
          h6(
            "Some biomarker values are already log2 transformed. These are excluded from log2 transform applied to biomarkers at large. ",
            p("Biomarkers having 0 values are log2 transformed by taking log2 of the minimum non-zero value divided by 2.")
          )
        )),
        h5(
          strong("Data Filters:")
        ),
        p(tags$ul(
          h6(
            "Use the AVISIT variable in the ALB filter to include specific visits in visualizations.")
        )),
        h5(
          strong("Visualization Specifics:")
        ),
        p(tags$ul(
          h6(
            strong("Scatter Plot:")
          )
        )),
        p(tags$ul(tags$ul(
          h6("The 'Regression Line' option should only be used in conjunction with the 'Treatment Facetting' option.
                                    Otherwise the per treatment regression formula and coefficient annotations will overlay."
          )
        )))
      ),
      
      filters = NULL
    ),
    
    module(
      "Source Data",
      server = function(input, output, session, datasets) {},
      ui = function(id) div(
        h5(strong("Molecule:"), MOLECULE),
        h5(strong("Indication:"), INDICATION),
        h5(strong("Study:"), STUDY, actionLink("showProtocolModal", tags$img(height=15, width=15, src="img/qmark.png"))),
        h5(strong("Analysis Type:"), ATYPE),
        tags$ul(
          h6(
            p(strong("Subject Level Data Set:"), ASL_path),
            #p(strong("Data Set Label:")),
            p(strong("Data Set Owner:"), adsl$uname),
            p(strong("Data Set Creation DateTime:"), adsl$mtime),
            p(strong("Number of  Subjects:"), format(length(sdsl_nsubjs), big.mark = ",")),
            p(strong("Number of Variables:"), format(length(names(ASL)), big.mark = ",")),
            p(strong("Number of Records:"), format(nrow(ASL), big.mark = ",")),
            br(),
            p(strong("Biomarker Data Set:"), ALB_path),
            #p(strong("Data Set Label:")),
            #p(strong("Data Set Owner:"), a(href = paste0('"https://roche.jiveon.com/people/', adbm$uname, '@gene.com"'), adbm$uname)),
            p(strong("Data Set Owner:"), adsl$uname),
            p(strong("Data Set Creation DateTime:"), adbm$mtime),
            p(strong("Number of  Subjects:"), format(length(sdbm_nsubjs), big.mark = ",")),
            p(strong("Number of Variables Kept for Visualizations:"), format(length(names(ALB)), big.mark = ",")),
            p(strong("Number of Records for Biomarkers of Interest:"), format(nrow(ALB), big.mark = ",")),
            p(strong("Number of Biomarkers of Interest:"), format(length(param_choices), big.mark = ",")),
            br(),
            p(strong("Biomarkers of interest (label, code):")),
            do.call(
              # call shiny tagList
              shiny::tagList,
              # with a list of shiny::p elements (one for each letter) by mapping a
              # '<p> builder' function over a list of values
              Map(function(param_list, paramcd_list) {
                shiny::p(shiny::tags$ul(h6(print(paste0(param_list, " ,")), paramcd_list)))
              }, paramcd_list = paramcd_list, param_list = param_list)
            )
          ))),
      filters = NULL
    ),
    
    tm_variable_browser(label = "View Variables"),

    tm_data_table(label = "View Data"), 

    modules(
      label = "Visualizations",

      tm_g_boxplot(
        label = "Box Plot",
        dataname = "ALB",
        param_var = "PARAMCD",
        param_choices = param_choices,
        param = param_choices[1],
        value_var = "AVAL",
        value_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "BASE2", "CHG2", "PCHG2", "AVALL2", "BASEL2", "BASE2L2"),
        facet= TRUE,
        plot_height = c(500, 200, 2000),
        loq_flag_var = "LOQFL",
        visit_var = "AVISITCD",
        visit_var_choices = "AVISITCD",
        trt_group = "ARM"
      ),
      tm_g_density_distribution_plot(
        label = "Density Distribution Plot",
        dataname = "ALB",
        param_var = "PARAMCD",
        param_choices = param_choices,
        param = param_choices[1],
        xaxis_var = "AVAL",
        xaxis_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "BASE2", "CHG2", "PCHG2", "AVALL2", "BASEL2", "BASE2L2"),
        trt_group = "ARM",
        color_manual = color_manual,
        loq_flag_var = 'LOQFL',
        plot_width = c(800, 200, 2000),
        plot_height = c(500, 200, 2000),
        font_size = c(12, 8, 20),
        line_size = c(1, 1, 12)
      ),
      tm_g_lineplot(
        label = "Line Plot",
        dataname = "ALB",
        xvar = "AVISIT",
        yvar = "AVAL",
        yvar_choices = c("AVAL","CHG"),
        param_var = "PARAMCD",
        param = "CRP",
        param_choices = param_choices,
        trt_group = "ARM"
      ),
      tm_g_scatterplot(
        label = "Scatter Plot",
        dataname = "ALB",
        param_var = "PARAMCD",
        param_choices = param_choices,
        param = "CRP",
        xaxis_var = "BASE",
        xaxis_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "BASE2", "CHG2", "PCHG2", "AVALL2", "BASEL2", "BASE2L2"),
        yaxis_var = "AVAL",
        yaxis_var_choices = c("AVAL", "BASE", "CHG", "PCHG", "BASE2", "CHG2", "PCHG2", "AVALL2", "BASEL2", "BASE2L2"),
        trt_group = "ARM",
        plot_width = c(800, 200, 2000),
        plot_height = c(800, 200, 2000),
        facet = FALSE,
        reg_line = FALSE,
        font_size = c(12, 8, 20),
        dot_size = c(1, 1, 12),
        reg_text_size = c(3, 3, 10)
      ),
      module(
        label = "Spaghetti Plot",
        server = function(input, output, session, datasets) {},
        ui = function(id) div(p("Spaghetti Plots Here")),
        filters = "ASL"
      )
    )
  ),
  header = tags$h1("I2ON Biomarker Visualizations"),
  footer = tags$p("Packages: teal.goshawk, goshawk", 
                  p("Authors: Wenyi Liu, Nick Paszty, Jeffrey Tomlinson, Bali Toth"),
                  p("Copyright 2018"))

  )

shinyApp(x$ui, x$server)
