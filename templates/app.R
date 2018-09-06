#.libPaths(c(file.path(getwd()), "~/goshawk", .libPaths()))
#goshawk:::g_lineplot
# required packages
library(teal)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(DescTools) # for %% operators e.g. %like any%
library(goshawk)
library(teal.goshawk)

#`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

################################################################################
# BEGIN: SPA Input Required
################################################################################

# absolute path and data det name
#ASL_path <- "/opt/BIOSTAT/qa/cdt7892a/s29350m/libraries/asl.sas7bdat"
#ALB_path <- "/opt/BIOSTAT/qa/cdt7892a/s29350m/libraries/alb.sas7bdat"

# for development team testing
ASL_path <- "~/btk/lupus/dataadam/asl.sas7bdat"
ALB_path <- "~/btk/lupus/dataadam/alb3arm.sas7bdat"

# default variable names to appear in Data View module
defVars <- list("PATNUM", "ARMCD")

# analysis type e.g. Interim Analysis, Final Analysis etc.
ATYPE <- "Interim Analysis"

# list of biomarkers of interest. see ALB1 assignment below
param_choices <- c("CRP","ADIGG","IG","IGA","IGE","IGG","IGM","TEST")

################################################################################
# END: SPA Input Required
################################################################################

ASL0 <- read_bce(ASL_path)
ASL <- subset(ASL0, subset = ITTFL == 'Y' & IAFL == 'Y')

ALB0 <- read_bce(ALB_path)

# post process the data to subset records per specification
ALB_SUBSET <- subset(ALB0,
              subset = PARAMCD %in% c(param_choices) & ITTFL == 'Y' & IAFL == 'Y' & ANLFL == 'Y' & AVISIT %like any% c('BASE%','%WEEK%'),
              select = c('STUDYID', 'USUBJID', 'ITTFL', 'ANLFL', 'ARM', 'AVISIT', 'AVISITN', 'PARAMCD', 'AVAL', 'AVALU', 'BASE', 'CHG', 'PCHG',
               'LBSTRESC', 'LBSTRESN'))

# calculate the minimum AVAL for each PARAMCD
PARAM_MINS <- ALB_SUBSET %>%
select(USUBJID, PARAMCD, AVAL) %>%
  filter(PARAMCD %in% param_choices) %>%
  group_by(PARAMCD) %>%
  summarise(AVAL_MIN=min(AVAL, na.rm=TRUE))

# post process the data to create several new variables and adjust existing record specific valules per specification
# - create a visit code variable - baseline record code is "BB" and week records coded to "W NN"
# - adjust existing BASELINE record values where values are missing: According to SPA this is a STREAM artifact
ALB_SUPED1 <- ALB_SUBSET %>% mutate(AVISITCD = paste0(substr(AVISIT,start=1, stop=1),
                                        substr(AVISIT, start=regexpr(" ", AVISIT), stop=regexpr(" ", AVISIT)+2))) %>%
               mutate(AVISITCDN =  ifelse(AVISITCD == "BB", 0, substr(AVISITCD,start=2, stop=4))) %>%
               mutate(BASE = ifelse(AVISIT == "BASELINE" & is.na(BASE), AVAL, BASE)) %>%
               mutate(CHG = ifelse(AVISIT == "BASELINE" & is.na(CHG), 0, CHG)) %>%
               mutate(PCHG = ifelse(AVISIT == "BASELINE" & is.na(PCHG), 0, PCHG))
               # may need to add similar code for BASE2 related variables


# merge minimum AVAL value onto the ALB data to calculate the log2 variables and preserve the variable order
ALB_SUPED2 <- merge(ALB_SUPED1, PARAM_MINS, by="PARAMCD")[, union(names(ALB_SUPED1), names(PARAM_MINS))] %>%
       mutate(AVALL2 = ifelse(AVAL == 0, log2(AVAL_MIN/2), log2(AVAL))) %>%
       mutate(BASEL2 = ifelse(BASE == 0, log2(AVAL_MIN/2), log2(BASE))) #%>% need SPA to finish adding BASE2 to ALB
       #mutate(BASE2L2 = ifelse(BASE2 == 0, log2(AVAL_MIN/2), log2(AVAL)))

# for proper chronological ordering of visits in visualizations
ALB_SUPED2$AVISITCDN <- as.numeric(ALB_SUPED2$AVISITCDN) # coerce character into numeric
ALB <- ALB_SUPED2 %>% mutate(AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN))

# to test loq_flag
ALB <- ALB %>% mutate(LOQFL = ifelse(PARAMCD == "CRP" & AVAL < .5, "Y", "N"))

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

# get number of lab parameter values
#sdbm_nparams <- ALB %>%
#  pull(PARAM) %>%
#  unique()

#browser() for debugging

x <- teal::init(
  data =  list(ASL = ASL, ALB = ALB),
  modules = root_modules(
    module(
      "Source Data",
      server = function(input, output, session, datasets) {},
      ui = function(id) div(h4(strong("Analysis Type:"), ATYPE),
                            p(strong("Subject Level Data Set:"), ASL_path),
                            #p(strong("Data Set Label:")),
                            p(strong("Data Set Owner:"), adsl$uname), # can uname be linked to peeps?
                            p(strong("Data Set Creation DateTime:"), adsl$mtime),
                            p(strong("Number of  Subjects:"), format(length(sdsl_nsubjs), big.mark = ",")),
                            p(strong("Number of Variables:"), format(length(names(ASL)), big.mark = ",")),
                            p(strong("Number of Records:"), format(nrow(ASL), big.mark = ",")),
                            br(),
                            p(strong("Biomarker Data Set:"), ALB_path),
                            #p(strong("Data Set Label:")),
                            p(strong("Data Set Owner:"), adbm$uname), # can uname be linked to peeps?
                            p(strong("Data Set Creation DateTime:"), adbm$mtime),
                            p(strong("Number of  Subjects:"), format(length(sdbm_nsubjs), big.mark = ",")),
                            p(strong("Number of Variables:"), format(length(names(ALB)), big.mark = ",")),
                            p(strong("Number of Records:"), format(nrow(ALB), big.mark = ","))),
                            #p(strong("Number of Lab Parameters:"), format(length(sdbm_nparams), big.mark = ","))),
      filters = NULL
    ),
    tm_variable_browser(label = "View Variables"), # may not want to keep this module
    tm_data_table(label = "View Data", variables_selected = defVars), # may not want to keep this module
    modules(
      label = "Visualizations",
      # tm_table( # may not want to keep this module
      #   label = "Demographic Table",
      #   dataname = "ASL",
      #   xvar = "SEX",
      #   xvar_choices = c("SEX", "RACE", "AGEGRP", "REGION"),
      #   yvar = "RACE",
      #   yvar_choices = c("RACE", "AGEGRP", "REGION")
      # ),
      module(
        label = "Box Plot",
        server = function(input, output, session, datasets) {},
        ui = function(id) div(p("Box PLots Here")),
        filters = "ASL"
      ),
      module(
        label = "Distribution",
        server = function(input, output, session, datasets) {},
        ui = function(id) div(p("Distribution Plots Here")),
        filters = "ASL"
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
        trt_group_choices = c("ARM", "ARMCD"),
        plot_width = c(800, 200, 2000),
        plot_height = c(800, 200, 2000),
        facet = FALSE,
        reg_line = FALSE,
        font_size = c(12, 8, 20),
        dot_size = c(1, 1, 12),
        reg_text_size = 3
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
