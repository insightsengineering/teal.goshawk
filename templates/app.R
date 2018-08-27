#.libPaths(c(file.path(getwd()), "~/goshawk", .libPaths()))
#goshawk:::g_lineplot
# required packages
library(teal)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(DescTools) # for %% operators e.g. %like any%
library(teal.goshawk)
library(goshawk)

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

# list of biomarkers of interest. see ALB1 assignment below
param_choices <- c("CRP","ADIGG","IG","IGA","IGE","IGG","IGM","TEST")

################################################################################
# END: SPA Input Required
################################################################################

ASL <- read_bce(ASL_path)
ALB0 <- read_bce(ALB_path)

# post process the data to subset records per specification and to create new variables
ALB1 <- subset(ALB0,
               subset = PARAMCD %in% c(param_choices) & ITTFL == 'Y' & IAFL == 'Y' & ANLFL == 'Y' & AVISIT %like any% c('BASE%','%WEEK%'), 
               select = c('STUDYID', 'USUBJID', 'ITTFL', 'IAFL', 'ANLFL', 'ARM', 'AVISIT', 'AVISITN', 'PARAMCD', 'AVAL', 'AVALU', 'BASE', 'CHG', 'PCHG'))

# create a visit code - baseline record code is "BB" week records coded to "W NN"
ALB <- ALB1 %>% mutate(AVISITCD = paste0(substr(AVISIT,start=1, stop=1), 
                                         substr(AVISIT, start=regexpr(" ", AVISIT), stop=regexpr(" ", AVISIT)+2)))

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
      ui = function(id) div(p(strong("Subject Level Data Set:"), ASL_path),
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
        value_var = "AVAL",
        value_var_choices = c("AVAL", "CHG", "PCHG"),
        baseline_var = "BASE",
        baseline_var_choices = c("BASE", "BASEL2", "SCRN", "SCRNL2"),
        trt_group = "ARM",
        trt_group_choices = c("ARM", "ARMCD"),
        plot_height = c(600, 200, 2000),
        m_facet = FALSE,
        man_color = NULL
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
