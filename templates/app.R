# required packages
library(teal)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(DescTools) # for %% operators e.g. %like any%

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

# list of ALB variable to keep - not sure if we want to do this so commented out. see ALB2 assignment below
#albvarlist <- c('CRP', 'ESR')

# list of biomarkers of interest. see ALB2 assignment below
bmlist <- c('CRP', 'ESR')

################################################################################
# END: SPA Input Required
################################################################################

ASL <- read_bce(ASL_path)
ALB <- read_bce(ALB_path)

# post process the data to subset records per specification and to create new variables
ALB2 <- subset(ALB,
               subset = PARAMCD %in% c(bmlist) & ITTFL == 'Y' & ANLFL == 'Y' & AVISIT %like any% c('BASE%','%WEEK%'), 
               select = c('STUDYID', 'USUBJID', 'ITTFL', 'ANLFL', 'ARM', 'AVISIT', 'AVISITN', 'PARAMCD', 'AVAL', 'AVALU'))

# create a visit code - baseline record code is "BB" week records coded to "W NN"
ANL <- ALB2 %>% mutate(AVISITCD = paste0(substr(AVISIT,start=1, stop=1), 
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
sdbm_nvars <- format(names(ANL), big.mark = ",")

# get number of subjects
sdbm_nsubjs <- ANL %>%
  pull(USUBJID) %>%
  unique()

# get number of lab parameter values
#sdbm_nparams <- ANL %>%
#  pull(PARAM) %>%
#  unique()

#browser() for debugging

x <- teal::init(
  data =  list(ASL = ASL, ANL = ANL),
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
                            p(strong("Number of Variables:"), format(length(names(ANL)), big.mark = ",")),
                            p(strong("Number of Records:"), format(nrow(ANL), big.mark = ","))),
                            #p(strong("Number of Lab Parameters:"), format(length(sdbm_nparams), big.mark = ","))),
      filters = NULL
    ),
    tm_variable_browser(label = "View Variables"), # may not want to keep this module
    tm_data_table(label = "View Data", variables_selected = defVars), # may not want to keep this module
    modules(
      label = "Visualizations",
      tm_table( # may not want to keep this module
        label = "Demographic Table",
        dataname = "ASL",
        xvar = "SEX",
        xvar_choices = c("SEX", "RACE", "AGEGRP", "REGION"),
        yvar = "RACE",
        yvar_choices = c("RACE", "AGEGRP", "REGION")
      ),
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
      module(
        label = "Line Plot",
        server = function(input, output, session, datasets) {},
        ui = function(id) div(p("Line Plots Here")),
        filters = "ASL"
      ),
      module(
        label = "Scatterplot",
        server = function(input, output, session, datasets) {},
        ui = function(id) div(p("Scatter Plots Here")),
        filters = "ASL"
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
