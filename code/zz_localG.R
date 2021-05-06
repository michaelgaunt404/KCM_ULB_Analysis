#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a proxy global file that is used for dev purposes
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script defines libraries
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#project file performs this task - section is not required

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse) #meta package install for all tidyverse packages
library(magrittr)
library(readxl) #needed for reading xlsx data
library(janitor) #used for general data cleaning
library(data.table)
library(lubridate)
library(DT)
library(skimr)
library(data.validator)
library(here)
library(ggridges)

#source helpers/utilities=======================================================

# c("code/utils_script_specific.R", "code/utils_helpers_general.R") %>%
#   map(~source(here(.x)))

source(here("code/utils_script_specific.R"))
source(here("code/utils_helpers_general.R"))





