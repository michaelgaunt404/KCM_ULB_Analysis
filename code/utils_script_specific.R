#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script holds utility functions specific for this script
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: all the scripts in here are general scripts
# all the scrupts in here are specific to this project
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#project file performs this task - section is not required

#reads all csvs using data.table::fread()
#removes zero variance columns
#and cleans names
read_csv_allFiles <- function(file_list, extra_path) {
  data_list =
    file_list %>%
    paste0(here(), extra_path, .) %>%
    map(~data.table::fread(.x) %>%
          na_if("NULL") %>%
          janitor::remove_empty("cols") %>%
          janitor::clean_names()
    )
  names(data_list) = file_list
  data_list
}

#helper to make floor divides
#generally used to make bins
floor_divide = function(value, floor){
  (value %/% floor)*floor
}



