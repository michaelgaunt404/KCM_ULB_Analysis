#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script handles flexdashboard knitting in worfklowr framework
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: it knits flexdashboard and places in the docs/ folder
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#project file performs this task - section is not required

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#zz_localG performs this task - section is not required

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#zz_localG performs this task - section is not required

#CODE BODY START================================================================


#!/usr/bin/env Rscript

# Run this script from the root of the workflowr
# project to add the dashboard to the workflowr website.

library(rmarkdown)
render("dashboard/dashboard.Rmd")
file.rename("dashboard/dashboard.html", "docs/dashboard.html")

