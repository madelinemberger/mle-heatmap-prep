# Set up Script
# Contains all the packages, file paths and helpful functions to load the beginning of each R session

library(tidyverse)
library(sf)
library(janitor)
library(here)
library(rgeos)
library(mapview)
library(foreach)

# helpful function
'%!in%' <- function(x,y)!('%in%'(x,y))

# directories
in_dir <- here("inputs")
out_dir <- here("outputs")



# static dates
launch_date <- "2021-12-03"

