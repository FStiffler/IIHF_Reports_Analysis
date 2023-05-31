##### Set dependencies
dependecies <- c("tidyverse", "rvest", "pdftools", "lubridate", "RColorBrewer", "knitr","kableExtra")

##### Install and load packages
missing <- dependecies[!(dependecies %in% installed.packages()[,"Package"])]
if(length(missing)) install.packages(missing)
lapply(dependecies, library, character.only = T)