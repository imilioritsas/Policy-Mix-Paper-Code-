# clean memory
rm(list=ls())

# load libraries
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(gets)
library(getspanel)
library(readr)
library(readxl)
library(plm)
library(countrycode)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)  # For arranging multiple plots
library(writexl)

# set working directory
dir <- "R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code" #use own directory here
setwd(dir)

# call scripts
# data cleaning
source("01.data_cleaning_wind.R") 

# analysis
source("02.analysis_wind.R")

#figures
source("03.figures_wind (Fig.3b).R")

#robustness checks; these can be run independently, as they include both data cleaning and analysis
source("04.robustness_checks_wind_excluding_prices.R")
source("04.robustness_checks_wind_with_prices.R")
