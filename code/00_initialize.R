library(AER)
library(lubridate)
library(RStata)
library(estimatr)
library(Matching)
library(tidyverse)
library(data.table)
library(kevostools)
library(rgdal)
library(sqldf)
library(splitstackshape)
library(extrafont)
library(scales)
library(kableExtra)
library(stargazer)

## QUICK FUNCTION TO CLEAR THE ENVIRONMENT OF MOST OBJECTS AFTER EACH STEP
## DOING SO TO WORK AROUND MEMORY CONSTRAINTS
save <- c("db", "cleanup", "theme_bc", "save")


cleanup <- function(...){
  save2 <- c(save, ...)
  rm(list=ls(envir = .GlobalEnv)[! ls(envir = .GlobalEnv) %in% save2], envir = .GlobalEnv)
}

db <- dbConnect(SQLite(), "D:/rolls.db")

options("RStata.StataVersion" = 15)
options("RStata.StataPath" = "\"C:\\Program Files (x86)\\Stata15\\StataSE-64\"")