library(sandwich)
library(lmtest)
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
