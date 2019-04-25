library(tidyverse)
library(data.table)
library(kevostools)
library(rgdal)
library(sqldf)


db <- dbConnect(SQLite(), "D:/rolls.db")