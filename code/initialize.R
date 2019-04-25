library(tidyverse)
library(data.table)
library(kevostools)
library(rgdal)
library(sqldf)
library(splitstackshape)
library(extrafont)

db <- dbConnect(SQLite(), "D:/rolls.db")