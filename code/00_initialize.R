library(miceadds)
library(ivpack)
library(AER)
library(jtools)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(lubridate)
library(Hmisc)
library(Greg)
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


theme_bc <- function(){
  library(ggplot2)
  library(extrafont)
  theme(plot.title = element_text(family = "Roboto Black", 
                                  color = "black", hjust = 0.5, margin = margin(0, 0, 10, 0)),
        axis.title = element_text(family = "Roboto", 
                                  color = "#525353"),
        axis.line = element_line(colour = "black"), 
        text = element_text(family = "Roboto", color = "#525353"), 
        plot.caption = element_text(family = "Roboto", size = 8),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#dddddb"),
        plot.background = element_rect(fill = "#dddddb"), 
        panel.grid.minor = element_blank(),
        legend.key = element_blank(), 
        legend.background = element_rect(color = "black", fill = "#dddddb"), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        plot.margin = margin(10, 10, 10, 10))
}

weighted.ttest.ci <- function(x, weights, conf.level = 0.95) {
  nx <- length(x)
  df <- nx - 1
  vx <- wtd.var(x, weights, normwt = TRUE) ## From Hmisc
  mx <- weighted.mean(x, weights)
  stderr <- sqrt(vx/nx)
  tstat <- mx/stderr ## not mx - mu
  alpha <- 1 - conf.level
  cint <- qt(1 - alpha/2, df)
  cint <- tstat + c(-cint, cint)
  cint * stderr
}

save <- c("db", "cleanup", "theme_bc", "save", "weighted.ttest.ci")


cleanup <- function(...){
  save2 <- c(save, ...)
  rm(list=ls(envir = .GlobalEnv)[! ls(envir = .GlobalEnv) %in% save2], envir = .GlobalEnv)
}

db <- dbConnect(SQLite(), "D:/rolls.db")

options("RStata.StataVersion" = 15)
options("RStata.StataPath" = "\"C:\\Program Files (x86)\\Stata15\\StataSE-64\"")