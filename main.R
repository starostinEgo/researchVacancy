library(jsonlite)
library(curl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)

setwd("C:\\Users\\EStarostin\\Desktop\\myWork/researchVacancy")
source("functions.R")

## step 1.0 get data from hh.ru

jobdf <- hh.getjobs(query = c("data+scientist"),
                    ##, "systems+analyst"
                    ##, "product+owner"), 
                    paid = FALSE)

jobdf <- hh.getSalaryExp(jobdf)


all.skills <- hh.getskills(jobdf$URL)


