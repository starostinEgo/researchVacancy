library(jsonlite)
library(curl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)

setwd("C:\\Users\\EStarostin\\Desktop\\myWork/researchVacancy")
source("functions.R")

jobdf <- hh.getjobs(query = c("data+scientist"),
                    ##, "systems+analyst"
                    ##, "product+owner"), 
                    paid = FALSE)



