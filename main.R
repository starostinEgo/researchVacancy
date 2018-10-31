library(jsonlite)
library(curl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)

setwd("C:\\Users\\EStarostin\\Desktop\\myWork/researchVacancy")
source("functions.R")

################################
## step 1 get data from hh.ru
################################

jobdf <- hh.getjobs(query = c("data+scientist"),
                    ##, "systems+analyst"
                    ##, "product+owner"), 
                    paid = FALSE)

jobdf <- hh.getSalaryExp(jobdf)

all.skills <- hh.getskills(jobdf$URL)


#################################
##prepare data
################################

length(unique(jobdf$id))
length(jobdf$id)

##get data from bank and convert salary to RUR
quotations <- quotations.update()

##convert salary use usd,eur and tax
jobdf <- convert.currency(df = jobdf, quotationsdf = quotations)
jobdf <- gross.to.net(df = jobdf)

## use name vacancy create classafication
jobdf <- get.positions(jobdf)


############################
##
#############################