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

## use mean create new salary data
jobs.paid <- select.paid(jobdf)
ggplot(jobs.paid,aes(x=salary))+
  geom_histogram()


## create grapghics density salary from region
jobs.paid$region <- ifelse(jobs.paid$city == "Москва" | 
                             jobs.paid$city == "Санкт-Петербург",jobs.paid$city,"Другие Города")


ggplot(jobs.paid, aes(salary, fill = region, colour = region)) +
           geom_density(alpha=.3) +
           scale_fill_discrete(guide = guide_legend(reverse=FALSE)) +
           scale_x_continuous(labels = function(x) format(x, scientific = FALSE), name = "Зарплата, руб.",
                              breaks = round(seq(min(jobs.paid$salary), 
                                                 max(jobs.paid$salary), by = 40000),1)) +
           scale_y_continuous(name = "Плотность распределения") +
           theme(axis.text.x = element_text(size=9), axis.title = element_text(size=10))


ggplot(jobs.paid %>% filter(region %in% c("Москва", "Санкт-Петербург"),
                            lvl %in% c("senior", "middle","junior")),
       aes(salary, fill = region, colour = region)) +
  facet_grid(lvl ~ .) +
  geom_density(alpha = .3) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE), name = "Зарплата, руб.",
                     breaks = round(seq(min(jobs.paid$salary), 
                                        max(jobs.paid$salary), by = 30000),1)) +
  scale_y_continuous(name = "Плотность распределения") +
  scale_fill_discrete(name = "Город") +
  scale_color_discrete(name = "Город") +
  guides(fill=guide_legend(
    keywidth=0.1,
    keyheight=0.1,
    default.unit="inch") 
  ) + theme(legend.spacing = unit(1,"inch"), axis.title = element_text(size=10))


ggplot(jobs.paid %>% filter(region == "Москва"), aes(salary, fill = lvl, color = lvl)) +
           geom_density(alpha=.4) +
           scale_fill_brewer(palette = "Set2")  +
           scale_color_brewer(palette = "Set2") +
           theme_light() +
           scale_y_continuous(name = "Плотность распределения") +
           scale_x_continuous(labels = function(x) format(x, scientific = FALSE), 
                              name = "Зарплата, руб.",
                              breaks = round(seq(min(jobs.paid$salary), 
                                                 max(jobs.paid$salary), by = 30000),1)) +
           theme(axis.text.x = element_text(size=9), axis.title = element_text(size=10))






tmp <- as.data.frame(table(all.skills$skill), col.names = c("Skill", "Freq"))
htmlTable::htmlTable(x = head(tmp[order(tmp$Freq, na.last = TRUE, decreasing = TRUE),]),
                     rnames = FALSE, header = c("Skill", "Freq"),
                     align = 'l', css.cell = "padding-left: .5em; padding-right: 2em;")

# Analyze skills
# 4.1 import dictionary

dict <- read.csv(file = "competencies.csv", header = TRUE,
                 stringsAsFactors = FALSE, sep = ";", na.strings = "", 
                 encoding = "UTF-8")

# 4.2 match skills with dictionary

all.skills <- categorize.skills(all.skills, dict)