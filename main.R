library(jsonlite)
library(curl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(XML)
library(htmlTable)

setwd("C:\\Users\\EStarostin\\Desktop\\research\\researchVacancy")
source("functions.R")




################################
## step 1 get data from hh.ru
################################

## функция идет на сайт hh и парсит данные по ключевым словам, что переданы в функции
## функция данные превращает в data.Frame

jobdf <- hh.getjobs(query = c( "sales"),
                    ##"data+scientist"),
                    ##, "systems+analyst"
                    ##, "product+owner"), 
                    paid = FALSE)

## функция идет по каждой url что была получена ранее и 
## собирает индивидуально дополнительную информацию

jobdf <- hh.getSalaryExp(jobdf)

## функция идет опять по каждому url что был получен ранее и 
## собирает информацию об необходимых скилах, 
## т.к. их больше один то делаем отдельную таблицу

all.skills <- hh.getskills(jobdf$URL)

#################################
##prepare data
################################

length(unique(jobdf$id))
length(jobdf$id)

## собираем информацию о текущем курсе валют, для перевода в рубли
quotations <- quotations.update()

##convert salary use usd,eur and tax
jobdf <- convert.currency(df = jobdf, quotationsdf = quotations)
jobdf <- gross.to.net(df = jobdf)

## use name vacancy create classafication

jobdf <- get.positions(jobdf)

## создание средней заработной платы на основе от и  до и с учетом 
## если какой то интервал пропущен, то применяем экстраполяцию

jobs.paid <- select.paid(jobdf)

#############################
## analyses salary using city and level (middle,junior...)
#############################

## распределения заработных плат в зависимости от города

jobs.paid$region <- ifelse(jobs.paid$city == "Москва" | 
                             jobs.paid$city == "Санкт-Петербург",jobs.paid$city,"Другие Города")


ggplot(jobs.paid, aes(salary, fill = region, colour = region)) +
           geom_density(alpha=.3) +
           scale_fill_discrete(guide = guide_legend(reverse=FALSE)) +
           scale_x_continuous(labels = function(x) format(x, scientific = FALSE), name = "Зарплата, руб.",
                              breaks = round(seq(min(jobs.paid$salary), 
                                                 max(jobs.paid$salary), by = 50000),1)) +
           scale_y_continuous(name = "Плотность распределения") +
           theme(axis.text.x = element_text(size=9), axis.title = element_text(size=10)) +
            ggtitle("распределение заработной платы по городам")


## распределения заработной платы в зависимости от города и уровня

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

## сравнения middle and senior только для Москвы

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


## преставление заработной платы в зависимости от опыта (Москва)

ggplot(jobs.paid %>% filter(region == "Москва"), aes(salary, fill = experience, color = experience)) +
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

## преставление заработной платы в зависимости от опыта (Питер)

ggplot(jobs.paid %>% filter(region == "Санкт-Петербург"), aes(salary, fill = experience, color = experience)) +
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



############################################
## key skills
############################################

## строим в лоб частоту скилов
tmp <- as.data.frame(table(all.skills$skill), col.names = c("Skill", "Freq"))
htmlTable::htmlTable(x = head(tmp[order(tmp$Freq, na.last = TRUE, decreasing = TRUE),]),
                     rnames = FALSE, header = c("Skill", "Freq"),
                     align = 'l', css.cell = "padding-left: .5em; padding-right: 2em;")

## добовляем регулиризацию слов
dict <- read.csv(file = "skills.csv", header = TRUE,
                 stringsAsFactors = FALSE, sep = ";", na.strings = "", 
                 encoding = "UTF-8")
all.skills <- left_join(all.skills,dict,by = c("skill"))

## строим график частота вакансий в зависимости от скилов

tmp <- merge(x = all.skills, y = jobdf %>% select(id,lvl), 
             by = "id", sort = FALSE)
tmp <- na.omit(tmp)

ggplot(as.data.frame(table(tmp %>%
                             select(unionSkill)))) +
  geom_bar(colour = "#666666", stat = "identity",
           aes(x = reorder(Var1, Freq), y = Freq, fill = reorder(Var1, -Freq))) +
  scale_y_continuous(name = "Число вакансий") +
  theme(legend.position = "none",
        axis.text = element_text(size = 12)) +
  coord_flip()


## график зависимости зарплаты от скилов

tmp <- merge(x = all.skills, y = jobs.paid, 
             by = "id", sort = FALSE)
tmp <- na.omit(tmp)

ggplotly(ggplot(tmp, 
                aes(unionSkill, salary)) +
           coord_flip() +
           geom_count(aes(size = ..n.., color = region)) +
           scale_fill_discrete(name = "region") +
           scale_y_continuous(name = "Зарплата, руб.") +
           scale_size_area(max_size = 11) +
           theme(legend.position = "bottom", axis.title = element_blank(),
                 axis.text.y = element_text(size=10, angle=10)))




#################
## Shiny 
#####################
library("shiny")
runExample("01_hello")

library(shiny)
runApp("01_app.R")

counties <- readRDS("census-app/data/counties.rds")
head(counties)
install.packages(c("maps", "mapproj"))
install.packages("quantmod")

library(maps)
library(mapproj)
library(quantmod)
source("helpers.R")
counties <- readRDS("data/counties.rds")
percent_map(counties$white, "darkgreen", "% White")



