library(httr)
library(jsonlite)
library(rvest)
library(leaflet)
library(lubridate)
library(anytime)
library(tidyverse)
library(RColorBrewer)
library(extrafont)
library(ggrepel)
library(leaflet)
library(gganimate)
library(sp)
library(rgdal)
library(htmlwidgets)
library(htmltools)
library(countrycode)
library(viridis)
library(randomcoloR)
library(RCurl)
library(gifski)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(ggridges)

hopkinsdata = getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
totalconfirmed = read.csv(text = hopkinsdata)
worldconfirmed = read.csv(text = hopkinsdata)
worldconfirmed = colSums(worldconfirmed[,-c(1:4)])

if(length(seq(as.Date("2020-01-22") , Sys.Date(), 1)) == length(worldconfirmed)){
  Time = data.frame('Time' = seq(as.Date("2020-01-22") , Sys.Date(), 1))
} else {
  Time = data.frame('Time' = seq(as.Date("2020-01-22") , Sys.Date()-1, 1))
}

totals = cbind(Time, 'Cases' = worldconfirmed)
totalconfirmed = subset(totalconfirmed, !duplicated(Country.Region))
main = cbind(Time, 'Cases' = as.numeric(totalconfirmed[1, 5:ncol(totalconfirmed)]), 
             'Country' = as.character(totalconfirmed$Country.Region[1]))
main$Country = as.character(main$Country)

for(i in 2:nrow(totalconfirmed)){
  tempdf = cbind(Time, 'Cases' = as.numeric(totalconfirmed[i, 5:ncol(totalconfirmed)]), 
                 'Country' = as.character(totalconfirmed$Country.Region[i]))
  tempdf$Country = as.character(tempdf$Country)
  main = bind_rows(main, tempdf)
}

p = ggplot(main, aes(x = Time, y = Cases, col = Country, label = Country)) +  geom_label() + 
  geom_line(size = 1.05, alpha = 0.7) + 
  theme_minimal() +
  theme(text = element_text(size = 13, color = 'black'),
        legend.position='none') + labs(x = '', y='') + 
  scale_color_viridis(discrete = T, option = 'D') + transition_reveal(Time) + 
  view_follow() + coord_cartesian(clip = 'off') + ggtitle("Cases by Country Over Time")

animations = animate(p, duration = 15, nframes = 5, fps = 10, height = 400, width = 900)

anim_save("plot1.gif", animations, "C:/Users/shane/Desktop/Spring 2020/STAT 431/Final-Project")
