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

#Leaflet Plots
lat.lon = read.csv('countries.csv')
res1 = GET('https://api.covid19api.com/summary')
cdf = fromJSON(rawToChar(res1$content))
res2 = GET('https://api.covid19api.com/total/dayone/country/united-states')
totalsdf = fromJSON(rawToChar(res2$content))
res3 = GET('https://api.covid19api.com/dayone/country/united-states/status/confirmed/live')
livedf = fromJSON(rawToChar(res3$content))
casedata = merge(cdf$Countries, lat.lon, by.x = 'CountryCode', by.y = 'country')
world <- 'https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json'
countries <- readOGR(world)
casedata$codes =  as.character(
  countrycode(casedata$Country,origin = 'country.name', destination = 'iso3c'))
countries@data = data.frame(countries@data, casedata[match(countries@data$id, casedata$codes),])
bins = unique(as.numeric(quantile(countries@data$NewConfirmed, probs = seq(0,1, .1), na.rm = T, type = 1)))
labels = sprintf("<strong>%s</strong><br/>%s",
                 countries$name, prettyNum(countries$NewConfirmed, big.mark = ",")) %>% 
  lapply(htmltools::HTML)
pal = colorBin("YlOrRd", domain = casedata$NewConfirmed, bins = bins)
cdc = "https://www.cdc.gov/coronavirus/2019-ncov/index.html"

#Tab 2 Plots
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

continents = countrycode(sourcevar = casedata$Country, origin = "country.name", destination = "continent")
casedata2 = cbind(casedata, continents)
casedata3 = na.omit(casedata2[-173,])

#Shiny UI Setup
body <- dashboardBody(
  fluidRow(
    tabBox(
      title = NULL, width = 12,
      id = "tabset1", height = "200px",
      tabPanel("Covid 19 Map",  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
               leafletOutput("map",width="100%",height="600px"),
               absolutePanel(top = 10, right = 10,
                             selectInput("colors", "Color Scheme",
                                         rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                             ),
                             actionButton('information', "Show worldwide totals"),
                             checkboxInput("legend", "Show legend", FALSE)), sidebarLayout(
                               sidebarPanel(
                                 h3(paste('Current Epicenter of the Virus', 
                                          countries@data$Country[which.max(countries@data$NewConfirmed)], sep = ": "))
                               ),
                               mainPanel(
                                 h2(paste('Confirmed Cases Today', 
                                          prettyNum(sum(countries@data$NewConfirmed, na.rm = T), big.mark = ","), sep = ": "))))),
      tabPanel("Supplementary Plots", imageOutput("plot1"), 
               plotOutput("plot2"), plotOutput("plot3"))
    )
  ),
  
)

ui <- fluidPage(dashboardPage(
  dashboardHeader(title = "Coivd 19 Tracker"),
  dashboardSidebar(disable = TRUE),
  body))


#Shiny Server
server <- function(input, output, session) {
  colorpal <- reactive({
    colorBin(input$colors, domain = casedata$NewConfirmed, bins = bins)})
  
  output$map <- renderLeaflet({
    leaflet(countries) %>% addProviderTiles("CartoDB.VoyagerLabelsUnder")%>% 
      setView(lng = 2.34, lat = 27, zoom = 1)})
  
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = countries) %>% 
      addPolygons(fillColor = ~pal(NewConfirmed), weight = 2, opacity = 1,
                  color = "white", dashArray = "3", fillOpacity = 0.7,  
                  highlight = highlightOptions(weight = 3,color = "dimgray",dashArray = "",
                                               fillOpacity = 0.7, bringToFront = TRUE), label = labels, 
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto"),
                  popup = paste0('<strong>', prettyNum(countries@data$name, big.mark = ","), 
                                 '</strong>', "<br>", "Total Confirmed: ",
                                 prettyNum(countries@data$TotalConfirmed, big.mark = ","), 
                                 "<br>", "Total Deaths: ",
                                 prettyNum(countries@data$TotalDeaths, big.mark = ","), 
                                 "<br>", "New Deaths: ",
                                 prettyNum(countries@data$NewDeaths, big.mark = ","), "<br>", 
                                 "<a href='", cdc, "' target='_blank'>", 
                                 "Click here for more COVID 19 information</a>"))})
  observe({
    proxy <- leafletProxy("map", data = countries)
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(pal = pal, values = ~NewConfirmed, opacity = 0.7, title = 'New Cases',
                          position = "bottomright")}})
  
  observe({
    proxy2 <- leafletProxy('map', data = countries)
    proxy2 %>% clearControls()
    if(input$information) {
      proxy2 %>% addPopups(0, 0, popup = paste0("Total Cases Worldwide: ",
                                                prettyNum(sum(countries@data$TotalConfirmed, na.rm = T), big.mark = ","),
                                                "<br>", "Total Deaths Worldwide: ",
                                                prettyNum(sum(countries@data$TotalDeaths, na.rm = T), big.mark = ","), 
                                                "<br>", "Total Recoveries Worldwide: ",
                                                prettyNum(sum(countries@data$TotalRecovered, na.rm = T), big.mark = ","), 
                                                "<br>","<a href='", cdc, "' target='_blank'>",
                                                "Click here for more COVID 19 information</a>"))
    }
  })
  
  output$plot1 <- renderImage({
    outfile <- tempfile(fileext='.gif')
    
    anim_save("outfile.gif", animations)
    
    list(src = "outfile.gif",
         contentType = 'image/gif'
    )}, deleteFile = TRUE)
  
  output$plot2 <- renderPlot({
    ggplot(totals) + geom_line(aes(Time, Cases, col = 'Reported Cases')) + theme_minimal() + 
      geom_smooth(aes(Time, Cases, col = 'Predicted Cases'), se = T) + ggtitle('World Cases Over Time') + 
      labs(x = '', y = '') + theme(legend.title = element_blank())})
  
  output$plot3 <- renderPlot({
    ggplot(data = casedata3, aes(x = TotalDeaths, y = continents, fill = continents)) + 
      geom_density_ridges(alpha=0.6) +
      theme_ridges() + labs(x = '', y = '') + ggtitle('Total Deaths by Continent',
                                                      subtitle = 'America has been omitted due to size') + 
      theme(legend.title = element_blank())
    
    
  })
  
  
}

shinyApp(ui, server)

