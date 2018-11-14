##include library============
library(shiny)
library(shinydashboard)
library(leaflet)
library(jsonlite)
library(tidyverse)
library(leaflet.extras)
library(lubridate)
library(bitops)
library(dplyr)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
library(forcats)
library(ggplot2)
library(ggmap)
library(shinydashboard)
library(googleway)

#===UI==========================

ui <- dashboardPage(
                    skin = "red",
                    title = "JAPAN_TRAVEL",
                    dashboardHeader(title = "JAPAN_TRAVEL", titleWidth = 300),
                    
                    # interactive sidebar with menu and widgets
                    dashboardSidebar(width = 300,
                                     
                    tags$div(
                            tags$blockquote("本app為一呈現臺人赴日遊玩傾向之呈現"),
                            tags$h4("說明:"),
                            tags$p("透過爬取PTT", tags$a(href="https://www.ptt.cc/bbs/Japan_Travel/index.html", "日本旅遊版"),"兩千篇左右的遊記，對文章進行處理，並透過pca與kmeans的處理，以期達到透過將景點分成多塊觀光區的效果。"),
                            tags$p("本app為一將已整理好的資料進行呈現之app，使用者能透過調整kmeans分群數，觀察其中之變化。"),
                            tags$p("本app之景點資料來源為日本觀光局中文版的景點資料，其中列出四百多個較知名景點(很多我連聽都沒聽過)。"),
                            tags$p("可是基本上不怎麼成功!!"),
                            style = "padding: 10px;"
                                     ),
                                     
                            tags$hr(),
                            
                            sliderInput("num_ofgroup1", h3("Kmeans分幾群"),min = 1, max = 13, value = 2),         
                                     
                            tags$hr(),
                    
                            sliderInput("group1", h3("第幾群"),min = 1, max = 13, value = 2),  
                    
                            tags$hr(),
                    
                            sliderInput("num_ofgroup2", h3("Kmeans分幾群(含經緯度)"),min = 1, max = 13, value = 2),
                    
                            tags$hr(),
                    
                            sliderInput("group2", h3("第幾群(含經緯度)"),min = 1, max = 13, value = 2),
                    
                            tags$hr(),
                    
                            textInput("place", h3("依照輸入的地點列出附近景點"), 
                            value = "輸入一地點") 
                            
                    ),
                    
                    # Main panel for displaying outputs ----
                    dashboardBody(
                      
                      tags$head(tags$style("#myMap{height:90vh !important;}")),
                      
                      tags$h4("實際上做出來的:"),
                      
                      leafletOutput("myMap"),
                      
                      tags$hr(),
                      
                      tags$h4("理想應該長這樣(把經緯度也丟進K-means):"),
                      
                      leafletOutput("myMap2"),
                      
                      tags$h4("根據輸入的地點給出附近推薦景點:"),
                      
                      leafletOutput("myMap3")
                      
                    )
)

#===SERVE=======================
server <- function(input, output) {
  options(shiny.maxRequestSize = 100*1024^2)
  
  relation_place_nolatlon <- read.csv("./data/relation_place_nolatlon.csv")
  relation_place <- read.csv("./data/relation_place.csv")
  row.names(relation_place) <- relation_place$X
  row.names(relation_place_nolatlon) <- relation_place_nolatlon$X
  relation_place_nolatlon <- subset(relation_place_nolatlon, select = -X)
  relation_place <- subset(relation_place, select = -X)
  
  output$myMap <- renderLeaflet({
    
    k <- input$num_ofgroup1
    
    pr.km_original <- kmeans(relation_place_nolatlon, centers = k, nstart = 10)
    relation_place_nolatlon$ cluster <- pr.km_original$cluster
    relation_place_nolatlon$lonn <- relation_place$lon
    relation_place_nolatlon$lat <- relation_place$lat
    
    group <- input$group1
    
    pr.km_original$cluster[pr.km_original$cluster == group]
    group_place_original <- relation_place_nolatlon[relation_place_nolatlon$cluster == group,]
    group_place_original$words <- row.names(group_place_original)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng=139,lat=37,zoom = 5) %>%
      addMarkers(lng=group_place_original$lon,lat=group_place_original$lat,popup=group_place_original$words)
  })
  
  output$myMap2 <- renderLeaflet({
    
    k <- input$num_ofgroup2
    
    pr.km_with_lat_lon <- kmeans(relation_place, centers = k, nstart = 10)
    relation_place$ cluster <- pr.km_with_lat_lon$cluster

    group <- input$group2
    
    pr.km_with_lat_lon$cluster[pr.km_with_lat_lon$cluster == group]
    group_place <- relation_place[relation_place$cluster == group,]
    group_place$words <- row.names(group_place)

    leaflet() %>%
      addTiles() %>%
      setView(lng=139,lat=37,zoom = 5) %>%
      addMarkers(lng=group_place$lon,lat=group_place$lat,popup=group_place$words)
  })
  
  output$myMap3 <- renderLeaflet({
    place <- input$place
    k <- 13
    pr.km_with_lat_lon <- kmeans(relation_place, centers = k, nstart = 10)
    relation_place$ cluster <- pr.km_with_lat_lon$cluster
    
    relation_place$names <- row.names(relation_place)
    tmp <- relation_place[relation_place$names==place,]
    group <- tmp$cluster
    
    pr.km_with_lat_lon$cluster[pr.km_with_lat_lon$cluster == group]
    group_place <- relation_place[relation_place$cluster == group,]
    group_place$words <- row.names(group_place)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng=139,lat=37,zoom = 5) %>%
      addMarkers(lng=group_place$lon,lat=group_place$lat,popup=group_place$words)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
