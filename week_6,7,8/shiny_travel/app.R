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
library(googleway)
# ##get more memory====================
# 
# memory.limit(40000)
# 
# ##words washing==============
# toSpace <- content_transformer(function(x, pattern) {
#   return (gsub(pattern, " ", x))
# }
# )
# d.corpus <- Corpus( DirSource("./travel") )
# d.corpus <- tm_map(d.corpus, removePunctuation)
# d.corpus <- tm_map(d.corpus, removeNumbers)
# 
# d.corpus <- tm_map(d.corpus, function(word) {
#   gsub("[A-Za-z0-9]", "", word)
# })
# vector1 <- list()
# id = c(10:20,30:40,50:60,70:80,90:100,110:120,130:140,150:160,170:180,190:200)
# for(i in id){
#   file_name <- paste0(i,".txt")
#   txt <- d.corpus[[file_name]][["content"]] 
#   vector1 <- c(vector1,as.list(strsplit(txt, "看板標題")[[1]])) #cut one article into several .txt
# }
# 
# ##Create new words dictionary==
# library(rvest)
# mixseg = worker()
# place <- read.csv("location.csv")
# place$words <- as.character(place$words)
# new_user_word(mixseg,place$words)
# 
# ##print an original map========
# place=place[!duplicated(place$words),]
# place$words <- as.character(place$words)
# map <- leaflet() %>%
#   addTiles() %>%
#   fitBounds("137","30","138","45") %>%
#   addMarkers(lng=place$lon,lat=place$lat,popup=place$words)
# map
# 
# ##cut words====================
# jieba_tokenizer = function(d)
# {
#   unlist( segment(d[[1]], mixseg) )
# }
# seg = lapply(vector1, jieba_tokenizer)
# 
# count_token = function(d)
# {
#   as.data.frame(table(d))
# }
# tokens = lapply(seg, count_token)
# n = length(seg)
# TDM = tokens[[1]]
# colNames <- names(seg)
# colNames <- c(1:n) 
# 
# ##create freq. mat of words cut by jeiba=
# for( id in c(2:n) )
# {
#   TDM = merge(TDM, tokens[[id]], by = "d", all = TRUE)
#   names(TDM) = c('d', colNames[1:id])
# }
# #delete the words that under 2 character
# TDM[is.na(TDM)] <- 0
# TDM$d <- as.character(TDM$d)
# TDM <- TDM[nchar(TDM$d)>1,]
# library(knitr)
# kable(head(TDM))
# kable(tail(TDM))
# 
# ##tf-idf==================================
# tf <- apply(as.matrix(TDM[,2:(n+1)]), 2, sum)
# 
# library(Matrix)
# idfCal <- function(word_doc)
# { 
#   log2( n / nnzero(word_doc) ) 
# }
# idf <- apply(as.matrix(TDM[,2:(n+1)]), 1, idfCal)
# 
# doc.tfidf <- TDM
# #use the Tf-Idf to calculate the weight of each word
# tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
# tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
# doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX
# stopLine = rowSums(doc.tfidf[,2:(n+1)])
# delID = which(stopLine == 0)
# library(knitr)
# TopWords = data.frame()
# for( id in c(1:n) )
# {
#   dayMax = order(doc.tfidf[,id+1], decreasing = TRUE)
#   showResult = t(as.data.frame(doc.tfidf[dayMax[1:20],1]))
#   TopWords = rbind(TopWords, showResult)
# }
# rownames(TopWords) = colnames(doc.tfidf)[2:(n+1)]
# TopWords = droplevels(TopWords)
# kable(TopWords)
# TDM$d = as.character(TDM$d)
# AllTop = as.data.frame( table(as.matrix(TopWords)) )
# AllTop = AllTop[order(AllTop$Freq, decreasing = TRUE),]
# 
# ##deal with the data used in pca========
# library(stats)
# names <- doc.tfidf$d ##abstract the words of seg and name the new matrix
# doc.tfidf <- subset(doc.tfidf, select = -d ) 
# matrix <- data.matrix(doc.tfidf)
# row.names(matrix) <- names #make the result of tf-idf be a new matrix
# r_mat <- t(matrix) #rotate the matrix
# 
# ##pca===================================
#  #pca to colume
# pcs2 <- prcomp(r_mat, center = T, scale = F)
#  #take the result of pca that we will use in kmeans
# center <- as.matrix(pcs2$center)
# 
# ##take the Specific words===============
# mumi <- data.frame(center)
# colnames(mumi) <- "abc"
# mumi$names <- row.names(mumi)
# mumi <- mumi[unique(mumi$names) %in% unique(place$words),]
# relation_place <- mumi$abc
# relation_place <- data.frame(relation_place)
# row.names(relation_place) <- mumi$names
# 
# ##deal with data used in kmeans=========
# relation_place$words <-  row.names(relation_place)
# relation_place <- merge(relation_place,place,by = "words",all.X = TRUE)
# relation_place=relation_place[!duplicated(relation_place$words),]
# relation_place <- subset(relation_place, select = -url) 
# row.names(relation_place) <- relation_place$words
# 
# relation_place <- subset(relation_place, select = -words)
# relation_place <- as.vector(relation_place)
# 
# relation_place_nolatlon <- subset(relation_place, select = -lat) 
# relation_place_nolatlon <- subset(relation_place_nolatlon, select = -lon) 
# 
# ##kmeans================================
# 
# k <- 6 
# 
# pr.km_with_lat_lon <- kmeans(relation_place, centers = k, nstart = 10)
# pr.km_original <- kmeans(relation_place_nolatlon, centers = k, nstart = 10)
# 
# relation_place$ cluster <- pr.km$cluster
# relation_place_nolatlon$ cluster <- pr.km_original$cluster
# 
# relation_place_nolatlon$lonn <- relation_place$lon
# relation_place_nolatlon$lat <- relation_place$lat
# 
# ##test each group distributed by kmeans (put the lon. & lat. into grouping)
# group = 6 ##change by the user
# 
# pr.km_with_lat_lon$cluster[pr.km_with_lat_lon$cluster == group]
# group_place <- relation_place[relation_place$cluster == group,]
# group_place$words <- row.names(group_place)
# 
# pr.km_original$cluster[pr.km_original$cluster == group]
# group_place_original <- relation_place_nolatlon[relation_place_nolatlon$cluster == group,]
# group_place_original$words <- row.names(group_place_original)
# 
# map2 <- leaflet() %>%
#   addTiles() %>%
#   fitBounds("137","30","138","45") %>%
#   addMarkers(lng=group_place$lon,lat=group_place$lat,popup=group_place$words)
# map2
# 
# map3 <- leaflet() %>%
#   addTiles() %>%
#   fitBounds("137","30","138","45") %>%
#   addMarkers(lng=group_place_original$lon,lat=group_place_original$lat,popup=group_place_original$words)
# map3

################################

#===UI==========================

ui <- dashboardPage(skin = "red",
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
                                     
                                     tags$hr()
                    ),
                    
                    # Main panel for displaying outputs ----
                    dashboardBody(
                      
                      tags$head(tags$style("#myMap{height:90vh !important;}")),
                      
                      tags$h4("實際上做出來的:"),
                      
                      leafletOutput("myMap"),
                      
                      tags$hr(),
                      
                      tags$h4("理想應該長這樣(把經緯度也丟進K-means):"),
                      
                      leafletOutput("myMap2")
                      
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
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)
