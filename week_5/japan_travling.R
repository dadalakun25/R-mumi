---
  title: "Japan_Traveling"
author: "工海二 張在然"
date: "2018年10月17日"
output: html_document
---
  ##本報告旨在透過觀察一年間ptt日本旅遊板的文章，希望能整理出台灣遊客在不同時節間，赴日本旅遊的地點的選擇上有什麼不同
  
  ```{r}
##引入所需要的套件包
rm(list=ls(all.names = TRUE))
library(bitops)
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
```

```{r}
##文字清洗
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
d.corpus <- Corpus( DirSource("./DATA") )
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)

d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})
```



```{r}
mixseg = worker()
my.data <- read.csv("data.csv") ##做了一份日本都道府縣的生詞表，但是好像沒有起甚麼作用...
str(my.data)
my.data$words <- as.character(my.data$words)
new_user_word(mixseg,my.data$words)
```
```{r}
#一個爬蟲，上到日本關光局的旅遊景點表上把每個地點都爬下來當作新詞
library(rvest)
doc <- read_html("https://www.welcome2japan.tw/location/destinations/")
place <- doc %>% html_nodes("#box li a") %>% html_text()
new_user_word(mixseg,place)
```


```{r}
#詞彙切割
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)
```
```{r}
n = length(seg)
TDM = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames) 
```
```{r}
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}
TDM[is.na(TDM)] <- 0
TDM$d <- as.character(TDM$d)
TDM <- TDM[nchar(TDM$d)>1,] #篩掉字數小於一的詞
library(knitr)
kable(head(TDM))
kable(tail(TDM))
```




```{r}

tf <- apply(as.matrix(TDM[,2:(n+1)]), 2, sum)

library(Matrix)
idfCal <- function(word_doc)
{ 
  log2( n / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,2:(n+1)]), 1, idfCal)

doc.tfidf <- TDM
#Tf-Idf

tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX

stopLine = rowSums(doc.tfidf[,2:(n+1)])
delID = which(stopLine == 0)

kable(head(doc.tfidf[delID,1]))
kable(tail(doc.tfidf[delID,1]))
```
```{r}
TopWords = data.frame()
for( id in c(1:n) )
{
  dayMax = order(doc.tfidf[,id+1], decreasing = TRUE)
  showResult = t(as.data.frame(doc.tfidf[dayMax[1:20],1]))
  TopWords = rbind(TopWords, showResult)
}
rownames(TopWords) = colnames(doc.tfidf)[2:(n+1)]
TopWords = droplevels(TopWords)
kable(TopWords)

```


```{r}
TDM$d = as.character(TDM$d)
AllTop = as.data.frame( table(as.matrix(TopWords)) )
AllTop = AllTop[order(AllTop$Freq, decreasing = TRUE),]

kable(head(AllTop))
```

##就實行的結果來說，可以參考的地方不多，主要問題應該出在於，不管是在哪個月分，台灣遊客對景點的選擇並沒有甚麼不同，導致不管是哪個景點，都會因為在每個月分的檔案中出現太多次反被篩選掉，再者，本報告統計資料量算是目前為止最為龐大的一次，導致垃圾資料也多出很多，但雖然沒辦法明顯的看到甚麼傾向，只要把觀察的範圍擴大，仍是可以看到一些蛛絲馬跡，像是一月有"福袋"這個詞，確實反映出每到日本新年，台灣旅客去日本百貨公司搶購福袋的現象;而九月的一連串大使館事件似乎也討論熱烈，看來tf-idf 比較會反映出某特定時段的事件。

```{r}
filenames = as.array(paste0("./DATA/",colnames(doc.tfidf)[2:(n+1)],".txt"))
sizeResult = apply(filenames, 1, file.size) / 1024
showSize = data.frame(colnames(doc.tfidf)[2:(n+1)], sizeResult)
names(showSize) = c("month", "size_KB")
showSize$month <- fct_inorder(showSize$month)

ggplot(showSize, aes(x = month, y = size_KB)) + geom_bar(stat="identity")
```

##上面為統計各個月分日旅版發文量之柱狀圖，可以發現九月份特別突出，回去比對資料後發現，其原因正來自由於九月日本一連串的天災事件，其中對大使館事件的討論也不少。

##結論：本報告本來是要探討台灣人赴日旅遊會不會因為月份的差異而有所偏好，但是似乎沒有達到很好的成效，仔細觀察了一下詞頻矩陣，隨便一個日本景點在每個月裡可能都會有好幾百筆，這也導致無法透過tfidf進行良好的篩選(還是說有甚麼更好的辦法?)，而雖然結果不盡人意，卻仍可以從結果的一堆廢字中看到一些符合那個月的事件或是相關的東西，甚是欣慰。
