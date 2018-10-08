rm(list=ls(all.names = TRUE))
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines, encoding="BIG5")
docs <- Corpus(VectorSource(files))
#移除可能有問題的符號
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
docs <- tm_map(docs, toSpace, "※")
docs <- tm_map(docs, toSpace, "◆")
docs <- tm_map(docs, toSpace, "‧")
docs <- tm_map(docs, toSpace, "推")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "在")
docs <- tm_map(docs, toSpace, "可以")
docs <- tm_map(docs, toSpace, "要")
docs <- tm_map(docs, toSpace, "到")
docs <- tm_map(docs, toSpace, "都")
docs <- tm_map(docs, toSpace, "走")
docs <- tm_map(docs, toSpace, "有")
docs <- tm_map(docs, toSpace, "不")
docs <- tm_map(docs, toSpace, "也")
docs <- tm_map(docs, toSpace, "就")
docs <- tm_map(docs, toSpace, "們")
docs <- tm_map(docs, toSpace, "說")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "我")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "看板")
docs <- tm_map(docs, toSpace, "作者")
docs <- tm_map(docs, toSpace, "發信站")
docs <- tm_map(docs, toSpace, "批踢踢實業坊")
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
#語詞詞幹化 (stemmization)
#以英文為例
#https://zh.wikipedia.org/wiki/%E8%AF%8D%E5%B9%B2%E6%8F%90%E5%8F%96
#library(SnowballC)
#確保任何形式的單字只會轉換成相同詞性出現一次
#docs <- tm_map(docs, stemDocument)

mixseg = worker()
my.data <- read.csv("mountain.csv")
str(my.data)
my.data$玉山 <- as.character(my.data$玉山)
new_user_word(mixseg,my.data$玉山)

jieba_tokenizer=function(d){
  #unlist(segment(d[[1]],mixseg))
  unlist(segment(mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))

library(ggplot2)
p <- ggplot(subset(freqFrame, Freq>1000), aes(x = reorder(Var1, -Freq), y= Freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p
wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.1),min.freq=10,x_max=500,max.words=200,
          random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
