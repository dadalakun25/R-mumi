library(rvest)
str888 <- "https://www.ptt.cc/bbs/KanColle/index.html"
str889 <- read_html(str888)
str890 <- html_nodes(str889,".title")
str891 <- html_text(str890)
cat(str891)