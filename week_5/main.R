source('pttTestFunction.R')
id = c(5218:5302)
URL = paste0("https://www.ptt.cc/bbs/Japan_Travel/index", id, ".html")
filename = paste0("Oct", ".txt")
pttTestFunction(URL[1], filename[1])
mapply(pttTestFunction, URL = URL, filename = filename)

