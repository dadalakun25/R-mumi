# 猜數字遊戲
# 基本功能
# 1. 請寫一個由"電腦隨機產生"不同數字的四位數(1A2B遊戲)
# 2. 玩家可"重覆"猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

# 額外功能：每次玩家輸入完四個數字後，檢查玩家的輸入是否正確(錯誤檢查)

ans <- as.character(c(sample(0:9,size=4)))
count <- 0
a <- 0
b <- 0
while(a<4){
  a <- 0
  b <- 0
  guess <- readline("請輸入一四位數:")
  guess <- substring(guess, 1:4, 1:4)
  dit <- c(as.character(seq(from =0, to=9, by=1))) #創造一個1~10的string向量
  result <- unique(guess) %in% unique(dit)         #將輸入值中是數字的轉成true不是數字則為false
  bad_result <- FALSE
  result <- unique(bad_result) %in% unique(result) #如果結果中有至少一個false則進入迴圈直到數入全為數字
  while(result){
    print("輸入的並非數字喔~請重新輸入一四位數~")
    guess <- readline("請輸入一四位數:")
    guess <- substring(guess, 1:4, 1:4)
    result <- unique(guess) %in% unique(dit)
    result <- unique(bad_result) %in% unique(result)
  }
  count <- count+1
  for(i in 1:4){
    for(j in 1:4){
      if(ans[i]==guess[i]){
        a <- a+1
        guess[i] <- "+"
        break
      } else if(ans[i]==guess[j]){
        b <- b+1
        guess[j] <- "+"
        break
      }
    }
  }
  cat("本次結果為: ",a, "A", b, "B")
}
cat("\n恭喜猜對!!!你猜了: ", count, " 次")

