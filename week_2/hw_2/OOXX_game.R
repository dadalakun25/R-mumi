p1_f <- 0
p2_f <- 0
count <- 1
ooxx <- c(as.character(seq(from =1, to=9, by=1)))
print("OOXX遊戲開始")
while(p1_f==p2_f){
  cat(ooxx[1],"|",ooxx[2],"|",ooxx[3])
  cat("\n一","一","一","一\n")
  cat(ooxx[4],"|",ooxx[5],"|",ooxx[6])
  cat("\n一","一","一","一\n")
  cat(ooxx[7],"|",ooxx[8],"|",ooxx[9])
  print("請輸入1~9一數字")
  if(count==9){
    print("和局")
    break
  }else if(count%%2==1){
    switch(scan(),
           ooxx[1] <- "O",
           ooxx[2] <- "O",
           ooxx[3] <- "O",
           ooxx[4] <- "O",
           ooxx[5] <- "O",
           ooxx[6] <- "O",
           ooxx[7] <- "O",
           ooxx[8] <- "O",
           ooxx[9] <- "O" )
  }else{
    switch(scan(),
           ooxx[1] <- "X",
           ooxx[2] <- "X",
           ooxx[3] <- "X",
           ooxx[4] <- "X",
           ooxx[5] <- "X",
           ooxx[6] <- "X",
           ooxx[7] <- "X",
           ooxx[8] <- "X",
           ooxx[9] <- "X" )
  }
  if(count%%2==1){
    if(ooxx[1]==ooxx[2]){
      if(ooxx[2]==ooxx[3]) p1_f <- 1  
    }
    if(ooxx[4]==ooxx[5]){
      if(ooxx[5]==ooxx[6]) p1_f <- 1  
    }
    if(ooxx[7]==ooxx[8]){
      if(ooxx[8]==ooxx[9]) p1_f <- 1  
    }
    if(ooxx[1]==ooxx[4]){
      if(ooxx[4]==ooxx[7]) p1_f <- 1  
    }
    if(ooxx[2]==ooxx[5]){
      if(ooxx[5]==ooxx[8]) p1_f <- 1  
    }
    if(ooxx[3]==ooxx[6]){
      if(ooxx[6]==ooxx[9]) p1_f <- 1  
    }
    if(ooxx[1]==ooxx[5]){
      if(ooxx[5]==ooxx[9]) p1_f <- 1  
    }
    if(ooxx[3]==ooxx[5]){
      if(ooxx[5]==ooxx[7]) p1_f <- 1  
    }
  }else{
    if(ooxx[1]==ooxx[2]){
      if(ooxx[2]==ooxx[3]) p2_f <- 1  
    }
    if(ooxx[4]==ooxx[5]){
      if(ooxx[5]==ooxx[6]) p2_f <- 1  
    }
    if(ooxx[7]==ooxx[8]){
      if(ooxx[8]==ooxx[9]) p2_f <- 1  
    }
    if(ooxx[1]==ooxx[4]){
      if(ooxx[4]==ooxx[7]) p2_f <- 1  
    }
    if(ooxx[2]==ooxx[5]){
      if(ooxx[5]==ooxx[8]) p2_f <- 1  
    }
    if(ooxx[3]==ooxx[6]){
      if(ooxx[6]==ooxx[9]) p2_f <- 1  
    }
    if(ooxx[1]==ooxx[5]){
      if(ooxx[5]==ooxx[9]) p2_f <- 1  
    }
    if(ooxx[3]==ooxx[5]){
      if(ooxx[5]==ooxx[7]) p2_f <- 1  
    }
  }
  count <- count + 1  
}

if(p1_f==1){
  print("玩家一獲勝!")
} else if(p2_f==1) print("玩家二獲勝!")

