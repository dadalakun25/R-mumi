p1_f <- 0
p2_f <- 0
count <- 1
ooxx1 <- c(" ","|"," ","|"," ")
ooxx1.5 <- c("一","一","一","一")
ooxx2 <- c(" ","|"," ","|"," ")
ooxx2.5 <- c("一","一","一","一")
ooxx3 <- c(" ","|"," ","|"," ")
print("ゲームスタート！")
cat(ooxx1)
cat(ooxx1.5)
cat(ooxx2)
cat(ooxx2.5)
cat(ooxx3)
while(p1_f & p2_f ==0){
  if(count%%2==1){
    switch(scan(),
            ooxx1[1] <- "O",
            ooxx1[3] <- "O",
            ooxx1[5] <- "O",
            ooxx2[1] <- "O",
            ooxx2[3] <- "O",
            ooxx2[5] <- "O",
            ooxx3[1] <- "O",
            ooxx3[3] <- "O",
            ooxx3[5] <- "O" )
  }else{
    switch(scan(),
           ooxx1[1] <- "X",
           ooxx1[3] <- "X",
           ooxx1[5] <- "X",
           ooxx2[1] <- "X",
           ooxx2[3] <- "X",
           ooxx2[5] <- "X",
           ooxx3[1] <- "X",
           ooxx3[3] <- "X",
           ooxx3[5] <- "X" )
  }
  
  count <- count + 1
}

