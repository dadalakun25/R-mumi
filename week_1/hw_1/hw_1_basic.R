##Task 1

iris
dim(iris)
head(iris)
tail(iris)
str(iris)
summary(iris)

##Task 2
for(i in c(1:9)){
  for(j in c(1:9)){
    cat(i,"*",j,"=",i*j," ")
  }
  cat("\n")
}

##Task 3
nums <- c(sample(10:100, size = 10))
nums
ptr <- 1
str2 <- vector()
for(i in 1:10){
  if(nums[i] == 66){
    print("太66666666666了")
    break
  }  
  else if(nums[i] >= 50 && nums[i]%%2 == 0){
    str2[ptr] <- nums[i]
    ptr <- ptr + 1
  }
  if(i==10){
    cat("偶數且大於50 : ",str2)
  }
}

##Task 4
year <- as.numeric(readline("請輸入一西元年: "))
if(year<1){
  print("輸入錯誤")
} else if(year%%4 == 0){
    if(year%%100 != 0 || year%%400 == 0){
      print("是閏年!")
    }  else{
      print("不是閏年")
    }
} else{
  print("不是閏年")
}




