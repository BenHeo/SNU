setwd('./test')

# system(command = "r --no-restore --no-save <test.r> test_out.txt")
system(command = "r --no-restore --no-save <test.r> test_out.txt", wait = FALSE) # 명령어 내리고 백그라운드에서 돌리고 다른 작업 가능!!

# system(command = "r --no-restore --no-save --args 10 <test.r> test_out.txt", wait = FALSE) # 변수를 직접 10이라고 입력했음

for (i in 1:8)
{
  system(command = paste0("r --no-restore --no-save --args ", 10, " <test.r> test_out", i, ".txt"), wait = FALSE)
}


# tasklist를 R을 통해 실행하고 그 중에서 R.exe가 몇 개나 실행되고 있는지 확인한다.
system("tasklist")
task_list <- system("tasklist", intern = TRUE) # intern = TRUE를 해야 저장 형태로 만들 수 있다
head(task_list)

b <-gregexpr(" ", a[[3]])[[1]]
i = 5
substring(a[[i]],1,b[1])
substring(a[[i]],b[1]+1,b[2])

gsub("(^ +)|( +$)", "", substring(a[[i]],1,b[1]))
gsub("(^ +)|( +$)", "", substring(a[[i]],b[1]+1,b[2]))

taskResult <- matrix("", length(a)-3, 5)  
nn <- c("image" ,"PID" , "session_name" ,"session_num" , "memory")
colnames(taskResult) <- nn
for(i in 4L:length(a))
{
  for (j in 1:5)
  {
    if (j == 1) tmp <- substring(a[[i]],1,b[1])
    if (j == 5) tmp <- substring(a[[i]],b[4]+1)
    if (j!=1 & j!=5) tmp <- substring(a[[i]],b[j-1]+1,b[j])
    taskResult[i-3,j] <- gsub("(^ +)|( +$)", "", tmp)
  }
}

taskRead <- function()
{
  a <- system("tasklist", intern = T)
  b <-gregexpr(" ", a[[3]])[[1]]
  taskResult <- matrix("", length(a)-3, 5)  
  nn <- c("image" ,"PID" , "session_name" ,"session_num" , "memory")
  colnames(taskResult) <- nn
  for(i in 4L:length(a))
  {
    for (j in 1:5)
    {
      if (j == 1) tmp <- substring(a[[i]],1,b[1])
      if (j == 5) tmp <- substring(a[[i]],b[4]+1)
      if (j!=1 & j!=5) tmp <- substring(a[[i]],b[j-1]+1,b[j])
      taskResult[i-3,j] <- gsub("(^ +)|( +$)", "", tmp)
    }
  }
  return(taskResult)
}

nMaxTask <-5
tr <- taskRead()
nCurrentTask  <- sum(tr[,1]=="R.exe")
nAvailableTask <- nMaxTask - nCurrentTask

########################################################
a = readLines("test.R") # test.R을 읽는다
a[8] = " a = 1" # test.R의 8번째 부분을 수정한다
cat(file = "test_m.R", sep = '\n') # 수정한 것을 저장한다


