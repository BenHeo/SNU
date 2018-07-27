options(echo=TRUE) # R 메시지를 다 출력하겠다는 의미
args <- commandArgs(trailingOnly = TRUE) # command에서 정보를 입력 받을 수 있다
print(args)
idx = as.integer(args[1])
rm(args)
a = 0
for (i in 1:30)
{
  Sys.sleep(1)
  a = a + 1
}
a = a*idx
print(idx)
save.image(paste0("test_",idx,".rdata"))