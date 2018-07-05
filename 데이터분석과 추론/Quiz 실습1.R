
# birthday problem
bp <- function(n)
{
  a <- (365-n+1):365
  mul <- 1
  b <- for (i in a){
    mul = mul*i
  }
  return(1-(mul/365^n))
}

for (i in 2:100){
  if (bp(i) > 0.5)
  {
    print(i)
    break
  }
}
bp(23)

# red&black ball problem
rb <- function(a,b,c,d){
  re <- (a/(a+b)) + (c/(c+d))
  return(re/2)
}
rb(99,1,1,1)
