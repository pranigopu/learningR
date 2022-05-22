seqsum <- function(n)
{
  i = 1
  val = 0
  while(i <= n)
  {
    val = val + i
    i = i + 1
  }
  print(val)
}
seqsum(100)