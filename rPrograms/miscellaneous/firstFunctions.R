f1 <- function(x){x + x}
f1(10)

f2 <- function(t, z)
{
  val = 3*z
  val = val*t
  print(val)
}
t = 5
z= 9
f2(t, z)

f3 <- function(n)
{
  i = 1
  val = 1
  while(i <= n)
  {
    val = val*i
    i = i + 1
  }
  print(val)
}
f3(5)


