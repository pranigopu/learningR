library(dplyr)
library(tibble)
#Construct 2 dataframes, df1 and df2.
#df1: has customer ID 1 to 6 
#   : 2 products; laptop repated 3x
#               ; tv repeated 3x
cust.id = c(1:6)
prod = c(rep("laptop", times = 3), rep("tv", times = 3))
df1 = data.frame(cust.id, prod)
df1

#df2: has customer ID 4 to 7
#   : 2 products; tv repeated 2x
#               ; microwave repeated 2x
cust.id = c(4:7)
prod = c(rep("tv", times = 2), rep("microwave", times = 2))
df2 = data.frame(cust.id, prod)
df2

#Union
union(df1, df2)
union_all(df1, df2)

#Intersect
intersect(df1, df2)

#Set diff
setdiff(df1, df2)
setdiff(df2, df1)

#Rename
df3 = rename(df1, cust.id = id, prod = commodity) 
df3
