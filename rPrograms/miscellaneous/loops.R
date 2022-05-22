x = 0; while(x < 10) {x = x + 4; print(x)}

#1
i = 1
sum1 = 0
while(i < 21) {sum1 = sum1 + i + 1; i = i + 1}
print(sum1)

#2
j = -10
sum2 = 0
while(j < 16) {sum2 = sum2 + j; j = j + 1}
print(sum2)

#3
k = 3
sum3 = 0
while(k <= 24) {sum3 = sum3 + k; k = k + 3}
print(sum3)

#4
t1 = 0
t2 = 1
temp
while(t2 < 100) {print(t1); temp = t1; t1 = t1 + t2; t2 = temp}

#5
sum5 = 0
for(i in 1:10)
{print(i)
  sum5 = sum5 + 2^i
  }
print(sum5)
