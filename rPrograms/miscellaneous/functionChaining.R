aq = tbl_df(airquality)
#Select the columns Wind, Solar.R, Temp and Month and filter to find data for only months 5 and 6
aq %>% select(Wind, Solar.R, Temp, Month) %>% filter(Month <= 6)

#Equivalent code without chaining
filter(select(aq, Wind, Solar.R, Temp, Month), Month <= 6)

#Hence, we see that chaining applies the result of one function as the argument of another