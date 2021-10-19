#MUTATE
#Add a new column to the airquality dataframe that displays temperature in Celsius
aq = tbl_df(airquality)
mutate(aq, Temp.C = (Temp - 32)*5/9)

#Convert weights of each automobile from lbs to kg and display that in a column wt.kg
mtc = tbl_df(mtcars) 
mtc
mutate(mtc, wt.kg = wt*0.453)

#Do the above with chaining and show only wt and wt.kg
mtc %>% mutate(wt.kg = wt*0.453) %>% select(wt, wt.kg)
