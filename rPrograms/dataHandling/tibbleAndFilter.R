#Tibble
??tibble
aq = tbl_df(airquality)
aq
#All rows with temperature > 70
filter(aq, Temp > 70)

#All rows with temperature < 70 and month as May
filter(aq, Temp < 70, Month == 5)

#Filter the 1st days of air quality measures for every month which has wind > 8
filter(aq, Wind > 8, Day == 1)

#Select ozone, wind and temperature columns from the dataset
select(aq, c(1, 3, 4))

#Display all columns exept ozone
select(aq, -Ozone)
