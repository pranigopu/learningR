#Select horsepower, weights, rear axle ratio and miles per gallon for 6 cylinders automatic automobile
mtc = tbl_df(mtcars)
mtc %>% select(hp, wt, drat, mpg) %>% filter(am == 1, cyl == 6)
