library(ggplot2)
setwd("/Users/pranav/Documents/Study/computerScience/programming/r/data")
dat = read.csv("patientSatisfaction.csv")
head(dat)
y = dat$Satisfaction

#' FUNCTION FOR PLOTTING THE GRAPHS FOR A MODEL
modelPlots = function(x, y, xName, yName)
{
  # MODEL
  y_x_model = lm(y~x)
  y_x_model
  # MODEL SUMMARY
  summary(y_x_model)
  # DATA FRAME FROM MODEL
  y_fitted = y_x_model$coefficients[1] + y_x_model$coefficients[2]*x
  y_x_dat = data.frame(y, x, y_fitted)
  # PLOTS
  # Fitted vs. actual
  plotName = paste(yName, "w.r.t.", xName)
  y_x_plot = ggplot(y_x_dat, aes(x, y))
  y_x_plot = y_x_plot + geom_point()
  y_x_plot = y_x_plot + geom_smooth(aes(x, y_fitted))
  y_x_plot = y_x_plot + geom_segment(aes(xend = x, yend = y_fitted))
  y_x_plot = y_x_plot + labs(title = plotName, x = xName, y = yName)
  par(mfrow = c(1, 1))
  print(y_x_plot)
  # Residual plots
  par(mfrow = c(1, 1))
  plot(y_x_model)
}

#' FINDING PLOTS FOR EVERY MODEL
modelPlots(dat$Age, y, "Age", "Satisfaction")
modelPlots(dat$Severity, y, "Severity", "Satisfaction")
modelPlots(dat$Anxiety, y, "Anxiety", "Satisfaction")
