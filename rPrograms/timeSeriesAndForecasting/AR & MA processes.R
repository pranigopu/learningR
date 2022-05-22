#' QUESTION 1
#========================
#' AIM
#------------
#' Generate an AR(2) process of size 1000. Choose the parameters from the stationarity region.
#========================
#' NOTES
#------------
#' AR(2) process
#______
#' An AR(2) process is an autoregressive process of order 2, i.e. the value
#' of an observation linearly depends on the previous two observations, i.e.
#' i.e. z-bar_t depends on z-bar_(t-1) and z-bar_(t-2).
#' AR(2) models are given by:
#______
#' z-bar_t = phi_1 • z-bar_(t-1) + phi_2 • z-bar_(t-2) + a_t
#------------
#' Stationarity region
#______
#' If all the coefficients i.e. phi_1, phi_2, etc. of an AR model lie between -1 and 1,
#' the AR process will be stationary.
#========================
#' GENERATING A RANDOM AR(2) PROCESS
#------------
#' To generate an AR process in R, we will use the function to
#' generate autoregressive moving average processes (ARIMA), since ARIMA generalizes
#' autoregressive, integrated and moving average models. With the right parameters,
#' we can create AR(2) models as required.
#------------
#' To generate an AR process in R, we will use the 'arima.sim' function.
#' The arguments are 'model' (for taking model parameters) and 'n' (for determining the output time series size).
#' In 'model', we will give a list object containing the element
#' ar = c(phi_1, phi_2), where 'ar' denotes that the parameters belong to an AR process,
#' phi_1 and phi_2 are the AR(2) model parameters.
#' Since no other parameters will be given, the output will be an AR(2) process specifically.
#------------
AR2_ts = arima.sim(model = list(ar = c(0.1, 0.2)), n = 1000)
summary(AR2_ts)
ts.plot(
  AR2_ts,
  main = "AR(2) based time series",
  xlab = "Time",
  ylab = "Value")

acf(
  AR2_ts,
  main = "ACF plot of AR(2) based time series")
#************************************************
#' QUESTION 2
#========================
#' AIM
#------------
#' Generate an MA(3) process of size 500. Is the process stationary? Comment on its acf and pacf plots.
#========================
#' NOTES
#------------
#' MA(3) process
#______
#' A MA(3) process is a moving average process of order 3, i.e. the value of an
#' observation linearly depends on the current as well as the past three error terms (white noise sequence elements).
#' i.e. z-bar_t depends on a_t, a_(t-1), a_(t-2) and a_(t-3).
#' MA(3) models are given by:
#' z-bar_t = a_t - theta_1 • a_(t-1) - theta_2 • a_(t-2) - theta_3 • a_(t-3)
#========================
#' GENERATING A RANDOM MA(3) PROCESS
#------------
#' The method and function used here is the same that was used to generate the AR(2) process time series
#' in the previous question. The difference is in the arguments,
#'  'model', we will give a list object containing the element
#' ma = c(theta_1, theta_2, theta_3), where 'ma' denotes that the parameters belong to an MA process,
#' and theta_2 are the MA(3) parameters. Since no other parameters will be given,
#' the output will be an MA(3) process specifically.
#------------
MA3_ts = arima.sim(model = list(ma = c(0.5, 0.6, -0.3)), n = 500)
summary(MA3_ts)
ts.plot(
  MA3_ts,
  main = "MA(3) based time series",
  xlab = "Time",
  ylab = "Value")

acf(
  AR2_ts,
  main = "ACF plot of MA(3) based time series")

pacf(
  AR2_ts,
  main = "PACF plot of MA(3) based time series")
#========================
#' NOTES
#------------
#' Partial autocorrelation has the same logic as partial correlation, where to compare
#' the correlation between two variables when there are more variables present, we control
#' for the effect of the other variables to give a more accurate association between the two
#' variables of focus. Similarly, partial autocorrelation aims to find the correlation between
#' the current observation and a lagged observation, while controlling for the effect of other lagged observations.
#' This gives a more accurate view about the autocorrelation between the current observation
#' and a past observation at a certain lag.