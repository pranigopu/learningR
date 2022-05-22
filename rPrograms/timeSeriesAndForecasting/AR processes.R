#' INTRODUCTION ON AUTOREGRESSIVE MODELS
#------------
#' An autoregressive (AR) model is a statistical time series model wherein
#' each observation is linearly dependent on past observations. The
#' general equation of an AR model of order p is given by:
#______
#' z-bar_t = phi_1 • z-bar_(t-1) + phi_2 • z-bar_(t-2) + ... + phi_p • z-bar_(t - p) + a_t
#______
#' where
# 1
#' p is the maximum lag at which past observations are included,
#' i.e. z-bar_t depends on all past observations until z-bar_(t - p).
# 2
#' z-bar_t = z_t - E(z_t)
# 3
#' a_t is the error term
#------------
#' Our focus is on AR(1) models i.e. AR models wherein current
#' observations only depend on the immediately preceding observation.
#' The general equation is given by:
#______
#' z-bar_t = phi • z-bar_(t-1) + a_t
#______
#========================
#' GENERATING RANDOM AR(1) PROCESSES
#------------
#' Here, we will be generating a random time series based on AR(1)
#' model, with a given phi value. In other words, we will be
#' generating a time series model that can be accurately modelled by
#' AR(1) model for the given phi value.
#------------
#' SIDE NOTE
#______
#' ARIMA => Autoregressive Integrated Moving Average
#______
#' ARIMA is a generalization of AR models, MA (moving average) models
#' and I (integrated) models. These components are explained below:
# 1
#' AR (Autoregression): Current values depend on some number of
#' past observations upto a certain lag.
# 2
#' I (Integrated): A time series is said to be integrated of order d
#' if taking repeated differences between z_t values results in a
#' stationary process. For example, if z_t is integrated of order 2,
#' then z_t - z_(t-1) - z_(t-2) is a stationary process.
#' Here, 'integrated' refers the property of the time series being
#' integrated at some order, i.e. it refers to time series that are
#' non-stationary with respect to mean, but can become stationary
#' after some number of repeated differencing.
# 3
#' MA (Moving Average): The value at each time point is smoothened
#' by averaging some number of past and future values around it.
#______
#' ARIMA model has three integer components, namely:
# 1
#' Order of autoregression of the time series (p). This indicates
#' the maximum lag at which observations are included i.e. extent to
#' which past values are used to model current values.
# 2
#' Order of integration of the time series (d). This
#' specifies the non-seasonal (i.e. trend) part of the
#' model.
# 3
#' Order of moving average of the time series (q). This indicates the
#' number of values around and including the current time point's
#' value that are averaged, in order to smoothen the current time
#' point's value. For example, MA(3) implies that each time point's
#' value is replaced by the average of the just preceding, current
#' and just succeeding value.
#______
#' ARIMA models are denoted as ARIMA(p, d, q). Note that here,
#' seasonality is not considered, only trend. To include the
#' seasonal component, we must use seasonal ARIMA, which is beyond the
#' scope of this assignment.
#------------
#' Generating AR(1) based time series using ARIMA
#______
#' ARIMA generalizes AR models, differencing models and MA models.
#' Hence, we can use the function for generating time series based
#' on ARIMA model to generate time series based on AR model as well.
#' In particular, ARIMA(1, 0, 0) = AR(1) (i.e. p = 1, d = 0, q = 0).
#------------
#' Generating AR(1) based time series using R
#______
#' To generate a time series based on AR(1), we can use the function
#' arima.sim, which generates time series based on ARIMA model.
#' The arguments required are as follows:
# 1
#' A list with component 'ar', 'ma' (or both) giving the AR and MA
#' coefficients respectively. For our purposes, we only need to give
#' coefficients for AR, and only one coefficient at that, since we
#' want to generate a time series based on AR(1) model.
#' An empty list gives an ARIMA(0, 0, 0) model, that is white noise.
# 2
#' 'n' i.e. the length ofthe output series. It is a strictly positive
#' integer.
#______
#' Function for generating a time series & its time plot & ACF plot
new_AR1_ts = function(phi, n)
{
  AR1_ts = arima.sim(model = list(ar = phi), n = n)
  print(AR1_ts[1:5])
  print(summary(AR1))
  
  # Time plot
  ts.plot(
    AR1_ts,
    main = " ",
    xlab = "Time",
    ylab = "Value")
  
  # ACF plot
  acf(
    AR1,
    main = "")
}
#------------
# Given phi values
phiValues = c(-0.8, -0.5, -0.3, 0.3, 0.7, 0.8)
# Generating time series, time plots and ACF plots
par(mfrow = c(1, 2))
for(phi in phiValues){new_AR1_ts(phi, 500)}
#========================
#' GENERATING AND COMPARING TWO ACF(1) BASED TIME SERIES
#------------
#' Here, we put n = 10000. The phi values for the two AR(1) models
#' (based on which the two time series will be generated) are 0.3
#' and 0.9 respectively.
par(mfrow = c(1, 2))
for(phi in c(0.3, 0.9)){new_AR1_ts(phi, 10000)}