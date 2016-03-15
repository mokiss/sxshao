## Forecast with ARIMA model

forecastArima <- function(variables, n.ahead = 1){
  atm.prs <- ts(variables, frequency=7)
  fit <- auto.arima(variables,seasonal=FALSE)
  order.fit=c(fit$arma[1], fit$arma[6] , fit$arma[2])
  fit_best=arima(atm.prs,order=order.fit)
  fore <- forecast(fit_best, h= n.ahead) 
  fore$mean[n.ahead]
}




