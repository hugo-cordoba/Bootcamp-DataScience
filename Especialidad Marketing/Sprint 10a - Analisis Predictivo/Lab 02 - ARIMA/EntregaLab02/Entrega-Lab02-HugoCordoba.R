library(dplyr)
library(stats)
library(ggplot2)
library(gridExtra)
library(forecast)

file <- "spain_energy_market-1.csv"
color <- "#0081C9"

# 1. READ DATA

df_month <- read.csv(file) %>%
  filter(name == "Demanda real") %>%
  mutate(date = as.Date(datetime),
         date_month = as.Date(format(date, "%Y-%m-01"))) %>%
  select(date_month, value) %>%
  group_by(date_month) %>%
  summarise(value = mean(value))

ts_demanda_freq_anual <- ts(
  data = df_month$value,
  frequency = 12
)

df_month %>%
  ggplot(aes(x=date_month, y=value)) + 
  geom_line(color=color) +
  scale_x_date(date_labels = "%Y") +
  ggtitle("Demanda de electricidad media por mes - Espana")

# 2. SERIE ESTACIONARIA

ts_demanda_freq_anual_diff <- ts_demanda_freq_anual %>%
  diff()

ts_demanda_freq_anual_diff %>%
  autoplot(color=color)

# 3. ARIMA

tsdisplay(ts_demanda_freq_anual_diff, lag.max=36)

arima <- stats::arima(
  ts_demanda_freq_anual_diff,
  order = c(0, 0, 0),
  seasonal = list(order = c(0, 0, 1), period=12)
)
arima

tsdisplay(arima$residuals, lag.max=36)


arima <- stats::arima(ts_demanda_freq_anual_diff,
  order = c(0, 0, 1),
  seasonal = list(order = c(1, 2, 0), period=12)
)
tsdisplay(arima$residuals, lag.max=36)

arima

# 4. AUTO-ARIMA

sarima_fitted <- forecast::auto.arima(ts_demanda_freq_anual_diff,
                                      seasonal = TRUE,
                                      stepwise = TRUE)
sarima_fitted

sarima_fitted %>% forecast::forecast(h=12) %>% autoplot()
