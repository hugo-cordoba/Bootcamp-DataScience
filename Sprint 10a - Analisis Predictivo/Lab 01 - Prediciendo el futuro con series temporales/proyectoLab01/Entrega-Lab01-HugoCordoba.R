library(dplyr)
library(stats)
library(ggplot2)
library(gridExtra)
library(forecast)

file <- "spain_energy_market.csv"
color <- "#0081C9"

# READ DATA + EDA

df <- read.csv(file)
df %>% head()

df$name %>% unique()

df_mod <- df %>%
  filter(name == "Demanda real") %>%
  select(datetime, value) %>%
  mutate(date = as.Date(datetime),
         date_month = as.Date(format(date, "%Y-%m-01")),
         date_year = as.Date(format(date, "%Y-01-01")))
df_mod %>% head()

# GRANULARIDADES

df_day <- df_mod %>%
  select(date, value)

df_month <- df_mod %>%
  select(date_month, value) %>%
  group_by(date_month) %>%
  summarise(value = sum(value))

df_year <- df_mod %>%
  select(date_year, value) %>%
  group_by(date_year) %>%
  summarise(value = sum(value))


g1 <- df_year %>%
  ggplot(aes(x=date, y=value)) +
  geom_line(color=color) +
  scale_x_date(date_labels = "%Y") +
  ggtitle("Demanda real Anual de electricidad - Espana")

g2 <- df_month %>%
  ggplot(aes(x=date_month, y=value)) +
  geom_line(color=color) +
  scale_x_date(date_labels = "%Y") +
  ggtitle("Demanda real Mensual de electricidad - Espana")

g3 <- df_day %>%
  ggplot(aes(x=date, y=value)) +
  geom_line(color=color) +
  scale_x_date(date_labels = "%Y") +
  ggtitle("Demanda real Diaria de electricidad - Espana")

grid.arrange(g1, g2, g3, nrow=3)


# REAJUSTAR SERIE MENSUAL

df_month %>%
  ggplot(aes(x=date_month, y=value)) +
  geom_line(color=color) +
  scale_x_date(date_labels = "%Y") +
  ggtitle("Demanda real Mensual de electricidad - Espana")

df_month_reajustado <- df_mod %>%
  group_by(date_month) %>%
  summarise(value = mean(value))

df_month_reajustado %>%
  ggplot(aes(x=date_month, y=value)) +
  geom_line(color=color) +
  scale_x_date(date_labels = "%Y") +
  ggtitle("Demanda real Mensual reajustado de electricidad - Espana")

# 2. DESCOMPOSICION

ts_demanda_freq_semanal <- ts(
  data = df_day$value,
  frequency = 7)

ts_demanda_freq_anual <- ts(
  data = df_day$value,
  frequency = 365
)

ts_demanda_freq_semanal %>% seasonplot(col=color, type="1")

ts_demanda_freq_anual %>% seasonplot(col=color, type="1")


decompose_model_demanda <- ts_demanda_freq_anual %>%
  stats::decompose(type = "additive")

decompose_model_demanda %>%
  plot()


# 3. SUAVIZADO EXPONENCIAL

ets_model_demanda <- forecast::ets(ts_demanda_freq_semanal)
ets_model_demanda

demanda_forecast_ets <- forecast::forecast(ets_model_demanda, h=7)
demanda_forecast_ets %>% autoplot()

zoom_value <- 30

demanda_forecast_ets$mean <- demanda_forecast_ets$mean %>%
  tail(zoom_value)

demanda_forecast_ets$x <- demanda_forecast_ets$x %>%
  tail(zoom_value)

demanda_forecast_ets$upper <- demanda_forecast_ets$upper %>%
  tail(zoom_value)

demanda_forecast_ets$lower <- demanda_forecast_ets$lower %>%
  tail(zoom_value)

demanda_forecast_ets %>% autoplot()


