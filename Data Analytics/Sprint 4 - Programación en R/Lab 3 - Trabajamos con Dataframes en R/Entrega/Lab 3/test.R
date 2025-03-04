install.packages("tidyverse")
install.packages("DT")
install.packages("kableExtra")

# Leer los archivos CSV
df_venta <- read.csv("data_PremiumPizza.csv", stringsAsFactors = FALSE)
df_calendario <- read.csv("calendario_festivos.csv", stringsAsFactors = FALSE)

# Convertir FECHA_SEMANA a tipo Date en df_venta
df_venta$FECHA_SEMANA <- as.Date(df_venta$FECHA_SEMANA)

# Separar la columna en df_calendario y convertir la fecha a tipo Date
df_calendario <- df_calendario %>%
  separate(FECHA_SEMANA_LUNES.FESTIVOS, 
           into = c("FECHA_SEMANA", "FESTIVOS"), 
           sep = ";") %>%
  mutate(FECHA_SEMANA = as.Date(FECHA_SEMANA),
         FESTIVOS = as.numeric(FESTIVOS))

# Unir dataframes
df_combined <- df_venta %>%
  left_join(df_calendario, by = "FECHA_SEMANA")

# Verificar la unión
head(df_combined)

# Análisis de precios
df_venta$AÑO <- format(df_venta$FECHA_SEMANA, "%Y")
precio_medio_por_año <- aggregate(PRECIO ~ AÑO, data = df_venta, FUN = mean)
precio_medio_por_año <- precio_medio_por_año[order(-precio_medio_por_año$PRECIO),]

año_precio_mas_alto <- precio_medio_por_año$AÑO[1]
precio_mas_alto <- precio_medio_por_año$PRECIO[1]

# Imputación de datos faltantes y cálculo de ventas 2018
media_unidades <- mean(df_venta$UNIDADES, na.rm = TRUE)
df_venta$UNIDADES_IMPUTADAS <- ifelse(is.na(df_venta$UNIDADES), media_unidades, df_venta$UNIDADES)

unidades_2018 <- sum(df_venta$UNIDADES_IMPUTADAS[df_venta$AÑO == "2018"])

# Transformación a formato long
df_long <- pivot_longer(df_venta, cols = c(UNIDADES, TIENDAS.ABIERTAS, PRECIO, GRPs))

# Análisis de tiendas abiertas en enero 2018
df_venta_clean <- df_venta[!is.na(df_venta$UNIDADES), ]
df_enero_2018 <- df_venta_clean[format(df_venta_clean$FECHA_SEMANA, "%Y-%m") == "2018-01", ]
media_tiendas_enero_2018 <- mean(df_enero_2018$TIENDAS.ABIERTAS)

# Análisis de GRPs en 2019
df_2019 <- df_venta[df_venta$AÑO == "2019", ]
grps_medio_2019 <- mean(df_2019$GRPs)