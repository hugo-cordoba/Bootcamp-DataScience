install.packages("tidyverse")
#Primero, asegurémonos de tener las bibliotecas necesarias
library(tidyverse)

# Leer el archivo 'data_PremiumPizza.csv'
df_venta <- read.csv("data_PremiumPizza.csv", stringsAsFactors = FALSE)

# Leer el archivo 'calendario_festivos.csv'
df_calendario <- read.csv("calendario_festivos.csv", stringsAsFactors = FALSE)

# Verificar que los dataframes se han creado correctamente
str(df_venta)
str(df_calendario)

df_venta %>%
  left_join(df_calendario)

#----------------------------------
# Primero, asegurémonos de que la columna FECHA_SEMANA sea de tipo Date
df_venta$FECHA_SEMANA <- as.Date(df_venta$FECHA_SEMANA)

# Extraer el año de la fecha
df_venta$AÑO <- format(df_venta$FECHA_SEMANA, "%Y")

# Calcular el precio medio por año
precio_medio_por_año <- aggregate(PRECIO ~ AÑO, data = df_venta, FUN = mean)

# Ordenar los resultados de mayor a menor precio
precio_medio_por_año <- precio_medio_por_año[order(-precio_medio_por_año$PRECIO),]

# Mostrar los resultados
print(precio_medio_por_año)

# Identificar el año con el precio medio más alto
año_precio_mas_alto <- precio_medio_por_año$AÑO[1]
precio_mas_alto <- precio_medio_por_año$PRECIO[1]

cat("El año con el precio medio más alto es", año_precio_mas_alto, 
    "con un precio medio de", round(precio_mas_alto, 2))


#----------------------------------------------------
# Asegurarnos de que FECHA_SEMANA es de tipo Date
df_venta$FECHA_SEMANA <- as.Date(df_venta$FECHA_SEMANA)

# Calcular la media de la columna UNIDADES (excluyendo NAs)
media_unidades <- mean(df_venta$UNIDADES, na.rm = TRUE)

# Imputar los NAs en UNIDADES con la media
df_venta$UNIDADES_IMPUTADAS <- ifelse(is.na(df_venta$UNIDADES), media_unidades, df_venta$UNIDADES)

# Extraer el año de FECHA_SEMANA
df_venta$AÑO <- format(df_venta$FECHA_SEMANA, "%Y")

# Filtrar los datos para el año 2018 y sumar las unidades
unidades_2018 <- sum(df_venta$UNIDADES_IMPUTADAS[df_venta$AÑO == "2018"])

# Mostrar el resultado
print(paste("Total de unidades vendidas en 2018:", round(unidades_2018, 0)))


#--------------
pivot_longer(df_venta, cols = c(UNIDADES, TIENDAS.ABIERTAS, PRECIO, GRPs))

#---------------------------------------------
# Asegurarnos de que FECHA_SEMANA es de tipo Date (si no lo es ya)
df_venta$FECHA_SEMANA <- as.Date(df_venta$FECHA_SEMANA)

# Eliminar las observaciones con NAs en la columna 'UNIDADES'
df_venta_clean <- df_venta[!is.na(df_venta$UNIDADES), ]

# Filtrar los datos para enero de 2018
df_enero_2018 <- df_venta_clean[format(df_venta_clean$FECHA_SEMANA, "%Y-%m") == "2018-01", ]

# Calcular la media de 'TIENDAS.ABIERTAS' para enero de 2018
media_tiendas_enero_2018 <- mean(df_enero_2018$TIENDAS.ABIERTAS)

# Mostrar el resultado
print(paste("Media de tiendas abiertas en enero de 2018:", round(media_tiendas_enero_2018, 3)))

#---------------------------------------
# Asegurarnos de que FECHA_SEMANA es de tipo Date (si no lo es ya)
df_venta$FECHA_SEMANA <- as.Date(df_venta$FECHA_SEMANA)

# Extraer el año de la fecha
df_venta$AÑO <- format(df_venta$FECHA_SEMANA, "%Y")

# Filtrar los datos para el año 2019
df_2019 <- df_venta[df_venta$AÑO == "2019", ]

# Calcular la media de GRPs para 2019
grps_medio_2019 <- mean(df_2019$GRPs)

# Mostrar el resultado
print(paste("GRPs medios en 2019:", round(grps_medio_2019, 2)))