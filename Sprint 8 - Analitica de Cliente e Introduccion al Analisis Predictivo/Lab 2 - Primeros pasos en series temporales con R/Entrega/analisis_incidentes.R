# Instalar los paquetes necesarios
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")

# Cargar librerías necesarias
library(dplyr)
library(lubridate)
library(ggplot2)

# 1. Primero, necesitaremos preparar los datos usando R:

# Leer el dataset
datos <- read.csv("ruta_del_archivo.csv")

# Convertir la columna de fecha y hora
datos$DateTime <- as.POSIXct(paste(datos$Date, datos$Time), format="%m/%d/%Y %H:%M")

# Encontrar los 3 delitos más frecuentes
delitos_frecuentes <- datos %>%
  group_by(Category) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(3)


# 2. Para cada uno de los tres delitos más frecuentes, crearemos
# visualizaciones con diferentes granularidades:

# Función para crear series temporales por delito
analizar_delito <- function(datos, delito) {
  # Filtrar por tipo de delito
  datos_delito <- datos %>% filter(Category == delito)
  
  # Serie horaria
  serie_horaria <- datos_delito %>%
    group_by(hora = floor_date(DateTime, "hour")) %>%
    summarise(count = n())
  
  # Serie diaria
  serie_diaria <- datos_delito %>%
    group_by(dia = floor_date(DateTime, "day")) %>%
    summarise(count = n())
  
  # Serie mensual
  serie_mensual <- datos_delito %>%
    group_by(mes = floor_date(DateTime, "month")) %>%
    summarise(count = n())
  
  # Visualizaciones
  p1 <- ggplot(serie_horaria, aes(x = hora, y = count)) +
    geom_line() +
    labs(title = paste("Serie horaria -", delito))
  
  p2 <- ggplot(serie_diaria, aes(x = dia, y = count)) +
    geom_line() +
    labs(title = paste("Serie diaria -", delito))
  
  p3 <- ggplot(serie_mensual, aes(x = mes, y = count)) +
    geom_line() +
    labs(title = paste("Serie mensual -", delito))
    
  return(list(p1, p2, p3))
}

# 3. Para detectar valores atípicos:

# Función para detectar outliers
detectar_outliers <- function(datos, delito) {
  serie_diaria <- datos %>%
    filter(Category == delito) %>%
    group_by(dia = floor_date(DateTime, "day")) %>%
    summarise(count = n())
  
  # Calcular límites para outliers usando el método IQR
  Q1 <- quantile(serie_diaria$count, 0.25)
  Q3 <- quantile(serie_diaria$count, 0.75)
  IQR <- Q3 - Q1
  
  limite_superior <- Q3 + 1.5 * IQR
  limite_inferior <- Q1 - 1.5 * IQR
  
  # Marcar outliers
  serie_diaria$outlier <- serie_diaria$count > limite_superior | 
                         serie_diaria$count < limite_inferior
  
  # Visualización
  ggplot(serie_diaria, aes(x = dia, y = count)) +
    geom_line() +
    geom_point(data = subset(serie_diaria, outlier), 
               aes(color = "Outlier"), size = 2) +
    labs(title = paste("Outliers en serie diaria -", delito))
}

# 4. Utilizamos las funciones

# Aplicar el análisis a cada uno de los tres delitos más frecuentes
for(delito in delitos_frecuentes$Category) {
  plots <- analizar_delito(datos, delito)
  print(plots[[1]]) # Serie horaria
  print(plots[[2]]) # Serie diaria
  print(plots[[3]]) # Serie mensual
  
  # Detectar outliers
  print(detectar_outliers(datos, delito))
}
  