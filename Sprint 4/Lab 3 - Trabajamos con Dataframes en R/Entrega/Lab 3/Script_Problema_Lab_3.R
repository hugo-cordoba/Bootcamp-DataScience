install.packages("tidyverse")
#Primero, asegurémonos de tener las bibliotecas necesarias
library(tidyverse)

# Leer el archivo 'data_PremiumPizza.csv' y convertirlo en tibble
df_venta <- read_csv("data_PremiumPizza.csv") %>% as_tibble()

# Leer el archivo 'calendario_festivos.csv' y convertirlo en tibble
df_calendario <- read_csv("calendario_festivos.csv") %>% as_tibble()

print(df_ventas)
print(df_calendar)
