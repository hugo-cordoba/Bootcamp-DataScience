

# 1 En esta primera línea tenemos que leer el fichero data_PremiumPizza.csv de la 
#carpeta data y guardar el resultado en un dataframe llamado df

# Leer el fichero CSV y guardar el resultado en el dataframe df
df <- read.csv("data/data_PremiumPizza.csv")

# Mostrar las primeras filas del dataframe para verificar la carga
head(df)

# --------------------------------

# 2 La variable UNIDADES contiene NAs. Sustituyelos por la mediana

# Calcular la mediana de la columna UNIDADES, ignorando los NA
mediana_unidades <- median(df$UNIDADES, na.rm = TRUE)

# Sustituir los valores NA en la columna UNIDADES por la mediana calculada
df$UNIDADES[is.na(df$UNIDADES)] <- mediana_unidades

head(df)

# --------------------------------

# 3 Añade al dataframe una variable llamada VENTA que se calcule como la multiplicacion
#de UNIDADES Y PRECIO

# Convertir la columna PRECIO a tipo numeric
df$PRECIO <- as.numeric(as.character(df$PRECIO))

# Añadir una nueva columna VENTA al dataframe df
df$VENTA <- df$UNIDADES * df$PRECIO

head(df)

# --------------------------------

# 4 Construye un bucle que recorra el dataframe fila por fila e imprima un mensaje de aviso
#cuando la venta de una semana supere 12 millones de euros. 
#El mensaje debe indicar la semana en la que ha ocurrido

# Recorre el dataframe fila por fila
for (i in 1:nrow(df)) {
  # Verificar si la venta en la fila actual supera los 12 millones de euros
  if (df$VENTA[i] > 12000000) {
    # Imprimir un mensaje de aviso con la semana en la que ha ocurrido
    cat("Aviso: En la semana", df$FECHA_SEMANA[i], "la venta ha superado los 12 millones de euros.\n")
  }
}

# --------------------------------

# 5 Construye un bucle que recorra el dataframe fila por fila en orden temporal y vaya
#calculando la suma acumulada de unidades vendidas. Cuando dicha suma alcance los 10 millones de unidades
#el bucle debe parar y se debe mostrar un mensaje que indique en que semana se ha alcanzado
#el valor y cuantas semanas se ha tardado en hacerlo (contando desde el inicio del dataframe)

# Inicializar variables para la suma acumulada y el contador de semanas
suma_acumulada <- 0
semanas_contadas <- 0

# Iterar sobre cada fila del dataframe en orden temporal
for (i in 1:nrow(df)) {
  # Sumar las unidades vendidas en la fila actual
  suma_acumulada <- suma_acumulada + df$UNIDADES[i]
  
  # Incrementar el contador de semanas
  semanas_contadas <- semanas_contadas + 1
  
  # Verificar si la suma acumulada ha alcanzado o superado los 10 millones de unidades
  if (suma_acumulada >= 10000000) {
    # Imprimir el mensaje con la semana en la que se alcanzó el umbral y el número de semanas transcurridas
    cat("La suma acumulada de unidades ha alcanzado los 10 millones en la semana", df$FECHA_SEMANA[i], ".\n")
    cat("Se ha tardado", semanas_contadas, "semanas en alcanzar este valor.\n")
    break  # Salir del bucle
  }
}

# --------------------------------

# 6 Ahora tenemos que construir una funcion llamada nunique que reciba un vector
#y devuelva el numero de elementos diferentes que contiene 
#(pista, necesitamos las funciones unique y length)

# Definir la función nunique
nunique <- function(vector) {
  # Obtener los elementos únicos del vector
  elementos_unicos <- unique(vector)
  
  # Calcular el número de elementos únicos
  numero_elementos_unicos <- length(elementos_unicos)
  
  # Devolver el número de elementos únicos
  return(numero_elementos_unicos)
}

# Ejemplo de uso
# Crear un vector de prueba
vector_prueba <- c(1, 2, 2, 3, 4, 4, 5)

# Llamar a la función nunique y mostrar el resultado
print(nunique(vector_prueba))

# --------------------------------

# 7 A continuación tenemos que aplicar la funcion nunique() sobre cada una de
#las columnas del dataframe df
#Para eso os recomiendo usar lapply()

nunique <- function(vector) {
  # Obtener los elementos únicos del vector
  elementos_unicos <- unique(vector)
  
  # Calcular el número de elementos únicos
  numero_elementos_unicos <- length(elementos_unicos)
  
  # Devolver el número de elementos únicos
  return(numero_elementos_unicos)
}

# Aplicar la función nunique a cada columna del dataframe df
resultados <- lapply(df, nunique)

# Convertir el resultado en un vector o dataframe para una mejor visualización
resultados <- unlist(resultados)

# Mostrar el resultado
print(resultados)


# --------------------------------

# 8 Guardamos el dataframe df en el archivo "dato_output.csv" (usando la coma como separador)

# Guardar el dataframe df en un archivo CSV con el nombre "dato_output.csv"
write.csv(df, file = "data/dato_output.csv", row.names = FALSE)



df <- read.csv('data_PremiumPizza.csv')
df$UNIDADES[is.na(df$UNIDADES)] <- median(df$UNIDADES, na.rm = T)

# Opcion 1
for (i in seq(1,nrow(df))){
  if (df[i, 'UNIDADES'] > 2000000){
    print(paste('Se han superado 2 millones'))
  }
}

# Opcion 2
i <- 1
while (i in nrow(df)){
  if (df[i, 'UNIDADES'] > 2000000){
    print(paste('Se han superado 2 millones'))
  }
  i <- i + 1
}

# Opcion 3
Ambas opciones son correctas

# Opcion 4
Ambas opciones son incorrectas






df <- read.csv('data_PremiumPizza.csv')
df$UNIDADES[is.na(df$UNIDADES)] <- median(df$UNIDADES, na.rm = T)

# Opcion 1
for (i in seq(1,nrow(df))){
  if (df[i, 'UNIDADES'] > 2000000){
    print(paste('Se han superado 2 millones'))
  }
}

# Opcion 2
i <- 1
while (i in nrow(df)){
  if (df[i, 'UNIDADES'] > 2000000){
    print(paste('Se han superado 2 millones'))
    break()
  }
  i <- i + 1
}

# Opcion 3
Ambas opciones son correctas

# Opcion 4
Ambas opciones son incorrectas











matriz_numerica <- matrix( c(1,2,3,4,5,10,12,14,16,18), nrow = 5, ncol = 2)

# Opcion 1
apply(x = matriz_numerica, MARGIN = 1, FUN = mean)

# Opcion 2
apply(x = matriz_numerica, MARGIN = 2, FUN = mean)

# Opcion 3
apply(x = matriz_numerica, FUN = mean)

# Opcion 4
lapply(x = matriz_numerica, MARGIN = 1, FUN = mean)





vector_ventas <- c(5637, 7364, 2353, 2567, 4646, 7344)

vector_meses <- c('Enero', 'Febrero', 'Marzo' ,'Abril' ,'Mayo' ,'Junio')

for (i in seq(l, length(vector_ventas))){
  if ( vector_ventas[i]>5000){
    print(paste0(vector_meses[i], ";", ventor_ventas, " | "))
  }
}





saveRDS(variable1, variable2, variable3, variable4, file='var_ses.rds')

save(variable1, variable2, variable3, variable4, file='var_ses.rds')

save.image('var_ses.RData')

saveRDS(variable1, file='var_ses.rds')