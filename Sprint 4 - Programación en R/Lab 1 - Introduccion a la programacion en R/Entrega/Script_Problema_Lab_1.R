### Introduccion ----------------------

#En el siguiente problema vamos a plantear un conjunto de tratamientos y transformaciones
#que tendreis que realizar de forma autonoma.

#Haciendo uso de los comentarios os indicare cual es la accion a realizar y tendreis que generar el codigo 
#que la lleve a cabo

#Consultar los vídeos del lab y los fastbooks os ayudara

#En caso de que surja cualquier duda o cuestion podeis contactar conmigo


### Enunciado ------------------------

#Supongamos que realizamos un test A/B, en el que comparamos la eficacia de dos anuncios en formato online.
#El primero de ellos es el clasico, el que ha sido usado por la empresa desde sus inicios. 
#El segundo de ellos es una version mejorada que se ha creado recientemente.

#Para llevar a cabo dicha comparativa tenemos los clicks diarios que han generado cada uno de los anuncios
#Todo ello en un intervalo temporal de catorce dias. Desde el 30 de noviembre hasta el 13 de diciembre de 2020

#A continuacion contruyo la informacion del experimento (son 3 vectores). Ejecutad sin hacer modificaciones
anuncio <- rep(c("anuncio antiguo","anuncio nuevo"),times=1,each=14)
clicks <- c(1924,1820,1724,2042,NA,2643,2801,1930,1837,1930,2190,2231,2790,2678,
            2345,1789,1980,2456,2556,2534,2902,2754,2490,2612,1923,2432,2942,2493)
fechas <- as.character(rep(seq(as.Date("2020-11-30"),as.Date("2020-12-13"),by="day"),2))

#Printeamos las variables para poder ver y entender su contenido
anuncio
clicks
fechas


### Ejercicios  ---------------------

#Ejercicio 1: Usa sobre cada uno de los vectores la funcion que devuelve sus longitudes
#(el numero de elementos que poseen)

# Longitud del vector 'anuncio'
length_anuncio <- length(anuncio)
print(length_anuncio)

# Longitud del vector 'clicks'
length_clicks <- length(clicks)
print(length_clicks)

# Longitud del vector 'fechas'
length_fechas <- length(fechas)
print(length_fechas)

# --------------------

#Ejercicio 2: El vector de clicks contiene un NA. Modifica ese dato para que pase a valer
#la mediana del vector clicks

# Calcular la mediana del vector 'clicks', ignorando el NA
mediana_clicks <- median(clicks, na.rm = TRUE)

# Reemplazar el NA con la mediana
clicks[is.na(clicks)] <- mediana_clicks

# Verificar el resultado
print(clicks)

# --------------------

#Ejercicio 3: Modifica aquellos valores de clicks iguales a 1924 y 1820 para que pasen a valer
#1926 y 1822 respectivamente_

# Modificar los valores de clicks
clicks[clicks == 1924] <- 1926
clicks[clicks == 1820] <- 1822

# Verificar el resultado
print(clicks)

# --------------------

#Ejercicio 4: Aplica una funcion sobre el vector anuncio para que puedas saber cuantos valores distintos 
#contiene y cuantas veces aparece cada uno de ellos

# Aplicar la función table() al vector 'anuncio'
conteo_anuncio <- table(anuncio)

# Mostrar el resultado
print(conteo_anuncio)

# --------------------

#Ejercicio 5: Modifica el vector anuncio para que el termino "antiguo" pase a ser "clasico" y 
#el termino "nuevo" pase a ser "moderno"

# Modificar los valores de 'anuncio'
anuncio <- gsub("antiguo", "clasico", anuncio)
anuncio <- gsub("nuevo", "moderno", anuncio)

# Verificar el resultado
print(anuncio)

# --------------------

#Ejercicio 6: Aplica una funcion sobre anuncio para que su contenido pase a estar en mayusculas

# Convertir el contenido del vector 'anuncio' a mayúsculas
anuncio <- toupper(anuncio)

# Verificar el resultado
print(anuncio)

# --------------------

#Ejercicio 7: Convierte el vector anuncio a tipo factor

# Convertir el vector 'anuncio' a tipo factor
anuncio <- as.factor(anuncio)

# Verificar el resultado
print(anuncio)
print(class(anuncio))  # Para comprobar que es de tipo factor

# --------------------

#Ejercicio 8: Convierte el vector fechas a tipo Date y comprueba que ha surtido efecto con la funcion class

# Convertir el vector 'fechas' a tipo Date
fechas <- as.Date(fechas)

# Verificar que la conversión ha surtido efecto
print(fechas)
print(class(fechas))  # Para comprobar que es de tipo Date

# --------------------

#Ejercicio 9: Calcula la fecha minima y maxima del vector fechas

# Calcular la fecha mínima del vector 'fechas'
fecha_minima <- min(fechas)

# Calcular la fecha máxima del vector 'fechas'
fecha_maxima <- max(fechas)

# Mostrar los resultados
print(fecha_minima)
print(fecha_maxima)

# --------------------

#Ejercicio 10: Construye una matriz con 14 filas y 2 columnas a partir del vector clicks.
#Guardala con el nombre matriz

# Construir la matriz con 14 filas y 2 columnas a partir del vector 'clicks'
matriz <- matrix(clicks, nrow = 14, ncol = 2, byrow = TRUE)

# Mostrar la matriz
print(matriz)

# --------------------

#Ejercicio 11: Construye un dataframe llamado df con 3 columnas. 
#La primera se debe llamar fecha y debe contener el vector fechas 
#La segunda se debe llamar anuncio y debe contener el vector anuncio
#La tercera se debe llamar clicks y debe contener el vector clicks

# Construir el dataframe con las columnas especificadas
df <- data.frame(
  fecha = fechas,
  anuncio = anuncio,
  clicks = clicks
)

# Mostrar el dataframe
print(df)

# --------------------

#Ejercicio 12: Aplica sobre df$clicks la funcion que da informacion sobre la distribucion numerica de la variable

# Resumen estadístico básico de 'df$clicks'
summary(df$clicks)

# Distribución de frecuencias de 'df$clicks'
table(df$clicks)

# Histograma de 'df$clicks' para visualizar la distribución
hist(df$clicks, main="Histograma de Clicks", xlab="Número de Clicks", ylab="Frecuencia", col="lightblue", border="black")

# --------------------

#Ejercicio 13: Calcula la suma de todos los clicks

# Calcular la suma de todos los valores en 'df$clicks'
suma_clicks <- sum(df$clicks, na.rm = TRUE)

# Mostrar el resultado
print(suma_clicks)

# --------------------

#Ejercicio 14: Calcula la suma de todos los clicks para cada uno de los anuncios.
#Ademas de sum tendras que hacer uso de la funcion subset.

# Sumar los clicks para el anuncio "clasico"
suma_clasico <- sum(subset(df, anuncio == "CLASICO")$clicks, na.rm = TRUE)

# Sumar los clicks para el anuncio "moderno"
suma_moderno <- sum(subset(df, anuncio == "MODERNO")$clicks, na.rm = TRUE)

# Mostrar los resultados
cat("Suma de clicks para el anuncio CLASICO:", suma_clasico, "\n")
cat("Suma de clicks para el anuncio MODERNO:", suma_moderno, "\n")

# --------------------

#Ejercicio 15: Asumiendo que por cada click generado nuestra empresa recibe 10 euros.
#Crea una columna dentro de df llamada ganancia que multiplique el numero de clicks por 10

# Crear la columna 'ganancia' en el dataframe df
df$ganancia <- df$clicks * 10

# Mostrar el dataframe actualizado
print(df)

# --------------------

#Ejercicio 16: Importa el paquete lubridate y crea en df una columna llamada semana a partir de redondear
#hacia abajo la columna fecha. Considera un inicio semanal de lunes.

# Instalar el paquete lubridate si no está instalado
if (!require(lubridate)) {
  install.packages("lubridate")
}

# Cargar el paquete lubridate
library(lubridate)

# Crear la columna 'semana' en el dataframe df
df$semana <- floor_date(df$fecha, unit = "week", week_start = 1)

# Mostrar el dataframe actualizado
print(df)

# --------------------

#Ejercicio 17: Ordena el df en base al numero de clicks

# Ordenar el dataframe df en base al número de clicks (de menor a mayor)
df <- df[order(df$clicks), ]

# Mostrar el dataframe ordenado
print(df)

# --------------------

#Ejercicio 18: Crea una columna dentro de df llamada nombre que contega tu nombre en todas las filas.
#Para ello tienes que usar la funcion que te ayuda a repetir un determinado valor varias veces.

# Crear la columna 'nombre' en el dataframe df con tu nombre repetido
df$nombre <- rep("Hugo", nrow(df))

# Mostrar el dataframe actualizado
print(df)

# --------------------

#Ejercicio 19: Parte en trozos la columna anuncio usando el espacio vacio (" ") como separador. 
#Antes tendras que aplicar la funcion as.character sobre dicha variable para que deje de ser factor
#El resultado te devolvera una lista. Guardala en una variable llamada lista_split

# Convertir la columna 'anuncio' a tipo character si aún es factor
df$anuncio <- as.character(df$anuncio)

# Dividir la columna 'anuncio' en trozos usando el espacio vacío como separador
lista_split <- strsplit(df$anuncio, split = " ")

# Mostrar el resultado
print(lista_split)

# --------------------

#Ejercicio 20: Para finalizar crea una lista llamada lista_final que contenga cuatro elementos
#En primer lugar, una variable basica tipo character con tu nombre
#En segunda lugar, la matriz que hemos creado en el ejercicio 10
#En tercer lugar, el dataframe que hemos llamado df
#Por ultimo, la lista_split que acabamos de crear el el ejericio 19

# Crear la variable básica tipo character con tu nombre
nombre_personal <- "Hugo"

# Crear la lista final con los cuatro elementos especificados
lista_final <- list(
  nombre = nombre_personal,    # Primer elemento: variable character con tu nombre
  matriz = matriz,             # Segundo elemento: la matriz creada en el ejercicio 10
  dataframe = df,              # Tercer elemento: el dataframe df
  lista_split = lista_split    # Cuarto elemento: la lista creada en el ejercicio 19
)

# Mostrar la lista final
print(lista_final)
