# Instalar paquetes necesarios
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")

# Cargar librerías necesarias
library(tidyverse)
library(cluster)
library(factoextra)
library(stats)

# Cargar datos
datos <- read.csv("Cust_Segmentation.csv")

# Preparación de datos
# 1. Eliminar valores NA
datos_clean <- na.omit(datos)

# 2. Seleccionar solo las variables numéricas relevantes para la segmentación
# Excluimos Customer Id y Address que no son relevantes para el clustering
variables_clustering <- c("Age", "Edu", "Years.Employed", "Income", 
                          "Card.Debt", "Other.Debt", "DebtIncomeRatio")
datos_numericos <- datos_clean[, variables_clustering]

# 3. Normalizar los datos
datos_norm <- scale(datos_numericos)

# 4. Experimentar con diferentes parámetros
distancias <- c("euclidean", "manhattan")
metodos <- c("ward.D2", "complete", "average")
k_valores <- 2:5  # Probamos entre 2 y 5 clusters

# Función para calcular silhouette score
calcular_silhouette <- function(dist_metodo, link_metodo, k, datos) {
  dist_matrix <- dist(datos, method = dist_metodo)
  hc <- hclust(dist_matrix, method = link_metodo)
  clusters <- cutree(hc, k = k)
  sil <- silhouette(clusters, dist_matrix)
  mean(sil[,3])
}

# Crear grid de parámetros y calcular scores
resultados <- expand.grid(
  distancia = distancias,
  metodo = metodos,
  k = k_valores,
  silhouette = NA
)

# Calcular silhouette score para cada combinación
for(i in 1:nrow(resultados)) {
  resultados$silhouette[i] <- calcular_silhouette(
    resultados$distancia[i],
    resultados$metodo[i],
    resultados$k[i],
    datos_norm
  )
}

# Mostrar los mejores resultados
print("Mejores combinaciones de parámetros:")
print(resultados[order(-resultados$silhouette), ])

# Aplicar el mejor modelo
mejor_modelo <- resultados[which.max(resultados$silhouette),]
dist_final <- dist(datos_norm, method = mejor_modelo$distancia)
hc_final <- hclust(dist_final, method = mejor_modelo$metodo)
clusters_final <- cutree(hc_final, k = mejor_modelo$k)

# Añadir clusters al dataset original
datos_clean$Cluster <- clusters_final

# Calcular perfiles por cluster
perfiles <- datos_clean %>%
  group_by(Cluster) %>%
  summarise(
    n = n(),
    edad_media = mean(Age),
    educacion_media = mean(Edu),
    años_empleado_media = mean(Years.Employed),
    ingreso_medio = mean(Income),
    deuda_tarjeta_media = mean(Card.Debt),
    otra_deuda_media = mean(Other.Debt),
    ratio_deuda_ingreso_medio = mean(DebtIncomeRatio),
    tasa_default = mean(Defaulted, na.rm = TRUE)
)

# Visualizaciones
# 1. Dendrograma
png("dendrograma.png")
plot(hc_final, main = "Dendrograma del Clustering Jerárquico",
     xlab = "Clientes", ylab = "Altura")
dev.off()

# 2. Visualización de clusters en 2D
png("clusters_2d.png")
fviz_cluster(list(data = datos_norm, cluster = clusters_final),
             main = "Visualización de Clusters",
             ellipse.type = "convex")
dev.off()

# Mostrar perfiles de clusters
print("Perfiles de los clusters:")
print(perfiles)
