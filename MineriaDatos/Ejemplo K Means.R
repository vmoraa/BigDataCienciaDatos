# Instalamos los paquetes necesarios, en caso que no los tengamos instalados
install.packages("cluster")
install.packages("factoextra")
install.packages("tidyverse")

# Cargamos las librerias que utilizaremos
library(cluster)
library(factoextra)
library(tidyverse)

# Indicamos el directorio de trabajo
setwd("C:/Users/Sebastian/Documents/R")

# Cargamos la base de datos
datos <- read.csv("faithful.csv", sep=";",header=TRUE)

# Normalizamos los datos
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

datos_norm <- as.data.frame(lapply(datos, normalize))

# K Means con K = 3
k3 <- kmeans(datos_norm,
             centers = 3,
             nstart = 25)

# Analicemos los resultados

# Tamano de cada cluster
k3$size
# Cluster al que pertenece cada observacion
k3$cluster
# Centros de cada cluster
k3$centers
# Suma cuadratica inicial de distancias
k3$totss
# Sumas cuadraticas de distancias dentro de cada cluster
k3$withinss
k3$tot.withinss


# Apliquemos el codo
clusters <- c(1,2,3,4,5,6,7,8,9,10)
codo <- c(0,0,0,0,0,0,0,0,0,0)
for (k in 1:10) { 
  codo[k] <- kmeans(datos_norm,
                    centers = k,
                    nstart = 25)$tot.withinss
}

plot(clusters, codo, type = "l")


# Calculemos los valores de silueta para nuestros clusters
silueta_k3 <- silhouette(k3$cluster, dist(datos_norm))
summary(silueta_k3)

# La primera columna es el cluster de cada observacion
silueta_k3[,1]
# La segunda columna es el cluster vecino de cada observacion
silueta_k3[,2]
# La tercera columna es el valor de silueta de cada observacion
silueta_k3[,3]

# Grafiquemos la silueta
silueta <- c(0,0,0,0,0,0,0,0,0)
for (k in 2:10) { 
  modelo_aux <- kmeans(datos_norm,
                       centers = k,
                       nstart = 25)
  silueta_aux <- silhouette(modelo_aux$cluster, dist(datos_norm))
  silueta[k] <- mean(silueta_aux[, 3])
}

plot(clusters, silueta, type = "l")

# Distribuciones de valores de silueta
fviz_silhouette(silueta_k3)


# Interpretemos que es cada cluster

# Agreguemos el cluster de cada observacion a los datos
datos$cluster3 <- k3$cluster

# Podemos analizar visualmente los clusteres
plot(datos$eruptions, datos$waiting, col = datos$cluster3)

# Podemos analizar las variables promedio de cada cluster
View(datos
     %>% group_by(cluster3)
     %>% summarise_all(mean))
