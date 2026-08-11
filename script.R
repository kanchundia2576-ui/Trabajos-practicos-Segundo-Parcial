# ==============================================================================
# 1. SIMULACIÓN Y K-MEANS
# ==============================================================================
# Simulación
x = cbind(rnorm(100, 1000, 100), c(rnorm(50), rnorm(50, 10, 1)))
plot(x, pch = 16)

# k-means datos originales
res = kmeans(x, 2)
plot(x, col = c("green", "red")[res$cluster], pch = 16)

# Estandarización
xs = scale(x)
plot(xs, pch = 16)

# k-means datos estandarizados
res = kmeans(xs, 2)
plot(x, col = c("green", "red")[res$cluster], pch = 16)


# ==============================================================================
# 2. CARGA Y PREPARACIÓN DE DATOS (DISTRITOS)
# ==============================================================================
library(foreign)
distritos = read.spss("distritos.sav", use.value.labels = TRUE, max.value.labels = Inf, to.data.frame = TRUE)
colnames(distritos) <- tolower(colnames(distritos))
nombres = distritos[, 1]
distritos = distritos[, -1]
rownames(distritos) = nombres
head(distritos)

# Ejecución básica de k-means
kmeans(scale(distritos), 2)


# ==============================================================================
# 3. ANÁLISIS DE CLUSTERS CON IRIS (MÉTODO DEL CODO)
# ==============================================================================
data(iris)
wss <- numeric()
for(h in 2:10) {
  b <- kmeans(iris[, 1:4], h)
  wss[h - 1] <- b$tot.withinss
}
plot(2:10, wss, type = "b")


# ==============================================================================
# 4. EVALUACIÓN DE CLUSTERS (SILHOUETTE Y CALINSKI-HARABASZ)
# ==============================================================================
library(cluster)
diss.distritos = daisy(scale(distritos))
par(mfrow = c(1, 3))
for(h in 2:4) {
  res = kmeans(scale(distritos), h)
  plot(silhouette(res$cluster, diss.distritos))
}

library(fpc)
ch <- numeric()
for(h in 2:10) {
  res <- kmeans(scale(distritos), h)
  ch[h - 1] <- calinhara(scale(distritos), res$cluster)
}
plot(2:10, ch, type = "b", xlab = "k", ylab = "Criterio de Calinski-Harabasz")

# Criterios automáticos de selección de clusters
kmeansruns(scale(distritos), criterion = "ch")
kmeansruns(scale(distritos), criterion = "asw")

# Visualización de clusters
res = kmeans(scale(distritos), 2)
plotcluster(distritos, res$cluster)


# ==============================================================================
# 5. CLUSTERING JERÁRQUICO (HCLUST Y AGNES)
# ==============================================================================
# Clustering jerárquico básico
a = hclust(dist(iris[, 1:4]))
plot(a)

# Método de Ward y corte de árbol
dis = dist(iris[, 1:4])
a = hclust(dis, method = "ward")
b = cutree(a, k = 3)
plot(a)
table(b, iris[, 5])

# Uso de AGNES en Iris
b <- agnes(iris[, 1:4], metric = "euclidean", method = "ward")
b = cutree(b, k = 3)
table(b, iris[, 5])

# Uso de AGNES en Distritos
res = agnes(scale(distritos), method = "average")
