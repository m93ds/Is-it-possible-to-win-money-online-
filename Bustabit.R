###############################################################################

#Metodos de Analisis Multivariado 

#Alumno: Matias A. Salgado Mancilla

#BUSTABIT


#Limpio Rstudio
rm(list =ls())

#Instalo librerias
install.packages("readr")
install.packages("dplyr")
install.packages("stats")
install.packages("ggplot2")
install.packages("factoextra")
install.packages("cluster")

#Cargo librerias
library(readr) #Leer base de datos
library(dplyr) #Manipular datos y usar Pipes/ Tuberias
library(stats) #Para PCA y MANOVA
library(ggplot2) #Graficos
library(factoextra) #Extraer informacion y visualizar datos multivariados
library(cluster) #Tratamiento de datos relacionados a clusters

#Carga y manipulacion de datos
bustabit_orig <- read_csv("~/R/Analisis Multivariado/Trabajo final/bustabit.csv", 
                     na = "NA")

bustabit <- bustabit_orig[1:1000,c(3:8)]
  
View(bustabit)
summary(bustabit)

#Tratamiento de NA
sum(is.na(bustabit))   # Cantidad de valores perdidos

bustabit[is.na(bustabit)] <- 0

#Renombro variables
bustabit <- rename(bustabit, Usuario = Username, Apuesta = Bet,
                   SalidaSg = CashedOut, Bonificacion = Bonus,
                   Beneficio = Profit, Multiplicador = BustedAt)


#Tipifico bustabit solo para graficarlo
bustabit_tip <- as.data.frame(scale(bustabit[,2:6],center = T, scale = T))

boxplot(bustabit_tip[,1], col = "red") #Apuesta
boxplot(bustabit_tip[,2], col = "blue") #SalidaSg
boxplot(bustabit_tip[,3], col = "green") #Bonificacion
boxplot(bustabit_tip[,4], col = "yellow") #Beneficio
boxplot(bustabit_tip[,5], col = "purple") #Multiplicador

#Creo variables de Juegos ganados(GameWon) y juegos perdidos(GameLost)
bustabit_variables <- bustabit %>% 
  mutate(SalidaSg = ifelse(is.na(SalidaSg), Multiplicador + .01, SalidaSg),
         Beneficio = ifelse(is.na(Beneficio), 0, Beneficio),
         Perdidas = ifelse(Beneficio == 0, - Apuesta, 0),
         Gana = ifelse(Beneficio == 0, 0, 1),
         Pierde = ifelse(Beneficio == 0, 1, 0))

#Creo variables con caracteristicas conductuales y economicas
bustabit_cluster <- as.data.frame( bustabit_variables %>%
  group_by(Usuario) %>%
  summarize(PromSalidaSg = mean(SalidaSg), 
            PromApuesta = mean(Apuesta),
            TotalBeneficio = sum(Beneficio),
            TotalPerdidas = sum(Perdidas), 
            JuegosGanados = sum(Gana),
            JuegosPerdidos = sum(Pierde)))

summary(bustabit_cluster)

#Centrar datos o tipificarlos, tip=tipificado
datos_tip <- as.data.frame(scale(bustabit_cluster[,2:7],center = T, scale = T))
summary(datos_tip)

#Visualizo nuevas variables creadas
boxplot(datos_tip[,1], col = "red") #PromSalidaSg
boxplot(datos_tip[,2], col = "blue") #PromApuesta
boxplot(datos_tip[,3], col = "green") #TotalBeneficio
boxplot(datos_tip[,4], col = "yellow") #TotalPerdidas
boxplot(datos_tip[,5], col = "purple") #JuegosGanados
boxplot(datos_tip[,5], col = "orange") #JuegosPerdidos


#Suma de cuadrados
suma2 <- 0

#Aplico For desde 1 a 10 centroides
for (i in 1:10) {
  kmedias <- kmeans(datos_tip, centers = i, nstart = 20)
  # Guardo el total en suma de cuadrados en la variable suma2
  suma2[i] <- kmedias$tot.withinss
}

#Represento visualmente la cantidad optima de clusters
plot(1:10, suma2, type = "b", 
     xlab = "Número de Clusters", 
     ylab = "Suma de Cuadrados", col = "purple3",
     main = "Cantidad Óptima de Clusters", cex.main = 1.5)

#Cantidad optima de clusters = 5
abline(v = 5, lwd=1.8, col='green4')

#Hago agrupamiento kmeans 
k <- 5
kmedias <- kmeans(datos_tip,centers = 5, nstart = 20)

#Calcular autovalores y autovectores de la matriz de correlaciones
S = cor(datos_tip)
eigen(S)

#Calcular los componentes principales basados en la matriz de correlaciones
datos_pc <- princomp(datos_tip,cor=TRUE)

summary(datos_pc,loadings=TRUE)

plot(datos_pc) #Componente 1 y 2 explican mas del 58% de los datos

#Asignacion de componentes principales
comp_princ <- as.data.frame(prcomp(datos_tip)$x)

# Agrego variable cluster a data set de componentes principales
comp_princ$cluster <- factor(kmedias$cluster)

# Visualizo datos segun componentes principales
ggplot(comp_princ, aes(x = PC1, y = PC2)) + geom_point()

#Visualizo cluster kmeans de acuerdo a componentes principales
fviz_cluster(kmedias, datos_tip, main = "Clusters")

#Evaluo suma de cuadrados intragrupos e intergrupos
kmedias$withinss
kmedias$betweenss

#Visualizo en la consola los clusters con sus casos correspondientes
for(i in 1:5) {
  print(i)
  print(which(kmedias$cluster==i))
}


#Preparo variable para analizar promedios de clusters
bustabit_agrupamiento <- as.data.frame(cbind(bustabit_cluster[,2:7],
                                             Cluster = kmedias$cluster))

#Analizo tabla con valores de los diferentes clusters
bustabit_promedios <- bustabit_agrupamiento %>%
  group_by(Cluster) %>%
  summarize_if(is.numeric,mean)

View(bustabit_promedios)


###############################################################################