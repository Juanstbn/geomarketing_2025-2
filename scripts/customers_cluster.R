#LIBRERIAs
library(factoextra)
library(ggfortify)


#ENTRADAS
#proviene de https://www.kaggle.com/datasets/vjchoudhary7/customer-segmentation-tutorial-in-python?resource=download
mall= read.csv("data/Mall_Customers.csv")

#RENOMBRAR COLUMNAS
colnames(mall)[c(4,5)]=c("Income","Score")

#EXTRAEMOS LAS VARIABLES DE INTERES
mall_data=mall[,c("Age","Income","Score")]

#SE ESCALAN LAS VARIABLES PARA EVITAR Q ALGUNA DOMINE POR SU MAGNITUD
mall_scaled= scale(mall_data)

#VISUALIZAR LA SUMA DE CUADRADOS DENTRO DEL CLUSTER (WSS) PARA DISTINTOS NIVELES DE K
fviz_nbclust(mall_scaled, kmeans,method="wss") +
  labs(title="Método del codo", x="Número de clusters(k)", y="WSS")

#EL METODO DEL CODO SUGIERE QUE K = 4

#SE EJECUTA K-MEANS
set.seed(123) #para reproducibilidad

km= kmeans(mall_scaled,centers=4, nstart=25)

#AÑADIR LOS CLUSTERS AL DATAFRAME ORIGINAL
mall$cluster=as.factor(km$cluster)

#GRAFICA DE DISPERSION

##INGRESO VS SCORE
ggplot(mall,aes(x=Income, y=Score, color=cluster)) +
  geom_point(size=2)+
  labs(x="Ingreso anual",
       y="Nivel de gastos")+
  theme_minimal()
#cluster 1: Alto ingreso, Alto Gasto
#cluster 2: Bajo ingreso, alto gasto
#cluster 3: bajo ingreso, bajo gasto
#cluster 4: alto ingreso, bajo gasto

##EDAD CS SCORE
ggplot(mall,aes(x=Age, y=Score, color=cluster)) +
  geom_point(size=2)+
  labs(x="Edad",
       y="Nivel de gastos")+
  theme_minimal()

#cluster 1: Adultos jovenes
#cluster 2: Adolescentes- jovenes
#cluster 3: Adultos - Adultos mayores
#cluster 4: Adultos

