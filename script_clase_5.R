# Set-up ------------------------------------------------------------------

rm(list=ls())

library(class)
library(ggplot2)
library(caret)
library(dplyr)
library(tidyr)

setwd("C:/Users/Felipe/OneDrive/Universidad/Semestre 2018-Vacacional/Big Data/Clases/Complementarias/Matrices de confusión")

alcaldes<-read.csv("base_gastos.csv")

# revisión de la base -----------------------------------------------------

str(alcaldes)
table(alcaldes$partido)

# inspección gráfica ------------------------------------------------------
windows()

par(mfrow=c(2,3))
for (i in 1:5){
  hist(log(alcaldes[,i]), freq = F)
}

alcaldes %>%
  gather(key = "var", value = "valor", -(6:7)) %>%
  ggplot(aes(y=valor, x=partido))+
  geom_boxplot() +
  scale_y_log10() +
  coord_flip()+
  facet_wrap(~ var)

alcaldes %>% 
  mutate_at(vars(1:5), funs(as.numeric(.>=mean(., na.rm=T)))) %>%
  gather(key = "var", value = "valor", -(6:7)) %>%
  ggplot(aes(fill=factor(valor), x=partido))+
  geom_bar(position = "fill") +
  facet_wrap(~var) +
  coord_flip()
  
alcaldes<-alcaldes[,-3]

cor(alcaldes[1:4])

de.off()

# knn ---------------------------------------------------------------------

# Partición de los datos
set.seed(2018)
index<-createDataPartition(alcaldes$partido, times = 1, p = 0.8, list = F)

alcaldes_train<-alcaldes[index,]
alcaldes_test<-alcaldes[-index,]

# Preprocesamiento de los datos
preproceso<-preProcess(alcaldes_train[,1:4],method = c("center", "scale"))

alcaldes_train[,1:4]<-predict(preproceso, newdata=alcaldes_train[,1:4])
alcaldes_test[,1:4]<-predict(preproceso, newdata=alcaldes_test[,1:4])

resultados<-NULL
for (i in 70:150){
  resultados<-bind_rows(resultados,
                        data.frame(k=i,
                                   accuracy=confusionMatrix(knn(train = alcaldes_train[,-(5:6)],
                                                                test = alcaldes_test[,-(5:6)], 
                                                                cl = alcaldes_train[,5],
                                                                k = i), alcaldes_test[,5])$overall["Accuracy"]))
}

windows()
qplot(data = resultados, x = k, y=accuracy, geom="line")
resultados[which.max(resultados$accuracy),]
## otra aproximación

alcaldes2<-alcaldes %>% 
  mutate_at(vars(3), funs(as.numeric(.>=mean(., na.rm=T))))

alcaldes2_train<-alcaldes2[index,]
alcaldes2_test<-alcaldes2[-index,]

# Preprocesamiento de los datos

resultados2<-NULL
for (i in 70:150){
  resultados2<-bind_rows(resultados2,
                        data.frame(k=i,
                                   accuracy=confusionMatrix(knn(train = alcaldes2_train[,-(5:6)],
                                                                test = alcaldes2_test[,-(5:6)], 
                                                                cl = alcaldes2_train[,5],
                                                                k = i), alcaldes2_test[,5])$overall["Accuracy"]))
}

windows()
qplot(data = resultados2, x = k, y=accuracy, geom="line")
resultados2[which.max(resultados2$accuracy),]

# ¿qué tan bueno es? ------------------------------------------------------

round(prop.table(table(alcaldes_test$partido))*100,2)



