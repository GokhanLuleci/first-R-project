install.packages("dplyr")  #Paket Kurulumu ve Çagirilmasi
library(dplyr) 

install.packages("caret")  #Paket Kurulumu ve Çagirilmasi
library(caret)

getwd() #Veri Içe Aktarim
titanic_veri <- read.csv("C:/Users/Gokhan Luleci/Documents/Titanic.csv", na.strings = NA)

str(titanic_veri) #Veri Inceleme
head(titanic_veri)
summary(titanic_veri)

colSums(is.na(titanic_veri)) #NA Tespit Etme
head(is.na.data.frame(titanic_veri))

final_titanic_veri <- titanic_veri %>%  #Veri Manipülasyonu
  mutate(age = ifelse(is.na(age), mean(na.omit(age)), age)) %>%
  mutate(family = sibsp + parch) %>%
  mutate(survived = as.factor(ifelse(survived == 0, "No", "Yes"))) %>%
  select(
    sinif = pclass,
    cinsiyet = sex,
    yas = age,
    aile = family,
    kurtulma = survived) %>%
  as.data.frame()

head(final_titanic_veri) #Veri Inceleme

str(final_titanic_veri) #Veri Inceleme
summary(final_titanic_veri)

colSums(is.na(final_titanic_veri)) #NA Tespit Etme
head(is.na.data.frame(final_titanic_veri))

set.seed(42) #Egitim ve Sinama Verileri Olusturma
dataIndex <- createDataPartition(final_titanic_veri$kurtulma, p = 0.8 , list = F) 
egitim_verisi <- final_titanic_veri[dataIndex, ]
sinama_verisi <- final_titanic_veri[-dataIndex, ]

#Model Kurulumu
?train
pp <- c("center", "scale") 
tc <- trainControl(method = "cv", number = 10 )

model_titanic <-  train( kurtulma~. , data = egitim_verisi , method = 'knn',
                        preProcess = pp, trControl = tc, tuneLength = 5)
print(model_titanic)
#Farkli Model Kurulumu
model_titanic_pp <-  train( kurtulma~. , data = egitim_verisi , method = 'knn',
                         preProcess = pp, tuneLength = 5)

print(model_titanic_pp)
#Farkli Model Kurulumu
model_titanic_tc <-  train( kurtulma~. , data = egitim_verisi , method = 'knn',
                          trControl = tc, tuneLength = 5)

print(model_titanic_tc)
#Tahminleme Yapma ve Confussion Matrix Yorumlama
tahminler <- predict.train(object = model_titanic, sinama_verisi[, 1:4], type = "raw")

confusionMatrix(tahminler,sinama_verisi[,5])
#Karar Agaci
install.packages("rpart")  #Paket Kurulumu ve Çagirilmasi
install.packages("rpart.plot")
library(rpart) 
library(rpart.plot) 
?rpart
set.seed(42)
#Model Kurulumu
model_titanic_rp <- train( kurtulma~. , data = egitim_verisi , method = 'rpart',
                           preProcess = pp, trControl = tc, tuneLength = 5)

print(model_titanic_rp)
#Tahminleme Yapma ve Confussion Matrix Yorumlama
tahminler_rp <- predict(model_titanic_rp, sinama_verisi, type = 'raw')

confusionMatrix(data=tahminler_rp, reference = sinama_verisi$kurtulma)

#Bonus Karar Agaci Gorsellestirme
fit <- rpart(kurtulma ~.,
             data = egitim_verisi,
             method = 'class',)

pred_fit <- predict(fit, sinama_verisi, type = 'class')
confusionMatrix(data=pred_fit, reference = sinama_verisi$kurtulma)
rpart.plot(fit, extra = 106)